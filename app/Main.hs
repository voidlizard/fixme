{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# Language PatternSynonyms #-}
{-# Language TemplateHaskell #-}
module Main where

import Fixme.Defaults
import Fixme.OrDie
import Fixme.Git
import Fixme.Hash
import Fixme.State
import Fixme.Types
import Fixme.Prelude
import Fixme.RunListAttribs
import Fixme.RunReport
import Fixme.Config
import Fixme.LocalConfig

import Data.Config.Suckless

import Codec.Serialise
import Control.Concurrent.Async
import Control.Monad
import Data.Set qualified as Set
import Data.Attoparsec.Text hiding (option,take)
import Data.Attoparsec.Text qualified as Atto
import Data.ByteString.Lazy.Char8 qualified as LBS
import Data.Foldable (for_)
import Data.Function
import Data.Generics.Uniplate.Data()
import Data.IntMap qualified as IntMap
import Data.List qualified as List
import Data.Maybe
import Data.String
import Data.Text.Encoding
import Data.Text.Encoding.Error (ignore)
import Data.Text.IO qualified as Text
import Data.Text qualified as Text
import Data.Text (Text)
import Data.UUID.V4 as UUID
import Lens.Micro.Platform
import Options.Applicative hiding (Parser)
import Options.Applicative qualified as O
import Prettyprinter
import Safe
import System.Directory
import System.Exit
import System.FilePattern
import System.IO
import System.Process.Typed
import Text.InterpolatedString.Perl6 (qc)
import Data.HashMap.Strict qualified as HashMap


newtype ScanOpt =
  ScanOpt
  { _scanDontAdd :: Bool
  }

makeLenses 'ScanOpt

data ListOpts =
  ListOpt
  { _listShowFull  :: Bool
  , _listFilters   :: [Text]
  , _listFiltExact :: Bool
  }

makeLenses 'ListOpt

runInit  :: IO ()
runInit = do

  print $ pretty "init" <+> pretty confDir

  createDirectoryIfMissing True confDir
  confExists <- doesFileExist confFile
  logExists <- doesFileExist logFile

  unless confExists do
    Text.writeFile confFile defConfig

  unless logExists do
    Text.writeFile logFile defLog
    env <- newFixmeEnv

    runFixmeState env do
      liftIO $ print $ pretty "init db"
      initState


runUuid :: IO ()
runUuid = do
  uuid <- UUID.nextRandom
  print $ pretty "uuid:" <+> pretty (show uuid)


runLog :: Bool -> String -> IO ()
runLog dry s = withState do
  print (pretty s)
  unless dry do
    appendFile logFile "\n"
    appendFile logFile s

runList :: ListOpts -> IO ()
runList opt = do
  e <- newFixmeEnv

  cfgFile <- readFile confFile

  r <- pure (parseTop cfgFile) `orDie` "can't parse config"

  filt <- if view listFiltExact  opt then do
            let ss = opt ^. listFilters
            let flt = parseFilt ss
            pure $ Filt @IO flt
          else
            pure $ Filt $ opt ^. listFilters

  fxms <- runFixmeState e (loadFixme filt)

  let tl = maximumDef 6 $ fmap Text.length (pref r)

  let fmt = if view listShowFull opt then Full o else Brief o
        where
          o = FmtAttr (fromIntegral (idlen r) ) tl (tpref r) mbPre mbSuff
          mbPre   | view listShowFull opt = rpref r
                  | otherwise = ""

          mbSuff  | view listShowFull opt = suff r
                  | otherwise = ""

  for_ fxms $ \fxm -> do
    print $ pretty (fmt fxm)

runUpdate :: ScanOpt -> IO ()
runUpdate opt = do

  cfgFile <- readFile confFile
  logFileData <- readFile logFile

  -- FIXME: better error handling
  r <- pure (parseTop cfgFile) `orDie` "can't parse config"

  log <- pure (parseTop logFileData) `orDie` "can't parse log"

  e <- newFixmeEnv

  runFixmeState e $ do
    initState

    -- let toText = Text.pack . show . pretty

    let merged = [ (Text.unpack a, Text.unpack b)
                 | ListVal @C (Key "fixme-merged" [LitStrVal a, LitStrVal b]) <- log
                 ]

    let deleted = [ Text.unpack a
                  | ListVal @C (Key "fixme-del" [LitStrVal a]) <- log
                  ]

    let attrs = [ (show $ pretty i, a, v)
                | ListVal @C (Key "fixme-set" [LitStrVal a, LitStrVal v, LitStrVal i]) <- log
                ]

    let allowedName s = s `Set.member` allowedAttribs r

    let allowedVal k v = isAllowedVal k v (allowedValues r)

    let fpat (_,f) =  or [ x ?== f | x <- masks r ]
                   && not (and [ x ?== f | x <- ignored r] )

    blobz <- gitListAllBlobs <&> List.nubBy ((==) `on` fst)
                 >>= filterM ( \(h,_) -> blobProcessed h <&> not)

    let files = blobz & reverse . filter fpat

    let fxdef = FixmeDef (List.nub (comm r))  (List.nub ("FIXME:" : pref r))

    fme <- liftIO $ List.nubBy ((==) `on` view fixmeId) . mconcat
              <$> mapConcurrently (parseBlob fxdef) files

    -- FIXME: remove-tag-len-hardcode-somehow-new

    let o = FmtAttr (fromIntegral (idlen r) ) 8 (tpref r) "" ""

    unless ( opt ^. scanDontAdd ) do

      transaction $ do
        for_ files $ \(h,_) -> setProcessed h

        for_ fme $ \f -> do
          liftIO $ print $ pretty (Brief o f)
          putFixme f

    -- FIXME: play only diff for log ?

    -- FIXME: don't play log twice(?)

    -- FIXME: to-play-log-function


      run <- forM attrs $ \(i,a,v) -> do
        ii <- findId i

        let valid = allowedName a && allowedVal a v

        unless (allowedVal a v) do
          liftIO $ hPrint stderr $ "*** error: value" <+> pretty v
                                                      <+> "not allowed for"
                                                      <+> pretty a

        unless (allowedName a) do
          liftIO $ hPrint stderr $ "*** error: attrib name" <+> pretty a
                                                            <+> pretty "not allowed"

        case ii of
           [x] | valid     -> pure [ setAttr Nothing x a v ]
               | otherwise -> pure []
           []  | valid     -> pure [ setAttr Nothing (fromString i) a v ]
               | otherwise -> pure []

           _   -> do
             liftIO $ hPrint stderr $ "fixme-set:"
                                         <+> pretty ii
                                         <+> "is ambiguous, ignored"
             pure mempty

      unless ( opt ^. scanDontAdd ) do
        sequence_ (mconcat run)


      del <- forM deleted $ \i -> do
        ii <- findId i

        case ii of
           [x] -> pure [ setDeleted x ]
           []  -> pure [ setDeleted (fromString i) ]
           _   -> do
             liftIO $ hPrint stderr $ "fixme-del:"
                                         <+> pretty i
                                         <+> "is ambiguous, ignored"
             pure mempty

      unless ( opt ^. scanDontAdd ) do
        sequence_ (mconcat del)

    merges <- forM merged $ \(a,b) -> do
                aId <- findId a
                bId <- findId b

                case (aId, bId) of
                  ( [x], [y] ) -> pure [addMerged x y]
                  ( [], [] )   -> pure [addMerged (fromString a) (fromString b)]
                  _ -> do
                    liftIO $ hPrint stderr $ "fixme-merge"
                                                <+> pretty a
                                                <+> pretty b
                                                <+> "is ambigous, ignored"
                    pure []

    unless ( opt ^. scanDontAdd ) do
      sequence_ (mconcat merges)


runCat :: FixmeHash -> Maybe Int -> Maybe Int -> IO ()
runCat h mbefore mafter = do
  e <- newFixmeEnv

  cfg <- getLocalConfig
  ctx <- getDefaultContext cfg

  runFixmeState e $ do
    ii <- findId (show $ pretty h) <&> listToMaybe
    i <- pure ii  `orDie` show ("fixme not found" <+> pretty h)
    fxm <- getFixme i `orDie` show ("fixme not found" <+> pretty h)

    let bef   = fromMaybe 0 (mbefore <|> fmap fst ctx)
    let aft   = fromMaybe 0 (mafter  <|> fmap snd ctx)
    let self  = length ( fxm ^. fixmeBody )
    let num   = bef + self + aft
    let from  = max 0 ( fxm ^. fixmeLine - bef - 1)

    pager <- getPager fxm cfg

    -- FIXME: check of file is really big
    --   use streaming instead of LBS(?)
    o <- gitReadObject (view fixmeFileGitHash fxm)

    let ls = take num $ drop from $ LBS.lines o

    -- FIXME: use-pager-with-colors!
    --   or temporaty file?
    maybe1 pager (liftIO $ for_ ls LBS.putStrLn)
     \pgr -> liftIO do
        -- print $ pretty pgr
        -- exitFailure
        let input = byteStringInput (LBS.unlines ls)
        let cmd = setStdin input $ setStderr closed
                                 $ shell [qc|{pgr}|]
        code <- runProcess cmd

        unless ( code == ExitSuccess ) do
          liftIO $ for_ ls LBS.putStrLn

parseBlob :: FixmeDef
          -> (GitHash, FilePath)
          -> IO [Fixme]

parseBlob def (gh, fp) = do
  -- FIXME: check if file is too big

  bs <- gitReadObject gh
  let utf8 = decodeUtf8With ignore (LBS.toStrict bs)

  let txt = utf8 & Text.lines
                 & zip [1..]

  let ls = IntMap.fromList txt

  heads' <- forM txt $ \(i,s) -> do
             let h = fixmeHash $ serialise (gh,i)
             let fixme0 = Fixme "" "" h gh fp i 0 mempty mempty
             let fixme = parseOnly (pHeader def fixme0) s
             pure $ either mempty (List.singleton . (i,) . updateId) fixme

  let heads = mconcat heads'
  let hm = IntMap.fromList heads

  forM heads  $ \(i,h) -> do
    let (_, nx) = IntMap.split i ls

    let ss = List.takeWhile ( \(j,_) -> not (IntMap.member j hm) )
                            (IntMap.toList nx)

    let lls = reverse $ nicer
                      $ reverse
                      $ go [] (view fixmeIndent h) (fmap snd ss)

    let r = updateId $ h & set fixmeBody    (view fixmeTitle h : lls)
    pure r

  where

    -- NOTE: constructs id from hash(file,issue-content).
    --       that promises that id's will be more or less
    --       stable and dupes will be only, when content
    --       is duplicated in the same file. It happens,
    --       but it's not a good practice anyway.
    updateId fx = fx & set fixmeId  hash
      where
        body  = view fixmeBody fx
        file  = view fixmeFile fx
        title = view fixmeTitle fx
        tag   = view fixmeTag fx
        bin   = serialise (file,tag,title,body)
        hash  = fixmeHash bin

    nicer [] = []
    nicer (x:xs) | Text.null x = nicer xs
                 | otherwise = x:xs

    go acc  _ [] = acc

    go acc  _ (e1:e2:_) | Text.null e1 && Text.null e2 = acc

    go acc  i (s:rest)  |    calcIndent s > i
                          || startsWithComment s
                          || Text.null s  = go (acc <> [stripAll s]) i rest

                         | otherwise = acc

    stripComment s = headDef s $ catMaybes [ Text.stripPrefix p s | p <- view fixmeComm def  ]

    stripAll s = Text.strip $ stripComment $ Text.strip s

    startsWithComment s = or [ Text.isPrefixOf x ss | x <- view fixmeComm def ]
      where ss = Text.strip s

pHeader :: FixmeDef -> Fixme -> Parser Fixme
pHeader fx fme = do

  spaces <- Atto.takeWhile isHorizontalSpace
  _      <- optional (choice comm)
  _      <- Atto.skipWhile isHorizontalSpace
  tag    <- choice tags
  _      <- Atto.skipWhile isHorizontalSpace
  title' <- Text.pack <$> Atto.manyTill anyChar (endOfInput <|> endOfLine)
  let title = Text.strip title'

  pure $  fme & set fixmeTag     tag
              & set fixmeTitle   title
              & set fixmeIndent  (calcIndent spaces)

  where
    comm = fmap string (view fixmeComm fx)
    tags = fmap string (view fixmeTags fx)

calcIndent :: Text -> Int
calcIndent txt = sum (fmap f s)
  where
    s0 = Text.takeWhile isHorizontalSpace txt
    s = Text.unpack s0

    f c | c == ' '  = 1
        | c == '\t' = 4
        | otherwise = 0

withState :: IO a -> IO a
withState m = do
  e <- newFixmeEnv
  runFixmeState e initState
  m

main :: IO ()
main = join . customExecParser (prefs showHelpOnError) $
  info (helper <*> parser)
  (  fullDesc
  <> header "fixme"
  <> progDesc "trackerless issue management"
  )
  where
    parser ::  O.Parser (IO ())
    parser = hsubparser (  command "init"    (info pInit   (progDesc "init fixme config"))
                        <> command "update"  (info pUpdate (progDesc "update state"))
                        <> command "list"    (info pList   (progDesc "list"))
                        <> command "report"  (info pReport (progDesc "run report"))
                        <> command "cat"     (info pCat    (progDesc "cat a fixme from git object"))
                        <> command "uuid"    (info pUuid   (progDesc "generate uuid"))
                        <> command "set"     (info pSet    (progDesc "set attribute value for a fixmie"))
                        <> command "del"     (info pDel    (progDesc "mark a fixme deleted"))
                        <> command "merge"   (info pMerge  (progDesc "mark a fixme merged"))
                        <> command "meta"    (info (hsubparser pMeta) (progDesc "metadata commands") )
                        <> command "scan"    (info pUpdate (progDesc "obsolete; use update"))
                        )
    pInit = do
      pure runInit

    pUpdateOpts = do
      ScanOpt <$> flag False True ( long "dry" <> short 'n' <> help "dry run" )

    pUpdate = withState . runUpdate <$> pUpdateOpts

    pUuid = pure runUuid

    pListOpts = do
      full <- flag False True ( long "full" <> short 'f' <> help "show full fixme" )
      exact <- flag False True ( long "query" <> short 'q' <> help "query by attribute" )
      filt <- many $ strArgument  ( metavar "FILTER" )
      pure $ ListOpt full filt exact

    pList = withState . runList <$> pListOpts

    pSetOpts = do
      attr <- strArgument ( metavar "ATTRIBUTE" )
      val  <- strArgument ( metavar "VALUE")
      fx  <- strArgument ( metavar "ID" )
      dry <- flag False True ( long "dry" <> short 'n' <> help "dry run" )
      pure (dry, [qc|fixme-set {attr} {val} {fx}|])

    pSet = do
      uncurry runLog <$> pSetOpts

    pDel = do
      fx  <- strArgument ( metavar "ID" )
      dry <- flag False True ( long "dry" <> short 'n' <> help "dry run" )
      pure $ runLog dry [qc|fixme-del {fx}|]

    pMerge = do
      a  <- strArgument ( metavar "FROM" )
      b  <- strArgument ( metavar "TO" )
      dry <- flag False True ( long "dry" <> short 'n' <> help "dry run" )
      pure $ runLog dry [qc|fixme-merged {a} {b}|]


    pCat = do
      fxid  <- strArgument ( metavar "ID" )
      b     <- optional $ option auto ( long "before" <> short 'B' <> help "lines before"  )
      a     <- optional $ option auto ( long "after" <> short 'A' <> help "lines after"  )
      pure $ runCat fxid b a


    pMeta =    command "attribs" (info pMetaAttr  (progDesc "list attributes"))
            <> command "config" (info pConf (progDesc "dumps config"))


    pMetaAttr = do
      pure runListAttribs

    pConf = pure $ withState do

      cfgFile <- readFile confFile

      -- FIXME: better error handling
      r <- pure (parseTop cfgFile) `orDie` "can't parse config"

      print $ vcat (fmap pretty r)

    pReport = do
      name <- optional $ strArgument ( metavar "REPORT-NAME" )
      pure $ withState $ runReport name


