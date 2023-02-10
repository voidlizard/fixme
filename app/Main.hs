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

import Data.Config.Suckless

import Codec.Serialise
import Control.Concurrent.Async
import Control.Monad
import Data.Attoparsec.Text
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
import Data.Traversable
import Data.UUID.V4 as UUID
import Lens.Micro.Platform
import Options.Applicative hiding (Parser)
import Options.Applicative qualified as O
import Prettyprinter
-- import Prettyprinter.Render.Text
import Safe
import System.Directory
import System.Exit
import System.FilePattern
import System.IO
import Text.InterpolatedString.Perl6 (qc)

pattern Key :: forall {c}. Id -> [Syntax c] -> [Syntax c]
pattern Key n ns <- SymbolVal  n : ns

type C = MegaParsec


newtype ScanOpt =
  ScanOpt
  { _scanDontAdd :: Bool
  }

makeLenses 'ScanOpt

data ListOpts =
  ListOpt
  { _listShowFull :: Bool
  , _listFilters  :: [Text]
  }

makeLenses 'ListOpt

data FmtAttr =
  FmtAttr
  { _fmtShortId :: Int
  , _fmtTagLen  :: Int
  , _fmtTagPref :: Text
  , _fmtPref    :: Text
  , _fmtSuff    :: Text
  }

makeLenses 'FmtAttr

data Format a = Brief FmtAttr a
              | Full FmtAttr a

instance Pretty (Format Fixme) where
  pretty (Brief fmt f) =   tp <> pretty shortId
                       <+> fill w (pretty (view fixmeTag f))
                       <+> pretty (Text.take 50 $ view fixmeTitle f)

    where
      shortId = List.take (fmt ^. fmtShortId) (show (pretty (view fixmeId f)))
      w = fmt ^. fmtTagLen
      tp = pretty $ fmt ^. fmtTagPref

  pretty (Full fmt f) =   pretty (view fmtPref fmt)
                       <> pretty (view fixmeTag f)
                       <+> pretty (view fixmeTitle f)
                       <> line
                       <> "id:" <+> pretty (fmt ^. fmtTagPref)
                                <>  pretty (view fixmeId f)
                       <> line
                       <> "file-hash:" <+> pretty (view fixmeFileGitHash f)
                       <> line
                       <> "file:" <+> pretty (view fixmeFile f)
                                  <> colon
                                  <> pretty (view fixmeLine f)

                       <> line
                       <> line
                       <> vcat (fmap (indent 0 . pretty) (view fixmeBody f))
                       <> pretty (view fmtSuff fmt)
                       <> line

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

masks :: [Syntax C] -> [String]
masks r =
  mconcat [ fmap (show.pretty) fs
          | (ListVal @C (Key "fixme-files" fs) ) <- r
          ]

ignored :: [Syntax C] -> [String]
ignored r =
  mconcat [ fmap (show.pretty) fs
          | (ListVal @C (Key "fixme-files-ignore" fs) ) <- r
          ]

comm :: IsString b => [Syntax C] -> [b]
comm r =
  mconcat [ fmap (fromString.show.pretty) fs
          | (ListVal @C (Key "fixme-comments" fs) ) <- r
          ]

pref :: IsString b => [Syntax C] -> [b]
pref r =
  mconcat [ fmap (fromString.show.pretty) fs
          | (ListVal @C (Key "fixme-prefix" fs) ) <- r
          ]

idlen :: [Syntax C] -> Integer
idlen r =
  lastDef 8 [ e
            | (ListVal @C (Key "fixme-id-show-len" [LitIntVal e]) ) <- r
            ]

tpref :: IsString a => [Syntax C] -> a
tpref r =
  lastDef "" [ (fromString.show.pretty) e
             | (ListVal @C (Key "fixme-tag-prefix" [e]) ) <- r
             ]

suff :: [Syntax C] -> Text
suff r =
  lastDef "" [  e
             | (ListVal @C (Key "fixme-list-full-row-suff" [LitStrVal e]) ) <- r
             ]

rpref :: [Syntax C] -> Text
rpref r  =
  lastDef "" [  e
             | (ListVal @C (Key "fixme-list-full-row-pref" [LitStrVal e]) ) <- r
             ]

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
    appendFile logFile "\n"

runList :: ListOpts -> IO ()
runList opt = do
  e <- newFixmeEnv

  cfgFile <- readFile confFile
  logFileData <- readFile logFile

  r <- pure (parseTop cfgFile) `orDie` "can't parse config"
  log <- pure (parseTop logFileData) `orDie` "can't parse log"

  fxms <- runFixmeState e (loadFixme (opt ^. listFilters))

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

runScan :: ScanOpt -> IO ()
runScan opt = do

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

        case ii of
           [x] -> pure [ setAttr Nothing x a v ]
           _   -> do
             liftIO $ hPrint stderr $ "fixme-set:"
                                         <+> pretty ii
                                         <+> "is ambigous, ignored"
             pure mempty

      unless ( opt ^. scanDontAdd ) do
        sequence_ (mconcat run)


      del <- forM deleted $ \i -> do
        ii <- findId i

        case ii of
           [x] -> pure [ setDeleted x ]
           _   -> do
             liftIO $ hPrint stderr $ "fixme-del:"
                                         <+> pretty ii
                                         <+> "is ambigous, ignored"
             pure mempty

      unless ( opt ^. scanDontAdd ) do
        sequence_ (mconcat del)

    merges <- forM merged $ \(a,b) -> do
                aId <- findId a
                bId <- findId b

                case (aId, bId) of
                  ( [x], [y] ) -> pure [addMerged x y]
                  _ -> do
                    liftIO $ hPrint stderr $ "fixme-merge"
                                                <+> pretty a
                                                <+> pretty b
                                                <+> "is ambigous, ignored"
                    pure []

    unless ( opt ^. scanDontAdd ) do
      sequence_ (mconcat merges)

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
             let fixme0 = Fixme "" "" h gh fp i 0 mempty
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

    let r = updateId $ h & set fixmeBody (view fixmeTitle h : lls)
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
    parser = hsubparser (  command "init"  (info pInit (progDesc "init fixme config"))
                        <> command "scan"  (info pScan (progDesc "scan"))
                        <> command "list"  (info pList (progDesc "list"))
                        -- <> command "track" (info pScan (progDesc "track fixme"))
                        <> command "uuid"  (info pUuid (progDesc "generate uuid"))
                        <> command "set"   (info pSet  (progDesc "set attribute value for a fixmie"))
                        <> command "del"   (info pDel  (progDesc "mark a fixme deleted"))
                        )
    pInit = do
      pure runInit

    pScanOpts = do
      ScanOpt <$> flag False True ( long "dry" <> short 'n' <> help "dry run" )

    pScan = withState . runScan <$> pScanOpts

    pUuid = pure runUuid

    pListOpts = do
      full <- flag False True ( long "full" <> short 'f' <> help "show full fixme" )
      filt <- many $ strArgument  ( metavar "FILTER" )
      pure $ ListOpt full filt

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


