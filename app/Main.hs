{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# Language PatternSynonyms #-}
{-# Language TemplateHaskell #-}
module Main where

import Fixme.Defaults
import Fixme.OrDie
import Fixme.Git
import Fixme.Hash

import Data.Config.Suckless

import Data.Function
import Codec.Serialise
import Control.Concurrent.Async
import Control.Monad
import Data.Attoparsec.Text
import Data.Attoparsec.Text qualified as Atto
import Data.ByteString.Lazy.Char8 qualified as LBS
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
import Options.Applicative hiding (Parser)
import Options.Applicative qualified as O
import Prettyprinter
import Prettyprinter.Render.Text
import Safe
import System.Directory
import Data.UUID.V4 as UUID
import Lens.Micro.Platform
import System.FilePattern
import Data.HashSet qualified as HashSet
import System.Exit
import Data.Trie.Set qualified as Trie

pattern Key :: forall {c}. Id -> [Syntax c] -> [Syntax c]
pattern Key n ns <- SymbolVal  n : ns

type C = MegaParsec

data FixmeDef =
  FixmeDef
  { _fixmeComm :: [Text]
  , _fixmeTags :: [Text]
  }

makeLenses 'FixmeDef

type FixmeTitle = Text
type FixmeTag = Text

data Fixme =
  Fixme
  { _fixmeTag         :: FixmeTag
  , _fixmeTitle       :: FixmeTitle
  , _fixmeId          :: FixmeHash
  , _fixmeFileGitHash :: GitHash
  , _fixmeFile        :: FilePath
  , _fixmeLine        :: Int
  , _fixmeIndent      :: Int
  , _fixmeBody        :: [Text]
  }
  deriving stock (Show)

makeLenses 'Fixme

data ScanOpt =
  ScanOpt
  { _scanFull :: Maybe Bool
  , _scanFilt :: [Text]
  }

makeLenses 'ScanOpt

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

  unless confExists do
    Text.writeFile confFile defConfig

  unless confExists do
    Text.writeFile logFile defLog

runUuid :: IO ()
runUuid = do
  uuid <- UUID.nextRandom
  print $ pretty "uuid:" <+> pretty (show uuid)

runScan :: ScanOpt -> Maybe FilePath -> IO ()
runScan opt fp = do

  cfgFile <- readFile confFile
  logFileData <- readFile logFile

  -- FIXME: better error handling
  r <- pure (parseTop cfgFile) `orDie` "can't parse config"

  log <- pure (parseTop logFileData) `orDie` "can't parse log"

  let masks = mconcat [ fmap (show.pretty) fs
                      | (ListVal @C (Key "fixme-files" fs) ) <- r
                      ]

  let ignore = mconcat [ fmap (show.pretty) fs
                       | (ListVal @C (Key "fixme-files-ignore" fs) ) <- r
                       ]

  let comm = mconcat [ fmap (fromString.show.pretty) fs
                     | (ListVal @C (Key "fixme-comments" fs) ) <- r
                     ]

  let pref = mconcat [ fmap (fromString.show.pretty) fs
                     | (ListVal @C (Key "fixme-prefix" fs) ) <- r
                     ]

  let idlen = lastDef 8 [ e
                        | (ListVal @C (Key "fixme-id-show-len" [LitIntVal e]) ) <- r
                        ]

  let tpref = lastDef "" [ (fromString.show.pretty) e
                         | (ListVal @C (Key "fixme-tag-prefix" [e]) ) <- r
                         ]


  let suff  = lastDef "" [  e
                         | (ListVal @C (Key "fixme-list-full-row-suff" [LitStrVal e]) ) <- r
                         ]

  let rpref = lastDef "" [  e
                         | (ListVal @C (Key "fixme-list-full-row-pref" [LitStrVal e]) ) <- r
                         ]

  let toText = Text.pack . show . pretty

  let merged = [ (toText a, toText b)
               | ListVal @C (Key "fixme-merged" [LitStrVal a, LitStrVal b]) <- log
               ] & Trie.fromList . fmap (Text.unpack . fst)

  let fpat (h,f) =  or [ x ?== f | x <- masks ]
                 && not (and [ x ?== f | x <- ignore ] )

  files <- gitListAllBlobs <&> reverse . filter fpat

  let fxdef = FixmeDef (List.nub comm)  (List.nub ("FIXME:" : pref))

  let ids = view scanFilt opt

  -- FIXME: filt-might-be-very-slow
  let filt fxm = notMerged && null ids || or [ pre p | p <- ids ]
        where
          tid = fromString $ show $ pretty (view fixmeId fxm)
          tit = view fixmeTitle fxm
          pre p =    Text.isPrefixOf p tid
                       || Text.isPrefixOf p tit
                       || Text.isPrefixOf p (view fixmeTag fxm)

          notMerged = Trie.null $ Trie.intersection merged prefAll

          prefAll = Trie.fromList [ x :: String
                                  | x <- prefAllL, length x >= 6
                                  ]

          prefAllL = Trie.toList $ Trie.prefixes (Trie.fromList [Text.unpack tid])


  fme <- List.nubBy ((==) `on` view fixmeId) . filter filt . mconcat
            <$> mapConcurrently (parseBlob fxdef) files

  let tl = maximumDef 6 $ fmap Text.length pref

  let fmt = if view scanFull opt == Just True then Full o else Brief o
        where
          o = FmtAttr (fromIntegral idlen) tl tpref mbPre mbSuff
          mbPre   | view scanFull opt == Just True  = rpref
                  | otherwise = ""

          mbSuff  | view scanFull opt == Just True  = suff
                  | otherwise = ""

  putDoc (vcat (fmap (pretty . fmt) fme))


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

main :: IO ()
main = join . customExecParser (prefs showHelpOnError) $
  info (helper <*> parser)
  (  fullDesc
  <> header "fixme"
  <> progDesc "trackerless issue management"
  )
  where
    parser ::  O.Parser (IO ())
    parser = hsubparser (  command "init" (info pInit (progDesc "init fixme config"))
                        <> command "scan" (info pScan (progDesc "scan"))
                        <> command "uuid" (info pUuid (progDesc "generate uuid"))
                        )
    pInit = do
      pure runInit

    pScanOpts = do
      ScanOpt <$> optional (flag' True ( long "full" <> short 'f' <> help "full format"))
              <*> many (strArgument  ( metavar "FILTER" ))

    pScan = do
      opts <- pScanOpts
      fp <- optional $ strArgument ( metavar "HASH" )
      pure (runScan opts fp)

    pUuid = pure runUuid


