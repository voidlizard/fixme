{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# Language PatternSynonyms #-}
{-# Language TemplateHaskell #-}
module Main where

import Fixme.Defaults
import Fixme.OrDie

import Data.Config.Suckless

import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.String

import Control.Concurrent.Async
import Control.Monad
import Data.Attoparsec.Text
import Data.Attoparsec.Text qualified as Atto
import Data.Foldable (for_)
import Data.Generics.Uniplate.Data()
import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap
import Data.List qualified as List
import Options.Applicative hiding (Parser)
import Options.Applicative qualified as O
import Prettyprinter
import System.Directory
import System.FilePattern.Directory
import Data.Maybe
import Safe

import Lens.Micro.Platform

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
  { _fixmeTag    :: FixmeTag
  , _fixmeTitle  :: FixmeTitle
  , _fixmeFile   :: FilePath
  , _fixmeLine   :: Int
  , _fixmeIndent :: Int
  , _fixmeBody   :: [Text]
  }
  deriving stock (Show)

makeLenses 'Fixme

newtype ScanOpt =
  ScanOpt
  { _scanFull :: Maybe Bool
  }

makeLenses 'ScanOpt

data Format a = Brief a
              | Full a

instance Pretty (Format Fixme) where
  pretty (Brief f) = pretty (view fixmeTag f)
                      <+> pretty (Text.take 50 $ view fixmeTitle f)

  pretty (Full f) = pretty (view fixmeTag f)
                      <+> pretty (Text.take 50 $ view fixmeTitle f)
                      <> line
                      <> "file:" <+> pretty (view fixmeFile f)
                                 <> colon
                                 <> pretty (view fixmeLine f)

                      <> line
                      <> line
                      <> vcat (fmap pretty (view fixmeBody f))
                      <> line

runInit  :: IO ()
runInit = do

  print $ pretty "init" <+> pretty confDir

  createDirectoryIfMissing True confDir
  confExists <- doesFileExist confFile

  unless confExists do
    Text.writeFile confFile defConfig

runScan :: ScanOpt -> Maybe FilePath -> IO ()
runScan opt fp = do

  cfgFile <- readFile confFile

  -- FIXME: better error handling
  r <- pure (parseTop cfgFile) `orDie` "can't parse config"

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

  files <- case fp of
           Nothing -> getDirectoryFilesIgnore "." masks ignore
           Just fn -> pure [fn]

  let fxdef = FixmeDef (List.nub comm)  (List.nub ("FIXME:" : pref))

  fme <- mconcat <$> mapConcurrently (parseFile fxdef) files

  let fmt = if view scanFull opt == Just True then Full else Brief

  mapM_ (print.pretty.fmt) fme


parseFile :: FixmeDef -> FilePath -> IO [Fixme]
parseFile def fp = do
  -- hPutStrLn stderr fp
  -- FIXME: check if file is too big
  txt <- Text.readFile fp <&> Text.lines
                          <&> zip [1..]

  let ls = IntMap.fromList txt

  heads' <- forM txt $ \(i,s) -> do
             let fixme0 = Fixme "" "" fp i 0 mempty
             let fixme = parseOnly (pHeader def fixme0) s
             pure $ either mempty (List.singleton . (i,)) fixme

  let heads = mconcat heads'
  let hm = IntMap.fromList heads

  forM heads  $ \(i,h) -> do
    let (_, nx) = IntMap.split i ls

    let ss = List.takeWhile ( \(j,_) -> not (IntMap.member j hm) )
                            (IntMap.toList nx)

    let lls = reverse $ nicer
                      $ reverse
                      $ go [] (view fixmeIndent h) (fmap snd ss)

    let r = h & set fixmeBody (view fixmeTitle h : lls)
    pure r

  where

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
                        )
    pInit = do
      pure runInit

    pScanOpts = do
      ScanOpt <$> optional (flag' True ( long "full" <> short 'f' <> help "full format"))

    pScan = do
      opts <- pScanOpts
      fp <- optional $ strArgument ( metavar "HASH" )
      pure (runScan opts fp)


