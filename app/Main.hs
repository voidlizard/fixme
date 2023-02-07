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

import Control.Monad
import Data.Attoparsec.Text
import Data.Attoparsec.Text qualified as Atto
import Data.Foldable (for_)
import Data.Generics.Uniplate.Data()
import Data.List qualified as List
import Options.Applicative hiding (Parser)
import Options.Applicative qualified as O
import Prettyprinter
import System.Directory
import System.FilePattern.Directory
import Control.Concurrent.Async

import Lens.Micro.Platform

pattern Key :: forall {c}. Id -> [Syntax c] -> [Syntax c]
pattern Key n ns <- SymbolVal  n : ns

type C = MegaParsec

newtype FixmeHeader =
  FixmeHeader
  { fixmeIndent :: Int
  }

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
  { _fixmeTag   :: FixmeTag
  , _fixmeTitle :: FixmeTitle
  , _fixmeFile  :: FilePath
  , _fixmeLine  :: Int
  , _fixmeIdent :: Int
  }
  deriving stock (Show)

makeLenses 'Fixme

newtype Brief a = Brief a

instance Pretty (Brief Fixme) where
  pretty (Brief f) = pretty (view fixmeTag f)
                      <+> pretty (Text.take 50 $ view fixmeTitle f)

runInit  :: IO ()
runInit = do

  print $ pretty "init" <+> pretty confDir

  createDirectoryIfMissing True confDir
  confExists <- doesFileExist confFile

  unless confExists do
    Text.writeFile confFile defConfig

runScan :: IO ()
runScan = do

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

  files <- getDirectoryFilesIgnore "." masks ignore


  let fxdef = FixmeDef (List.nub comm)  (List.nub ("FIXME:" : pref))

  fme <- mconcat <$> mapConcurrently (parseFile fxdef) files

  mapM_ (print.pretty.Brief) fme


parseFile :: FixmeDef -> FilePath -> IO [Fixme]
parseFile def fp = do
  -- hPutStrLn stderr fp
  -- FIXME: check if file is too big
  txt <- Text.readFile fp <&> Text.lines
                          <&> zip [0..]

  r <- forM txt $ \(i,s) -> do
        let fixme0 = Fixme "" "" fp i 0
        let fixme = parseOnly (pHeader def fixme0) s
        case fixme of
          Left{}  -> pure mempty
          Right h -> pure [h]

  pure (mconcat r)

pHeader :: FixmeDef -> Fixme -> Parser Fixme
pHeader fx fme = do

  spaces <- Atto.takeWhile isHorizontalSpace
  _      <- optional (choice comm)
  _      <- Atto.skipWhile isHorizontalSpace
  tag    <- choice tags
  _      <- Atto.skipWhile isHorizontalSpace
  title  <- Text.pack <$> Atto.manyTill anyChar (endOfInput <|> endOfLine)

  pure $  fme & set fixmeTag   tag
              & set fixmeTitle title

  where
    comm = fmap string (view fixmeComm fx)
    tags = fmap string (view fixmeTags fx)


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

    pScan = do
      pure runScan


