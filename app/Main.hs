{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# Language PatternSynonyms #-}
module Main where

import Fixme.Defaults
import Fixme.OrDie

import Data.Config.Suckless

import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text

import Text.InterpolatedString.Perl6 (qc)
import Control.Monad
import Options.Applicative
import Prettyprinter
import System.Directory
import System.FilePath.Posix
import Data.Either
import Data.Data

import System.FilePattern.Directory

import Data.Generics.Uniplate.Data()
import Data.Generics.Uniplate.Operations


type C = MegaParsec

pattern Key :: forall {c}. Id -> [Syntax c] -> [Syntax c]
pattern Key n ns <- SymbolVal  n : ns

runInit  :: IO ()
runInit = do

  print $ pretty "init" <+> pretty confDir

  createDirectoryIfMissing True confDir
  confExists <- doesFileExist confFile

  unless confExists do
    Text.writeFile confFile defConfig

runCheck :: IO ()
runCheck = do

  cfgFile <- readFile confFile

  -- FIXME: better error handling
  r <- pure (parseTop cfgFile) `orDie` "can't parse config"

  let masks = mconcat [ fmap (show.pretty) fs
                      | (ListVal @C (Key "fixme-files" fs) ) <- r
                      ]

  let ignore = mconcat [ fmap (show.pretty) fs
                      | (ListVal @C (Key "fixme-files-ignore" fs) ) <- r
                      ]

  files <- getDirectoryFilesIgnore "." masks ignore

  -- print $ pretty masks

  mapM_ parseFile files

parseFile :: FilePath -> IO ()
parseFile fp = do
  putStrLn fp
  -- FIXME: check if file is too big
  txt <- Text.readFile fp
  pure ()


main :: IO ()
main = join . customExecParser (prefs showHelpOnError) $
  info (helper <*> parser)
  (  fullDesc
  <> header "fixme"
  <> progDesc "trackerless issue management"
  )
  where
    parser ::  Parser (IO ())
    parser = hsubparser (  command "init" (info pInit (progDesc "init fixme config"))
                        <> command "check" (info pCheck (progDesc "check"))
                        )
    pInit = do
      pure runInit

    pCheck = do
      pure runCheck


