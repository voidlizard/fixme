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

  print $ vcat (fmap pretty r)

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



