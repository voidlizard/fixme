module Fixme.RunLog where

import Fixme.Prelude
import Fixme.Defaults
import Fixme.State
import Fixme.LocalConfig

runLog :: Bool -> String -> IO ()
runLog dry s = do
  e  <- getLocalConfigPath >>= newFixmeEnv
  runFixmeState e $ liftIO do
    print (pretty s)
    unless dry do
      appendFile logFile "\n"
      appendFile logFile s

