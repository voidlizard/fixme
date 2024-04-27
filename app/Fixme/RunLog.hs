module Fixme.RunLog where

import Fixme.Prelude
import Fixme.Defaults
import Fixme.State

runLog :: FixmePerks m => Bool -> String -> FixmeState m ()
runLog dry s = do
  e  <- newFixmeEnvDefault
  runFixmeState e $ liftIO do
    print (pretty s)
    unless dry do
      appendFile logFile "\n"
      appendFile logFile s

