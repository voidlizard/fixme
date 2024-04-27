module Fixme.RunLog where

import Fixme.Prelude
import Fixme.State

runLog :: FixmePerks m => Bool -> String -> FixmeState m ()
runLog dry s = do
  e  <- newFixmeEnvDefault
  let logFile = view localLogFile e
  runFixmeState e $ liftIO do
    unless dry do
      appendFile logFile "\n"
      appendFile logFile s

