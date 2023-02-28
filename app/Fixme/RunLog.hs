module Fixme.RunLog where

import Fixme.Prelude
import Fixme.Defaults
import Fixme.State

import Control.Monad

runLog :: Bool -> String -> IO ()
runLog dry s = withState do
  print (pretty s)
  unless dry do
    appendFile logFile "\n"
    appendFile logFile s

