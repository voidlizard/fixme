module Fixme.RunListAttribs where

import Fixme.Prelude
import Fixme.State
import Fixme.LocalConfig


runListAttribs :: IO ()
runListAttribs = do
  e <- newFixmeEnvDefault

  runFixmeState e $ do
    s <- listAttrs
    liftIO $ for_ s (print.pretty)


