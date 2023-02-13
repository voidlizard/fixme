module Fixme.RunListAttribs where

import Fixme.Prelude
import Fixme.State

import Data.Foldable(for_)
import Prettyprinter


runListAttribs :: IO ()
runListAttribs = do
  e <- newFixmeEnv

  runFixmeState e $ do
    s <- listAttrs
    liftIO $ for_ s (print.pretty)


