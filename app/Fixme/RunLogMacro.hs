module Fixme.RunLogMacro where

import Fixme.Prelude
import Fixme.Defaults
import Fixme.RunLog
import Data.Config.Suckless

import Data.Generics.Uniplate.Data()
import Data.Map qualified as Map
import Data.Maybe
import System.Exit
import Data.Text qualified as Text


runLogMacro :: [String] -> IO ()

runLogMacro [] = do
  die "log macro should be in form: name args*"

runLogMacro (name:args) = do

  cfgFile <- readFile confFile
  r <- pure (parseTop cfgFile) `orDie` "can't parse config"

  let argNums = [ Text.pack ("$" <> show i) | i <- [1..32] ]
  let argSub  = zip argNums (fmap fromString args) & Map.fromList

  let subst x = flip transformBi x \case
                  LitStrVal s | s `Map.member` argSub ->
                    Literal @C noContext (mkLit (fromMaybe s $ Map.lookup s argSub))

                  other -> other

  let macros  = [ (show $ pretty n, de)
                | (ListVal (Key "fixme-log-macro" (SymbolVal n:de)) ) <- r
                ] & Map.fromList & subst

  let ma = Map.lookup name macros

  maybe1 ma (pure ()) $ \macro -> do

    for_ macro $ \m -> do
      runLog False (show $ pretty m)


