module Fixme.Prelude
  ( module HBS2.Prelude.Plated
  , module HBS2.Base58
  , module HBS2.OrDie
  , module HBS2.System.Dir
  , module Control.Monad.IO.Class
  , module Data.Default
  , module Data.Function
  , module Data.Functor
  , module Prettyprinter
  , module Lens.Micro.Platform
  , module Control.Monad.Reader
  , module GHC.Generics
  , IsString(..)
  , txt
  , parseFilt
  )
  where


import HBS2.Prelude.Plated hiding (at)
import HBS2.OrDie
import HBS2.System.Dir
import HBS2.Base58

import Control.Monad.IO.Class
import Data.Text qualified as Text
import Data.Default
import Data.Function
import Data.Functor
import Prettyprinter
import GHC.Generics (Generic)
import Control.Monad.Reader
import Lens.Micro.Platform

txt :: Pretty a => a -> Text
txt = Text.pack . show . pretty

-- FIXME: intercalate ":" is a bad idea
parseFilt :: [Text] -> [(Text,Text)]
parseFilt ss = [(Text.strip a,Text.intercalate ":" bs) | (a:bs) <- fmap (Text.splitOn ":") ss]



