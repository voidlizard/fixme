module Fixme.Prelude
  ( module Control.Monad.IO.Class
  , module Data.Default
  , module GHC.Generics
  , module Prettyprinter
  , module HBS2.Base58
  , module HBS2.Prelude.Plated
  , module HBS2.OrDie
  , IsString(..)
  , Text
  , txt
  , parseFilt
  )
  where


import HBS2.Prelude.Plated
import HBS2.OrDie
import HBS2.Base58

import Control.Monad.IO.Class
import Data.String
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Default
import Prettyprinter
import GHC.Generics (Generic(..))

txt :: Pretty a => a -> Text
txt = Text.pack . show . pretty

-- FIXME: intercalate ":" is a bad idea
parseFilt :: [Text] -> [(Text,Text)]
parseFilt ss = [(Text.strip a,Text.intercalate ":" bs) | (a:bs) <- fmap (Text.splitOn ":") ss]



