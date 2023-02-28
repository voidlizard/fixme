module Fixme.Prelude
  ( module Control.Monad.IO.Class
  , module Fixme.OrDie
  , module Data.Default
  , module GHC.Generics
  , module Prettyprinter
  , IsString(..)
  , Text
  , maybe1
  , txt
  , parseFilt
  )
  where

import Fixme.OrDie

import Control.Monad.IO.Class
import Data.String
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Default
import Prettyprinter
import GHC.Generics (Generic(..))

maybe1 :: Maybe a -> b -> (a -> b) -> b
maybe1 mb n j = maybe n j mb

txt :: Pretty a => a -> Text
txt = Text.pack . show . pretty

-- FIXME: intercalate ":" is a bad idea
parseFilt :: [Text] -> [(Text,Text)]
parseFilt ss = [(Text.strip a,Text.intercalate ":" bs) | (a:bs) <- fmap (Text.splitOn ":") ss]



