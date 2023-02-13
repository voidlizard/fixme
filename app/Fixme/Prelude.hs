module Fixme.Prelude
  ( module Control.Monad.IO.Class
  , IsString(..)
  , Text
  , maybe1
  )
  where


import Control.Monad.IO.Class
import Data.String
import Data.Text (Text)

maybe1 :: Maybe a -> b -> (a -> b) -> b
maybe1 mb n j = maybe n j mb

