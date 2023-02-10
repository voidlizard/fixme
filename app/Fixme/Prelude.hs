module Fixme.Prelude
  ( module Control.Monad.IO.Class
  , maybe1
  )
  where


import Control.Monad.IO.Class

maybe1 :: Maybe a -> b -> (a -> b) -> b
maybe1 mb n j = maybe n j mb

