{-# Language TemplateHaskell #-}
module Fixme.Types where

import Fixme.Git
import Fixme.Hash

import Data.Text (Text)
import Lens.Micro.Platform
import GHC.Generics
import Codec.Serialise

data FixmeDef =
  FixmeDef
  { _fixmeComm :: [Text]
  , _fixmeTags :: [Text]
  }

makeLenses 'FixmeDef

type FixmeTitle = Text
type FixmeTag = Text

data Fixme =
  Fixme
  { _fixmeTag         :: FixmeTag
  , _fixmeTitle       :: FixmeTitle
  , _fixmeId          :: FixmeHash
  , _fixmeFileGitHash :: GitHash
  , _fixmeFile        :: FilePath
  , _fixmeLine        :: Int
  , _fixmeIndent      :: Int
  , _fixmeBody        :: [Text]
  }
  deriving stock (Generic,Show)

makeLenses 'Fixme

instance Serialise Fixme

