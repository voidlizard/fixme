{-# Language TemplateHaskell #-}
{-# Language PatternSynonyms #-}
module Fixme.Types where

import Fixme.Git
import Fixme.Hash
import Data.Config.Suckless

import Data.Default
import Codec.Serialise
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Text (Text)
import Data.Text qualified as Text
import Data.List qualified as List
import GHC.Generics hiding (to)
import Lens.Micro.Platform
import Prettyprinter

type C = MegaParsec

pattern Key :: forall {c}. Id -> [Syntax c] -> [Syntax c]
pattern Key n ns <- SymbolVal  n : ns


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
  , _fixmeDynAttr     :: HashMap Text Text
  }
  deriving stock (Generic,Show)

makeLenses 'Fixme

fixmeLineEnd :: SimpleGetter Fixme Int
fixmeLineEnd = to f
  where
    f b = _fixmeLine b + length (_fixmeBody b)


data FmtAttr =
  FmtAttr
  { _fmtShortId :: Int
  , _fmtTagLen  :: Int
  , _fmtTagPref :: Text
  }

makeLenses 'FmtAttr

instance Default FmtAttr where
  def = FmtAttr 10 8 ""

data Format a = Brief FmtAttr a

instance Pretty (Format Fixme) where
  pretty (Brief fmt f) =   tp <> pretty shortId
                       <+> fill w (pretty (view fixmeTag f))
                       <+> pretty (Text.take 50 $ view fixmeTitle f)

    where
      shortId = List.take (fmt ^. fmtShortId) (show (pretty (view fixmeId f)))
      w = fmt ^. fmtTagLen
      tp = pretty $ fmt ^. fmtTagPref



instance Serialise Fixme

