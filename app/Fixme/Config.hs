module Fixme.Config where

import Fixme.Prelude
import Fixme.Types
import Data.Config.Suckless

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Set (Set)
import Data.Text qualified as Text
import Data.Text (Text)
import Data.Maybe
import Prettyprinter
import Safe

asText :: Syntax c -> Text
asText = \case
  LitStrVal x -> x
  LitIntVal x -> Text.pack $ show x
  SymbolVal (Id x)  -> x
  e -> Text.pack (show $ pretty e)

masks :: [Syntax C] -> [String]
masks r =
  mconcat [ fmap (show.pretty) fs
          | (ListVal @C (Key "fixme-files" fs) ) <- r
          ]

ignored :: [Syntax C] -> [String]
ignored r =
  mconcat [ fmap (show.pretty) fs
          | (ListVal @C (Key "fixme-files-ignore" fs) ) <- r
          ]

comm :: [Syntax C] -> [Text]
comm r =
  mconcat [ fmap asText fs
          | (ListVal @C (Key "fixme-comments" fs) ) <- r
          ]

pref :: [Syntax C] -> [Text]
pref r = catMaybes $
          [ fmap asText (headMay fs)
          | (ListVal @C (Key "fixme-prefix" fs) ) <- r
          ]

idlen :: [Syntax C] -> Integer
idlen r =
  lastDef 8 [ e
            | (ListVal @C (Key "fixme-id-show-len" [LitIntVal e]) ) <- r
            ]

tpref :: IsString a => [Syntax C] -> a
tpref r =
  lastDef "" [ (fromString.show.pretty) e
             | (ListVal @C (Key "fixme-tag-prefix" [e]) ) <- r
             ]

allowedAttribs :: [Syntax C ] -> Set Text
allowedAttribs r = Set.fromList $
  mconcat [ [ s | SymbolVal (Id s) <- sv  ]
          | (ListVal @C (Key "fixme-attribs" sv) ) <- r
          ]

allowedValues :: [Syntax C ] -> Map Text (Set Text)
allowedValues r = Map.fromListWith (<>) a
  where
    a = [ (txt v, Set.fromList (fmap txt vs))
        | (ListVal @C (Key "fixme-value-set" (v:vs)) ) <- r
        ]

isAllowedVal :: Text -> Text -> Map Text (Set Text) -> Bool
isAllowedVal k v m = case Map.lookup k m of
  Nothing -> True
  Just s  -> v `Set.member` s


