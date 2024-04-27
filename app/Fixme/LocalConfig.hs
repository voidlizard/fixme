{-# Language PatternSynonyms #-}
module Fixme.LocalConfig where

import Data.Config.Suckless
import Fixme.Prelude
import Fixme.Types

import Text.InterpolatedString.Perl6 (qc)

type LocalConfig = [Syntax C]

class PagerFeatures a where
  pagerHighlightRow :: a -> Maybe Int
  pagerHighlightRow _ = Nothing

instance PagerFeatures ()


getPager :: (MonadIO m, PagerFeatures ft) => ft -> Fixme -> LocalConfig -> m (Maybe String)
getPager ft fxm cfg = do
  -- liftIO $ print $ pretty cfg

  let ext = dropWhile (== '.') $ takeExtension (fxm ^. fixmeFile)

  let pager = lastDef [] [  [ show (pretty s) |  s <- xs ]
                         | (ListVal (Key "fixme-pager" xs) ) <- cfg
                         ]

  pure $ case pager of
    [] -> Nothing

    ("bat":args) -> do
      let hi = maybe1 (pagerHighlightRow ft) mempty \r -> [qc|-H {r}|] :: String
      let fname = [qc|{hi} --file-name {view fixmeFile fxm}|] :: String
      Just [qc|bat {fname} {unwords args}|]

    xs -> Just $ unwords xs

getDefaultContext :: MonadIO m => [Syntax C] -> m (Maybe (Int, Int))
getDefaultContext cfg = do
  pure $
   lastMay  [ (fromIntegral a, fromIntegral b)
            | (ListVal (Key "fixme-def-context" [LitIntVal a, LitIntVal b]) ) <- cfg
            ]

