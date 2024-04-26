{-# Language PatternSynonyms #-}
module Fixme.LocalConfig where

import Data.Config.Suckless
import Fixme.Prelude
import Fixme.Types

import Text.InterpolatedString.Perl6 (qc)
import System.FilePath
import System.Directory
import Lens.Micro.Platform
import Safe

newtype LocalConfig = LocalConfig [Syntax C]
                      deriving newtype (Monoid, Semigroup)


class PagerFeatures a where
  pagerHighlightRow :: a -> Maybe Int
  pagerHighlightRow _ = Nothing

instance PagerFeatures ()


cfg :: FilePath
cfg = "config"

getLocalConfigPath  :: MonadIO m => m FilePath
getLocalConfigPath = liftIO do
  xdg <- getXdgDirectory XdgConfig "fixme"
  pure $ xdg </> cfg

getLocalConfig :: MonadIO m => m LocalConfig
getLocalConfig = liftIO do
  localConf <- getLocalConfigPath

  createDirectoryIfMissing True localConf

  here <- doesFileExist localConf

  if not here then
    pure mempty

  else do
    file <- readFile localConf
    conf <- pure (parseTop file)
      `orDie` show ( "can't parse config" <+> pretty localConf )

    pure (LocalConfig conf)


getPager :: (MonadIO m, PagerFeatures ft) => ft -> Fixme -> LocalConfig -> m (Maybe String)
getPager ft fxm (LocalConfig cfg) = do
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

getDefaultContext :: MonadIO m => LocalConfig -> m (Maybe (Int, Int))
getDefaultContext (LocalConfig cfg) = do
  pure $
   lastMay  [ (fromIntegral a, fromIntegral b)
            | (ListVal (Key "fixme-def-context" [LitIntVal a, LitIntVal b]) ) <- cfg
            ]

