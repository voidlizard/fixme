{-# Language PatternSynonyms #-}
module Fixme.LocalConfig where

import Data.Config.Suckless
import Fixme.Prelude
import Fixme.OrDie
import Fixme.Types

import Text.InterpolatedString.Perl6 (qc)
import Data.Functor
import System.FilePath
import System.Directory
import Prettyprinter
import Lens.Micro.Platform
import Data.List qualified as L
import Safe

newtype LocalConfig = LocalConfig [Syntax C]
                      deriving newtype (Monoid, Semigroup)

getLocalConfig :: MonadIO m => m LocalConfig
getLocalConfig = liftIO do

  let cfg = "config"

  xdg <- getXdgDirectory XdgConfig "fixme"

  let localConf = xdg </> cfg

  createDirectoryIfMissing True xdg

  here <- doesFileExist localConf

  if not here then
    pure mempty

  else do
    file <- readFile localConf
    conf <- pure (parseTop file)
      `orDie` show ( "can't parse config" <+> pretty localConf )

    pure (LocalConfig conf)


getPager :: MonadIO m => Fixme -> LocalConfig -> m (Maybe String)
getPager fxm (LocalConfig cfg) = do
  -- liftIO $ print $ pretty cfg

  let ext = dropWhile (== '.') $ takeExtension (fxm ^. fixmeFile)

  let pager = lastDef [] [  [ show (pretty s) |  s <- xs ]
                         | (ListVal @C (Key "fixme-pager" xs) ) <- cfg
                         ]

  pure $ case pager of
    [] -> Nothing

    ("bat":args) -> do
      let fname = [qc|--file-name {view fixmeFile fxm}|] :: String
      Just [qc|bat {fname} {unwords args}|]

    xs -> Just $ unwords xs

