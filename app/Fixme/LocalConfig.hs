module Fixme.LocalConfig where

import Data.Config.Suckless
import Fixme.OrDie

import Data.Functor
import System.FilePath
import System.Directory
import Prettyprinter

getLocalConfig :: IO [Syntax MegaParsec]
getLocalConfig = do

  let cfg = "config"

  xdg <- getXdgDirectory XdgData "fixme"

  let localConf = xdg </> cfg

  createDirectoryIfMissing True xdg

  here <- doesFileExist localConf

  if not here then
    pure mempty

  else do
    conf <- pure (parseTop localConf)
      `orDie` show ( "can't parse config" <+> pretty localConf )

    pure mempty



