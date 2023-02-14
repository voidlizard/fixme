module Fixme.RunReport where

import Fixme.Prelude
import Fixme.Types
import Fixme.Defaults
import Fixme.State

import Data.Config.Suckless

import Control.Monad
import Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString.Lazy qualified as BS
import Data.Function
-- import Data.Functor
import Data.Text qualified as Text
import qualified Data.Text.Lazy.IO as TIO
import qualified Data.Text.Lazy as LT
import Lens.Micro.Platform hiding ((.=))
import Prettyprinter
import Safe
import System.Directory
import System.FilePath
import Text.Microstache hiding (Key)
import Data.HashMap.Strict (HashMap)
import System.IO

import Data.Matrix qualified as M

newtype Report = Report { items :: [HashMap Text Text] }
                 deriving stock (Generic)

instance ToJSON Report

columns :: [Syntax C] -> LT.Text -> IO LT.Text
columns syn ss = do

  let (delim',fmts) = splitAt 1 syn

  let delim = case fmap txt delim' of
                [x] -> LT.fromStrict x
                _   -> "\t"

  let lz = LT.lines ss & fmap (LT.splitOn delim)

  pure $ LT.unlines $ LT.intercalate " " <$> fmap (zipWith fmt fmts) lz

  where

    fmt :: Syntax C -> LT.Text -> LT.Text
    fmt sy s = case sy of
      SymbolVal "_" -> s
      LitIntVal n   -> LT.take (fromIntegral n) $ LT.justifyLeft (fromIntegral n) ' ' s
      _             -> ""


applyPost :: [[Syntax C]] -> LT.Text -> IO LT.Text
applyPost syn source  = do

  let post = [ columns opt
             | (Key "builtin:columns" opt)  <- syn
             ]

  foldM (\s p -> p s) source post


runReport :: Maybe Text -> IO ()
runReport name = do

  cfgFile <- readFile confFile
  let dir = takeDirectory confFile

  -- FIXME: error-handling
  conf <- pure (parseTop cfgFile) `orDie` "can't parse config"

  let r = [ insn
          | ListVal @C (Key "fixme-report" (SymbolVal n : SymbolVal "json" : insn)) <- conf
          , n == maybe n Id name
          ] & headDef mempty

  let queries = [ txt e
                | ListVal @C (Key "query" [e])  <- r
                ]

  let flt = case parseFilt queries of
              [] -> Filt @IO ( [] :: [Text] )
              xs -> Filt xs

  let render  = [ rend
                | ListVal @C (Key "render" rend)  <- r
                ] & lastMay

  let postprocess = [ p
                    | ListVal @C (Key "post" p)  <- r
                    ]

  zzz <- case render of
              Just [SymbolVal "builtin:microstache", t] -> do
                tpl <- canonicalizePath (dir </> Text.unpack (txt t))
                compiled <- compileMustacheFile tpl
                pure $ \f -> do
                  let encoded =  toJSON $ Report (fmap (view fixmeDynAttr) f)
                  applyPost postprocess (renderMustache compiled encoded)
                    >>= TIO.putStr

              _ ->  pure $ \f -> do
                      BS.putStr $ encodePretty $ Report $ fmap (view fixmeDynAttr) f

  e <- newFixmeEnv
  runFixmeState e $ do
    fixmies <- loadFixme flt
    liftIO (zzz fixmies)


