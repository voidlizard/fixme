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
import Data.Text qualified as Text
import qualified Data.Text.Lazy.IO as TIO
import qualified Data.Text.Lazy as LT
import Lens.Micro.Platform hiding ((.=))
import Safe
import System.Directory
import System.FilePath
import Text.Microstache hiding (Key)
import Data.HashMap.Strict (HashMap)
import System.Exit
import Text.InterpolatedString.Perl6 (qc,q)
import Data.Maybe

newtype Report = Report { items :: [HashMap Text Text] }
                 deriving stock (Generic)

instance ToJSON Report

-- colorize s | LT.isPrefixOf "FIXME" s = LT.fromStrict $ ANSI.red( LT.toStrict s)
--            | LT.isPrefixOf "TODO"  s = LT.fromStrict $ ANSI.yellow( LT.toStrict s)
--            | otherwise = s

colorize :: LT.Text -> LT.Text
colorize = id

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
    fmt sy s = colorize $ case sy of
      SymbolVal "_" -> s
      LitIntVal n   -> LT.take (fromIntegral n) $ LT.justifyLeft (fromIntegral n) ' ' s
      _             -> ""


applyPost :: [[Syntax C]] -> LT.Text -> IO LT.Text
applyPost syn source  = do

  let post = [ columns opt
             | (Key "builtin:columns" opt)  <- syn
             ]

  foldM (\s p -> p s) source post

builtinListBrief :: String
builtinListBrief = [qc|
( fixme-report builtin:list-brief json
  (render builtin:microstache builtin:list-brief)
  (post builtin:columns | 10 10 8 10 _)
)
|]

builtinListFull :: String
builtinListFull = [qc|
( fixme-report builtin:list-full json
  (render builtin:microstache builtin:list-full)
)
|]

builtinListBriefTpl :: LT.Text
builtinListBriefTpl = [q|{{! first line }}
{{#items}}
{{id}}|[{{workflow}}]|{{tag}}|{{assigned}}|{{&title}}
{{/items}}
|]

builtinListFullTpl :: LT.Text
builtinListFullTpl = [q|{{! first line }}
{{#items}}
## {{tag}} {{&title}}

id: {{id}}
file: {{&file}}
blob: {{blob}}

{{&body}}

;;

{{/items}}
|]

runReport :: [Text] -> Maybe (Filt IO) -> IO ()
runReport args mbFilt = do

  let name = headMay args

  cfgFile <- readFile confFile
  let dir = takeDirectory confFile

  -- FIXME: error-handling
  bil <- pure (parseTop builtinListBrief) `orDie` "broken builtin reports"
  bif <- pure (parseTop builtinListFull) `orDie` "broken builtin reports"
  conf <- pure (parseTop cfgFile) `orDie` "can't parse config"  <&> (`mappend` bil)
                                                                <&> (`mappend` bif)
  let r = [ insn
          | ListVal (Key "fixme-report" (SymbolVal n : SymbolVal "json" : insn)) <- conf
          , n == maybe n Id name
          ] & headDef mempty

  let queries = [ txt e
                | ListVal (Key "query" [e])  <- r
                ]

  let flt' = case parseFilt (queries <> tailDef mempty args) of
              [] -> Filt @IO ( [] :: [Text] )
              xs -> Filt xs

  -- case flt' of
  --   Filt zz -> print zz

  let flt = fromMaybe flt' mbFilt

  let render  = [ rend
                | ListVal (Key "render" rend)  <- r
                ] & lastMay

  let postprocess = [ p
                    | ListVal (Key "post" p)  <- r
                    ]

  let withTemplate = \case
        Left err -> die (show err)
        Right compiled -> do
                pure $ \f -> do
                          let encoded =  toJSON $ Report (fmap (view fixmeDynAttr) f)
                          applyPost postprocess (renderMustache compiled encoded)
                            >>= TIO.putStr

  zzz <- case render of

            Just [SymbolVal "builtin:microstache", SymbolVal "builtin:list-brief"] -> do
              let res = compileMustacheText "builtin:list-brief" builtinListBriefTpl
              withTemplate res

            Just [SymbolVal "builtin:microstache", SymbolVal "builtin:list-full"] -> do
              let res = compileMustacheText "builtin:list-full" builtinListFullTpl
              withTemplate res

            Just [SymbolVal "builtin:microstache", t] -> do
              tpl <- canonicalizePath (dir </> Text.unpack (txt t))
              compileMustacheFile tpl >>= withTemplate . Right

            _ ->  pure $ \f -> do
                    BS.putStr $ encodePretty $ Report $ fmap (view fixmeDynAttr) f

  e <- newFixmeEnv
  runFixmeState e $ do
    fixmies <- loadFixme flt
    liftIO (zzz fixmies)


