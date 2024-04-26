{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# Language PatternSynonyms #-}
{-# Language TemplateHaskell #-}
module Main where

import Fixme.Prelude
import Fixme.Defaults
import Fixme.Git
import Fixme.Hash
import Fixme.State
import Fixme.Types
import Fixme.RunListAttribs
import Fixme.RunReport
import Fixme.LocalConfig
import Fixme.RunLog
import Fixme.RunLogMacro
import Fixme.RunUpdate
import Paths_fixme (version)

import Data.Config.Suckless

import Control.Monad
import Data.ByteString.Lazy.Char8 qualified as LBS
import Data.Generics.Uniplate.Data()
import Data.Maybe
import Data.Text.IO qualified as Text
import Data.UUID.V4 as UUID
import Data.Version (showVersion)
import Lens.Micro.Platform
import Options.Applicative hiding (Parser)
import Options.Applicative qualified as O
import System.Directory
import System.Exit
import System.Process.Typed
import Text.InterpolatedString.Perl6 (qc)


-- TODO: check-haskell-comment-still-works

-- FIXME: no-haskell-commits-dont-work


data ListOpts =
  ListOpt
  { _listShowFull  :: Bool
  , _listFilters   :: [Text]
  , _listFiltExact :: Bool
  }

makeLenses 'ListOpt

runInit  :: (FixmePerks m, m ~ IO) => m ()
runInit = liftIO do

  print $ pretty "init" <+> pretty confDir

  path <- getLocalConfigPath

  createDirectoryIfMissing True confDir
  confExists <- doesFileExist confFile
  logExists <- doesFileExist logFile

  unless confExists do
    Text.writeFile confFile defConfig

  unless logExists do
    Text.writeFile logFile defLog
    env <- newFixmeEnv path

    runFixmeState env do
      liftIO $ print $ pretty "init db"
      withState initState


runUuid :: IO ()
runUuid = do
  uuid <- UUID.nextRandom
  print $ pretty "uuid:" <+> pretty (show uuid)


runList :: FixmePerks m => ListOpts -> m ()
runList opt = do

  path <- getLocalConfigPath

  e <- newFixmeEnv path

  runFixmeState e do

    cfgFile <- liftIO $ readFile confFile

    r <- pure (parseTop cfgFile) `orDie` "can't parse config"

    filt <- if view listFiltExact  opt then do
              let ss = opt ^. listFilters
              let flt = parseFilt ss
              pure $ Filt flt
            else
              pure $ Filt $ opt ^. listFilters

    let brief = ["builtin:list-brief"]
    let full  = ["builtin:list-full"]

    let report = if view listShowFull opt then full else brief

    runReport report (Just filt)


newtype HighlightFixmeBegin = HighlightFixmeBegin Int

instance PagerFeatures HighlightFixmeBegin where
  pagerHighlightRow (HighlightFixmeBegin x) = Just x

runCat :: FixmePerks m => FixmeHash -> Maybe Int -> Maybe Int -> m ()
runCat h mbefore mafter = do

  path <- getLocalConfigPath

  e <- newFixmeEnv path

  cfg <- getLocalConfig
  ctx <- getDefaultContext cfg

  runFixmeState e $ do
    ii <- findId (show $ pretty h) <&> listToMaybe
    i <- pure ii  `orDie` show ("fixme not found" <+> pretty h)
    fxm <- getFixme i `orDie` show ("fixme not found" <+> pretty h)

    let bef   = fromMaybe 0 (mbefore <|> fmap fst ctx)
    let aft   = fromMaybe 0 (mafter  <|> fmap snd ctx)
    let self  = length ( fxm ^. fixmeBody )
    let num   = bef + self + aft
    let from  = max 0 ( fxm ^. fixmeLine - bef - 1)


    -- FIXME: check of file is really big
    --   use streaming instead of LBS(?)
    o <- gitReadObject (view fixmeFileGitHash fxm)

    let feat = HighlightFixmeBegin (min (fxm ^. fixmeLine) (bef+1))

    let ls = take num $ drop from $ LBS.lines o

    pager <- getPager feat fxm cfg

    -- FIXME: use-pager-with-colors!
    --   or temporaty file?
    maybe1 pager (liftIO $ for_ ls LBS.putStrLn)
     \pgr -> liftIO do
        -- print $ pretty pgr
        -- exitFailure
        let input = byteStringInput (LBS.unlines ls)
        let cmd = setStdin input $ setStderr closed
                                 $ shell [qc|{pgr}|]
        code <- runProcess cmd

        unless ( code == ExitSuccess ) do
          liftIO $ for_ ls LBS.putStrLn




main :: IO ()
main = join . customExecParser (prefs showHelpOnError) $
  info (helper <*> versionOption <*> parser)
  (  fullDesc
  <> header ("fixme " <> ver)
  <> progDesc "trackerless issue management"
  )
  where
    ver = "v" <> showVersion version

    versionOption :: O.Parser (a -> a)
    versionOption = infoOption ver (long "version" <> short 'v' <> help "Show version" <> hidden)

    parser ::  O.Parser (IO ())
    parser = hsubparser (  command "init"    (info pInit   (progDesc "init fixme config"))
                        <> command "update"  (info pUpdate (progDesc "update state"))
                        <> command "list"    (info pList   (progDesc "list"))
                        <> command "report"  (info pReport (progDesc "run report"))
                        <> command "cat"     (info pCat    (progDesc "cat a fixme from git object"))
                        <> command "uuid"    (info pUuid   (progDesc "generate uuid"))
                        <> command "set"     (info pSet    (progDesc "set attribute value for a fixmie"))
                        <> command "del"     (info pDel    (progDesc "mark a fixme deleted"))
                        <> command "merge"   (info pMerge  (progDesc "mark a fixme merged"))
                        <> command "meta"    (info (hsubparser pMeta) (progDesc "metadata commands") )
                        <> command "scan"    (info pUpdate (progDesc "obsolete; use update"))
                        )
              <|> pLogMacro

    pInit = do
      pure runInit

    pUpdateOpts = do
      ScanOpt <$> flag False True ( long "dry" <> short 'n' <> help "dry run" )

    pUpdate = runUpdate <$> pUpdateOpts

    pUuid = pure runUuid

    pListOpts = do
      full <- flag False True ( long "full" <> short 'f' <> help "show full fixme" )
      exact <- flag False True ( long "query" <> short 'q' <> help "query by attribute" )
      filt <- many $ strArgument  ( metavar "FILTER" )
      pure $ ListOpt full filt exact

    pList = runList <$> pListOpts

    pSetOpts = do
      attr <- strArgument ( metavar "ATTRIBUTE" )
      val  <- strArgument ( metavar "VALUE")
      fx  <- strArgument ( metavar "ID" )
      dry <- flag False True ( long "dry" <> short 'n' <> help "dry run" )
      pure (dry, [qc|fixme-set {attr} {val} {fx}|])

    pSet = do
      uncurry runLog <$> pSetOpts

    pDel = do
      fx  <- strArgument ( metavar "ID" )
      dry <- flag False True ( long "dry" <> short 'n' <> help "dry run" )
      pure $ runLog dry [qc|fixme-del {fx}|]

    pMerge = do
      a  <- strArgument ( metavar "FROM" )
      b  <- strArgument ( metavar "TO" )
      dry <- flag False True ( long "dry" <> short 'n' <> help "dry run" )
      pure $ runLog dry [qc|fixme-merged {a} {b}|]


    pCat = do
      fxid  <- strArgument ( metavar "ID" )
      b     <- optional $ option auto ( long "before" <> short 'B' <> help "lines before"  )
      a     <- optional $ option auto ( long "after" <> short 'A' <> help "lines after"  )
      pure $ runCat fxid b a


    pMeta =    command "attribs" (info pMetaAttr  (progDesc "list attributes"))
            <> command "config" (info pConf (progDesc "dumps config"))


    pMetaAttr = do
      pure runListAttribs

    pConf = pure do

      cfgFile <- readFile confFile

      -- FIXME: better error handling
      r <- pure (parseTop cfgFile) `orDie` "can't parse config"

      print $ vcat (fmap pretty r)

    pReport = do
      args <- many $ strArgument ( metavar "REPORT-NAME" )
      pure do
        conf <- getLocalConfigPath
        env <- newFixmeEnv conf
        runFixmeState env $ runReport args Nothing

    pLogMacro = do
      args <- some $ strArgument ( metavar "MACRO-ARGS" )
      pure $ runLogMacro args

