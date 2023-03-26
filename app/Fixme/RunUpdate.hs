{-# Language TemplateHaskell #-}
module Fixme.RunUpdate where

import Fixme.Defaults
import Fixme.Types
import Fixme.State
import Fixme.OrDie
import Fixme.Config
import Fixme.Git
import Fixme.Hash
import Data.Config.Suckless

import Codec.Serialise
import Control.Applicative
import Control.Concurrent.Async
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Data.Attoparsec.Text hiding (option,take)
import Data.Attoparsec.Text qualified as Atto
import Data.ByteString.Lazy.Char8 qualified as LBS
import Data.Foldable(for_)
import Data.Function
import Data.IntMap qualified as IntMap
import Data.List qualified as List
import Data.Maybe
import Data.Set qualified as Set
import Data.String
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (ignore)
import Data.Text qualified as Text
import Data.Text (Text)
import Lens.Micro.Platform
import Prettyprinter
import Safe
import System.FilePattern
import System.IO

newtype ScanOpt =
  ScanOpt
  { _scanDontAdd :: Bool
  }

makeLenses 'ScanOpt


runUpdate :: ScanOpt -> IO ()
runUpdate opt = do

  cfgFile <- readFile confFile
  logFileData <- readFile logFile

  -- FIXME: better error handling
  r <- pure (parseTop cfgFile) `orDie` "can't parse config"

  log <- pure (parseTop logFileData) `orDie` "can't parse log"

  e <- newFixmeEnv

  runFixmeState e $ do
    initState

    -- let toText = Text.pack . show . pretty

    let merged = [ (Text.unpack a, Text.unpack b)
                 | ListVal @C (Key "fixme-merged" [LitStrVal a, LitStrVal b]) <- log
                 ]

    let deleted = [ Text.unpack a
                  | ListVal @C (Key "fixme-del" [LitStrVal a]) <- log
                  ]

    let attrs = [ (show $ pretty i, a, v)
                | ListVal @C (Key "fixme-set" [LitStrVal a, LitStrVal v, LitStrVal i]) <- log
                ]

    let allowedName s = s `Set.member` allowedAttribs r

    let allowedVal k v = isAllowedVal k v (allowedValues r)

    let exclude = ignored r

    let fpat (_,f) =  or [ x ?== f | x <- masks r ]
                   && (null exclude || not ( and [ x ?== f | x <- exclude] ))

    blobz <- gitListAllBlobs <&> List.nubBy ((==) `on` fst)
                 >>= filterM ( \(h,_) -> blobProcessed h <&> not)

    let files = blobz & reverse . filter fpat

    let fxdef = FixmeDef (List.nub (comm r))  (List.nub ("FIXME:" : pref r))

    fme <- liftIO $ List.nubBy ((==) `on` view fixmeId) . mconcat
              <$> mapConcurrently (parseBlob fxdef) files

    -- FIXME: remove-tag-len-hardcode-somehow-new

    let o = FmtAttr (fromIntegral (idlen r) ) 8 (tpref r)

    unless ( opt ^. scanDontAdd ) do

      transaction $ do
        for_ files $ \(h,_) -> setProcessed h

        for_ fme $ \f -> do
          liftIO $ print $ pretty (Brief o f)
          putFixme f

    -- FIXME: play only diff for log ?

    -- FIXME: don't play log twice(?)

    -- FIXME: to-play-log-function


      run <- forM attrs $ \(i,a,v) -> do
        ii <- findId i

        let valid = allowedName a && allowedVal a v

        unless (allowedVal a v) do
          liftIO $ hPrint stderr $ "*** error: value" <+> pretty v
                                                      <+> "not allowed for"
                                                      <+> pretty a

        unless (allowedName a) do
          liftIO $ hPrint stderr $ "*** error: attrib name" <+> pretty a
                                                            <+> pretty "not allowed"

        case ii of
           [x] | valid     -> pure [ setAttr Nothing x a v ]
               | otherwise -> pure []
           []  | valid     -> pure [ setAttr Nothing (fromString i) a v ]
               | otherwise -> pure []

           _   -> do
             liftIO $ hPrint stderr $ "fixme-set:"
                                         <+> pretty ii
                                         <+> "is ambiguous, ignored"
             pure mempty

      unless ( opt ^. scanDontAdd ) do
        sequence_ (mconcat run)


      del <- forM deleted $ \i -> do
        ii <- findId i

        case ii of
           [x] -> pure [ setDeleted x ]
           []  -> pure [ setDeleted (fromString i) ]
           _   -> do
             liftIO $ hPrint stderr $ "fixme-del:"
                                         <+> pretty i
                                         <+> "is ambiguous, ignored"
             pure mempty

      unless ( opt ^. scanDontAdd ) do
        sequence_ (mconcat del)

    merges <- forM merged $ \(a,b) -> do
                aId <- findId a
                bId <- findId b

                case (aId, bId) of
                  ( [x], [y] ) -> pure [addMerged x y]
                  ( [], [] )   -> pure [addMerged (fromString a) (fromString b)]
                  _ -> do
                    liftIO $ hPrint stderr $ "fixme-merge"
                                                <+> pretty a
                                                <+> pretty b
                                                <+> "is ambigous, ignored"
                    pure []

    unless ( opt ^. scanDontAdd ) do
      sequence_ (mconcat merges)


parseBlob :: FixmeDef
          -> (GitHash, FilePath)
          -> IO [Fixme]

parseBlob def (gh, fp) = do
  -- FIXME: check if file is too big

  bs <- gitReadObject gh
  let utf8 = decodeUtf8With ignore (LBS.toStrict bs)

  let txt = utf8 & Text.lines
                 & zip [1..]

  let ls = IntMap.fromList txt

  heads' <- forM txt $ \(i,s) -> do
             let h = fixmeHash $ serialise (gh,i)
             let fixme0 = Fixme "" "" h gh fp i 0 mempty mempty
             let fixme = parseOnly (pHeader def fixme0) s
             pure $ either mempty (List.singleton . (i,) . updateId) fixme

  let heads = mconcat heads'
  let hm = IntMap.fromList heads

  forM heads  $ \(i,h) -> do
    let (_, nx) = IntMap.split i ls

    let ss = List.takeWhile ( \(j,_) -> not (IntMap.member j hm) )
                            (IntMap.toList nx)

    let lls = reverse $ nicer
                      $ reverse
                      $ go [] (view fixmeIndent h) (fmap snd ss)

    let r = updateId $ h & set fixmeBody    (view fixmeTitle h : lls)
    pure r

  where

    -- NOTE: constructs id from hash(file,issue-content).
    --       that promises that id's will be more or less
    --       stable and dupes will be only, when content
    --       is duplicated in the same file. It happens,
    --       but it's not a good practice anyway.
    updateId fx = fx & set fixmeId  hash
      where
        body  = view fixmeBody fx
        file  = view fixmeFile fx
        title = view fixmeTitle fx
        tag   = view fixmeTag fx
        bin   = serialise (file,tag,title,body)
        hash  = fixmeHash bin

    nicer [] = []
    nicer (x:xs) | Text.null x = nicer xs
                 | otherwise = x:xs

    go acc  _ [] = acc

    go acc  _ (e1:e2:_) | Text.null e1 && Text.null e2 = acc

    go acc  i (s:rest)  |    calcIndent s > i
                          || startsWithComment s
                          || Text.null s  = go (acc <> [stripAll s]) i rest

                         | otherwise = acc

    stripComment s = headDef s $ catMaybes [ Text.stripPrefix p s | p <- view fixmeComm def  ]

    stripAll s = Text.strip $ stripComment $ Text.strip s

    startsWithComment s = or [ Text.isPrefixOf x ss | x <- view fixmeComm def ]
      where ss = Text.strip s


pHeader :: FixmeDef -> Fixme -> Parser Fixme
pHeader fx fme = do

  spaces <- Atto.takeWhile isHorizontalSpace
  _      <- optional (choice comm)
  _      <- Atto.skipWhile isHorizontalSpace
  tag    <- choice tags
  _      <- Atto.skipWhile isHorizontalSpace
  title' <- Text.pack <$> Atto.manyTill anyChar (endOfInput <|> endOfLine)
  let title = Text.strip title'

  pure $  fme & set fixmeTag     tag
              & set fixmeTitle   title
              & set fixmeIndent  (calcIndent spaces)

  where
    comm = fmap string (view fixmeComm fx)
    tags = fmap string (view fixmeTags fx)


calcIndent :: Text -> Int
calcIndent txt = sum (fmap f s)
  where
    s0 = Text.takeWhile isHorizontalSpace txt
    s = Text.unpack s0

    f c | c == ' '  = 1
        | c == '\t' = 4
        | otherwise = 0

