{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# Language TemplateHaskell #-}
{-# Language AllowAmbiguousTypes #-}
{-# Language UndecidableInstances #-}
module Fixme.State
  ( module Fixme.State
  , transactional
  ) where

import Fixme.Prelude
import Fixme.Git
import Fixme.Types
import Fixme.Hash
import Fixme.Defaults

import DBPipe.SQLite

import Data.Config.Suckless

import Control.Applicative
import Codec.Serialise
import Control.Monad.Reader
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy (ByteString)
import Data.ByteString qualified as BS
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Maybe
import Data.Either
import Data.Set qualified as Set
import Data.Text.Encoding (encodeUtf8)
import Data.Text qualified as Text
import Text.InterpolatedString.Perl6 (qc)
import Data.Coerce
import System.FilePath
import System.Directory hiding (canonicalizePath)
import System.IO (hPrint,stderr,stdout)

import UnliftIO

type FixmePerks m = MonadUnliftIO m

class Monad m => HasFixmeFilter a m where

instance Monad m => HasFixmeFilter () m

data FixmeEnv =
  FixmeEnv
  { _localStateDir :: FilePath
  , _config        :: [Syntax C]
  , _db            :: DBPipeEnv
  }

makeLenses 'FixmeEnv

localLogFile :: SimpleGetter FixmeEnv FilePath
localLogFile = to
  \FixmeEnv{..} -> _localStateDir </> "log"


localConfigFile :: SimpleGetter FixmeEnv FilePath
localConfigFile = to
  \FixmeEnv{..} -> _localStateDir </> "config"

newtype FixmeState m a =
  FixmeState  { fromFixmeState :: ReaderT FixmeEnv m a }
  deriving newtype ( Applicative
                   , Functor
                   , Monad
                   , MonadIO
                   , MonadUnliftIO
                   , MonadTrans
                   , MonadReader FixmeEnv
                   )



newtype LocalConfigFile = LocalConfigFile FilePath
                          deriving newtype ( Eq
                                           , Ord
                                           , Show
                                           , IsString
                                           , Generic
                                           , Monoid
                                           , Semigroup
                                           )

newtype LocalDatabaseFile = LocalDatabaseFile FilePath
                            deriving newtype ( Eq
                                             , Ord
                                             , Show
                                             , IsString
                                             , Generic
                                             , Monoid
                                             , Semigroup
                                             )


newtype LocalStateDir = LocalStateFile FilePath
                        deriving newtype ( Eq
                                         , Ord
                                         , Show
                                         , IsString
                                         , Generic
                                         , Monoid
                                         , Semigroup
                                         )


newFixmeEnvDefault :: FixmePerks m
                   => m FixmeEnv
newFixmeEnvDefault = newFixmeEnv mempty (Just ".fixme/config") Nothing

newFixmeEnv :: FixmePerks m
            => [Syntax C]
            -> Maybe LocalConfigFile
            -> Maybe LocalDatabaseFile
            -> m FixmeEnv

newFixmeEnv conf lconf ldbfile = do

  (localConf, lcp) <- maybe1 lconf (pure mempty) $ \path  -> do

     cpath <- canonicalizePath (coerce path)

     touch cpath

     try @_ @IOException (liftIO (readFile cpath))
               <&> fromRight mempty
               <&> parseTop
               <&> fromRight mempty
               <&> (,takeDirectory cpath)

  xdg <- liftIO (getXdgDirectory XdgConfig "fixme")
             <&> (</> "config")

  userConf <- try @_ @IOException (liftIO (readFile xdg))
                 <&> fromRight mempty
                 <&> parseTop
                 <&> fromRight mempty

  let theConf = localConf <> userConf <> conf

  let dbFrom lc = lc <&> \s -> takeDirectory (coerce s) </> "state.db"

  let db = (coerce ldbfile <|> dbFrom lconf)
              & fromMaybe ".fixme/state.db"

  let dbOpts = dbPipeOptsDef
  FixmeEnv lcp theConf <$>  newDBPipeEnv dbOpts db


runFixmeState :: FixmePerks m => FixmeEnv -> FixmeState m a -> m a
runFixmeState env m = runReaderT ( fromFixmeState m ) env

withFixme :: FixmePerks m => FixmeEnv -> FixmeState m a -> m a
withFixme = runFixmeState

withState :: (MonadUnliftIO m, MonadReader FixmeEnv m) => DBPipeM m a -> m a
withState m = do
  database <- asks _db
  withDB database m

initState :: MonadIO m => DBPipeM m ()
initState = do
    ddl [qc| create table if not exists blob
              ( hash text not null primary key
              )
           |]

    ddl [qc| create table if not exists fixme
             ( id text not null primary key
             , fixme blob not null
             )
            |]

    ddl [qc| create table if not exists merged
             ( a text not null
             , b text not null
             , primary key (a, b)
             )
           |]

    ddl [qc| create table if not exists deleted
             ( id text not null
             , primary key (id)
             )
           |]

    ddl [qc| create table if not exists fixmeattr
             ( id text not null references fixme(id)
             , attr text not null
             , value text
             , primary key (id, attr)
             )
           |]

    ddl [qc| create table if not exists fixmeattrlog
             ( rev text not null
             , id text not null references fixme(id)
             , attr text not null
             , value text
             , primary key (rev, id, attr)
             )
           |]

    ddl [qc| create table if not exists logprocessed
             ( hash text not null primary key
             )
           |]

    ddl [qc| create table if not exists stateprocessed
             ( hash text not null primary key
             )
           |]


allProcessed :: FixmePerks m => FixmeState m [GitHash]
allProcessed = withState do
  let sql = [qc|
  select hash from blob
  |]

  select_ sql <&> fmap fromOnly


blobProcessed :: FixmePerks m => GitHash -> FixmeState m Bool
blobProcessed h = withState do
  let sql = [qc|
    select null from blob where hash = ? limit 1
  |]
  select @(Only (Maybe Int)) sql (Only h)
    <&> isJust . listToMaybe . fmap fromOnly

setStateProcessed ::FixmePerks m => FixmeHash -> FixmeState m ()
setStateProcessed h = withState do

  let sql = [qc|
   insert into stateprocessed (hash) values(?)
   on conflict (hash) do nothing
  |]

  insert sql (Only h)

stateProcessed :: FixmePerks m => FixmeHash -> FixmeState m Bool
stateProcessed h = withState do

  let sql = [qc|
  select null from stateprocessed where hash = ? limit 1
  |]

  select @(Only (Maybe Int)) sql (Only h) <&> not . null


setLogProcessed :: FixmePerks m => GitHash -> FixmeState m ()
setLogProcessed h = withState do
  let sql = [qc|
    insert into logprocessed (hash) values(?)
    on conflict (hash) do nothing
  |]
  insert sql (Only h)

logProcessed :: FixmePerks m => GitHash -> FixmeState m Bool
logProcessed h = withState do
  let sql = [qc|
    select null from logprocessed where hash = ? limit 1
  |]
  select @(Only (Maybe Int)) sql (Only h) <&> not . null

setProcessed :: FixmePerks m => GitHash -> FixmeState m ()
setProcessed h = withState do
  here <- lift $ blobProcessed h
  unless here do
    let sql = [qc|
      insert into blob (hash) values(?)
    |]
    insert sql (Only h)

-- from here

findId :: FixmePerks m => String -> FixmeState m [FixmeHash]
findId s = withState do
  let sql = [qc|
    select id from fixme where id like ?
  |]
  select sql (Only (s <> "%")) <&> fmap fromOnly

existsFixme :: FixmePerks m => Fixme -> FixmeState m Bool
existsFixme fxm = withState do
  let h = fxm ^. fixmeId
  let sql = [qc|
    select null from fixme where id = ? limit 1
  |]
  select @(Only (Maybe Int)) sql (Only h) <&> not . null

addMerged :: FixmePerks m => FixmeHash -> FixmeHash -> FixmeState m ()
addMerged a b = withState do
  let sql = [qc|
    insert into merged (a, b) values (?, ?) on conflict (a, b) do nothing
  |]
  insert sql (a, b)

setDeleted :: FixmePerks m => FixmeHash -> FixmeState m ()
setDeleted fid = withState do
  let sql = [qc|
    insert into deleted (id) values (?) on conflict (id) do nothing
  |]
  insert sql (Only fid)

setAttr :: FixmePerks m => Pretty a => Maybe GitHash -> FixmeHash -> Text -> a -> FixmeState m ()
setAttr gh' h t v = withState do
  insert [qc|
    insert into fixmeattr (id, attr, value) values (?, ?, ?) on conflict (id, attr) do update set value = ?
  |] (show (pretty h), t, show (pretty v), show (pretty v))
  maybe1 gh' (pure ()) $ \gh ->
    insert [qc|
      insert into fixmeattrlog (rev, id, attr, value) values (?, ?, ?, ?) on conflict (id, attr) do update set value = ?
    |] (show (pretty gh), show (pretty h), t, show (pretty v), show (pretty v))

putFixme :: FixmePerks m => Fixme -> FixmeState m ()
putFixme fxm' = do
  let bs = serialise fxm
  here <- existsFixme fxm
  unless here do
    let i = fxm ^. fixmeId
    withState do
      insert [qc|
        insert into fixme (id, fixme) values (?, ?) on conflict (id) do nothing
      |] (i, bs)
    setAttr Nothing i "tag"   (fxm ^. fixmeTag)
    setAttr Nothing i "title" (fxm ^. fixmeTitle)
  where
    fxm = set fixmeDynAttr mempty fxm'

listFixme :: FixmePerks m => FixmeState m [FixmeHash]
listFixme = withState do
  let sql = [qc|
    select id from fixme
  |]
  select_ sql <&> fmap fromOnly

getFixme :: FixmePerks m => FixmeHash -> FixmeState m (Maybe Fixme)
getFixme h = withState do
  let sql = [qc|
    select fixme from fixme where id = ? limit 1
  |]
  select sql (Only h)
    <&> listToMaybe
    <&> fmap fromOnly
    >>= maybe (pure Nothing) (pure . either (const Nothing) Just . deserialiseOrFail @Fixme)

makeFixme :: (ByteString, Text) -> Maybe Fixme
makeFixme (s1, s2') =  fxm <&> over fixmeDynAttr insId
                             . over fixmeDynAttr insBody
                             . over fixmeDynAttr insFile
                             . over fixmeDynAttr insHash
                             . set  fixmeDynAttr attr
  where
    -- s1 = BS.fromStrict s1'
    fxm  = either (const Nothing) Just $ deserialiseOrFail s1 :: Maybe Fixme
    s2 = BS.fromStrict $ encodeUtf8 s2'
    attr = fromMaybe mempty (Aeson.decode s2) :: HashMap Text Text

    insId = case fxm of
      Just e  -> HashMap.insert "id" (fromString $ show $ pretty $ view fixmeId e)
      Nothing -> id

    insBody = case fxm of
      Just e  -> HashMap.insert "body" (Text.unlines $ view fixmeBody e)
      Nothing -> id

    insFile = case fxm of
      Just e  -> HashMap.insert "file" (fromString $ view fixmeFile e)
      Nothing -> id

    insHash = case fxm of
      Just e  -> HashMap.insert "blob" (txt $ view fixmeFileGitHash e)
      Nothing -> id

class LoadFixme q   where
  loadFixme :: forall m . FixmePerks m => q -> FixmeState m [Fixme]

data Filt m = forall a . (Show a, Pretty a, LoadFixme a) => Filt a

instance LoadFixme (Filt m) where
  loadFixme (Filt a) = loadFixme a

instance LoadFixme [Text] where
  loadFixme flt = do

    let vals = Text.intercalate "," [ "(?)" | _ <- cnd ]

    let
      sql :: String
      sql = [qc|

       with vals(v) as (values {vals})
       select  fixme as fxm
             , ( select json_group_object(a.attr,a.value)
                 from fixmeattr a where a.id = f.id) as attr

        from fixme f
       where
          not exists (select null from merged m where m.a = f.id)
          and not exists (select null from deleted d where d.id = f.id)
          and  (
            exists ( select null from fixmeattr a
                     where a.id = f.id
                       and (
                            (  exists (select null from vals where a.value like v ) )
                         or (  exists (select null from vals where a.id like v ) )
                        )
                   )
          )
    |]

    withState $ select sql cnd <&> mapMaybe makeFixme

    where

      cnd'' = flt

      cnd' | null cnd'' = [""]
           | otherwise = cnd''

      cnd = fmap (<> "%") cnd'

-- TODO: better query DSL processing

instance LoadFixme [(Text,Text)] where
  loadFixme [] = pure mempty

  loadFixme flt' | null flt = loadFixme @[Text] mempty <&> invFilt

                 | otherwise = do


    let vals = Text.intercalate "," [ "(?,?)" | _ <- flt ]

    let sql :: String
        sql = [qc|

      with vals(a,v) as (values {vals})
      select f.fixme
           , ( select json_group_object(a.attr,a.value)
               from fixmeattr a where a.id = f.id) as attr

      from fixme f
             join fixmeattr a on a.id = f.id
             join vals v on v.a = a.attr and v.v = a.value
       where
        not exists (select null from merged m where m.a = f.id )
        and not exists (select null from deleted d where d.id = f.id )

      group by f.id
    |]


    let unflt = mconcat [ [stripDsl k,v] | (k,v) <- flt', not (Text.isPrefixOf "~" k) ]

    let q = HashMap.fromList [ (stripDsl k,v) | (k,v) <- flt', not (Text.isPrefixOf "?" k) ]

    withState do
      select sql unflt <&> mapMaybe makeFixme
                       <&> filter (HashMap.isSubmapOf q . view fixmeDynAttr)
                       <&> invFilt

    where

      invFilt fme = filter (\x -> Set.null $ Set.fromList (HashMap.toList (view fixmeDynAttr x)) `Set.intersection` nq) fme

      nq = Set.fromList [ (stripDsl k,v) | (k,v) <- flt', Text.isPrefixOf "~" k ]

      flt = [ (stripDsl k, v) | (k,v) <- flt', not (Text.isPrefixOf "~" k) ]
      -- flt = [ (stripDsl k, v) | (k,v) <- flt' ]

      stripDsl = Text.dropWhile (`elem` "~? \t")

listAttrs :: FixmePerks m => FixmeState m [Text]
listAttrs = withState do
  let
    sql :: String
    sql = [qc|
    select distinct(attr) from fixmeattr;
  |]
  select_ sql <&> fmap fromOnly

instance ToField GitHash where
  toField h = toField (show $ pretty h)

instance ToField FixmeHash where
  toField h = toField (show $ pretty h)

instance FromField FixmeHash where
  fromField = fmap fromString . fromField @String

instance FromField GitHash where
  fromField = fmap fromString . fromField @String


