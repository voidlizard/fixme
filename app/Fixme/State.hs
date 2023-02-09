{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# Language TemplateHaskell #-}
module Fixme.State where

import Fixme.Git
import Fixme.Types
import Fixme.Hash
import Fixme.Defaults

import Codec.Serialise
import Control.Monad.Reader
import Database.SQLite.Simple
import Database.SQLite.Simple.ToField
import Database.SQLite.Simple.FromField
import Data.ByteString.Lazy (ByteString)
import Data.Maybe
import Lens.Micro.Platform
import Data.String
import Prettyprinter
import Text.InterpolatedString.Perl6 (qc)
import Data.Text (Text)
import Data.Text qualified as Text


import Debug.Trace

class Monad m => HasFixmeFilter a m where

instance Monad m => HasFixmeFilter () m

newtype FixmeEnv =
  FixmeEnv
  { _fixmeEnvDb :: Connection
  }

makeLenses 'FixmeEnv

newtype FixmeState m a =
  FixmeState  { fromFixmeState :: ReaderT FixmeEnv m a }
  deriving newtype ( Applicative
                   , Functor
                   , Monad
                   , MonadIO
                   , MonadTrans
                   , MonadReader FixmeEnv
                   )


newFixmeEnv :: IO FixmeEnv
newFixmeEnv = do
  conn <- liftIO $ open stateFile
  pure $ FixmeEnv conn

withConn :: MonadIO m => FixmeState m a -> FixmeState m a
withConn m = do
  co <- asks (view fixmeEnvDb)
  r <- m
  liftIO $ close co
  pure r

runFixmeState :: MonadIO m => FixmeEnv -> FixmeState m a -> m a
runFixmeState env m = runReaderT ( fromFixmeState m ) env


initState :: MonadIO m => FixmeState m ()
initState = do
  conn <- asks (view fixmeEnvDb)
  liftIO $ do
    execute_ conn [qc| create table if not exists
                       blob ( hash text not null primary key
                            )
                     |]

    execute_ conn [qc| create table if not exists
                       fixme ( id text not null primary key
                             , fixme blob not null
                             )
                     |]

    execute_ conn [qc| create table if not exists
                       merged ( a text not null
                              , b text not null
                              , primary key (a,b)
                              )
                     |]

    execute_ conn [qc|create table if not exists
                      fixmeattr ( id text not null references fixme(id)
                                , attr text not null
                                , value text
                                , primary key (id,attr) );
                     |]


transaction :: forall m a . MonadIO m => FixmeState IO a -> FixmeState m a
transaction m = do
  conn <- asks (view fixmeEnvDb)
  env <- ask
  liftIO $ withTransaction conn (runFixmeState env m)

allProcessed :: MonadIO m => FixmeState m [GitHash]
allProcessed = do
  conn <- asks (view fixmeEnvDb)

  let sql = [qc|
  select hash from blob
  |]

  liftIO $ query_ conn sql <&> fmap fromOnly

  -- pure $ fmap fromString str

blobProcessed :: MonadIO m => GitHash -> FixmeState m Bool
blobProcessed h = do
  conn <- asks (view fixmeEnvDb)

  let sql = [qc|
  select null from blob where hash = ? limit 1
  |]

  liftIO $ query @_ @[Maybe Bool] conn sql (Only h) <&> not . null

setProcessed :: MonadIO m => GitHash -> FixmeState m ()
setProcessed h = do
  conn <- asks (view fixmeEnvDb)

  here <- blobProcessed h

  unless here do

    let sql = [qc|
     insert into blob (hash) values(?)
    |]

    liftIO $ execute conn sql (Only h)


findId :: MonadIO m => String -> FixmeState m [FixmeHash]
findId s = do
  conn <- asks (view fixmeEnvDb)

  let sql = [qc|
  select id from fixme where id like ?
  |]

  liftIO $ query conn sql (Only (s<>"%")) <&> fmap fromOnly

existsFixme :: MonadIO m => Fixme -> FixmeState m Bool
existsFixme fxm = do
  conn <- asks (view fixmeEnvDb)

  let h = fxm ^. fixmeId

  let sql = [qc|
  select null from fixme where id = ? limit 1
  |]

  liftIO $ query @_ @[Maybe Bool] conn sql (Only h) <&> not . null

addMerged :: MonadIO m => FixmeHash -> FixmeHash -> FixmeState m ()
addMerged a b = do
  conn <- asks (view fixmeEnvDb)

  let sql = [qc|
  insert into merged (a,b) values (?,?) on conflict (a,b) do nothing
  |]

  liftIO $ execute  conn sql (a,b)


setAttr ::  MonadIO m  => Pretty a => FixmeHash -> Text -> a -> FixmeState m ()
setAttr h t v = do
  conn <- asks (view fixmeEnvDb)
  liftIO $ execute conn [qc| insert into fixmeattr (id,attr,value)
                             values (?,?,?) on conflict (id,attr) do update set value = ?
                           |] ( show (pretty h), t, show (pretty v), show (pretty v))

putFixme :: MonadIO m => Fixme -> FixmeState m ()
putFixme fxm = do
  let bs = serialise fxm

  conn <- asks (view fixmeEnvDb)

  here <- existsFixme fxm

  unless here do

    let i = fxm ^. fixmeId

    let sql = [qc|
     insert into fixme (id,fixme) values(?,?) on conflict (id) do nothing
    |]

    liftIO $ execute conn sql (i,bs)

    setAttr (fxm ^. fixmeId) "tag"   (fxm ^. fixmeTag)
    setAttr (fxm ^. fixmeId) "title" (fxm ^. fixmeTitle)


listFixme :: MonadIO m => FixmeState m [FixmeHash]
listFixme = do
  conn <- asks (view fixmeEnvDb)

  let sql = [qc|
   select id,1 from fixme
  |]

  z <- liftIO $ query_  @(FixmeHash, Int) conn sql
  pure $ fmap fst z

getFixme :: MonadIO m => FixmeHash -> FixmeState m (Maybe Fixme)
getFixme h = do

  conn <- asks (view fixmeEnvDb)

  let sql = [qc|
   select fixme from fixme where id = ? limit 1
  |]

  bs <- liftIO $ query @_ @(Only ByteString) conn sql (Only h) <&> fmap fromOnly . listToMaybe

  let fxm = bs >>= either (const  Nothing) Just . deserialiseOrFail @Fixme

  pure fxm

-- FIXME: play-log-on-scan
--   Проигрывать логи при сканировании, складывать в кэш
--   При загрузке использовать фильтры
--
loadFixme :: forall m . (MonadIO m) => [Text] -> FixmeState m [Fixme]
loadFixme cnd'' = do

  conn <- asks (view fixmeEnvDb)

  let vals = Text.intercalate "," [ "(?)" | _ <- cnd ]

  let sql = [qc|
   with vals(v) as (values {vals})
   select fixme from fixme f
   where not exists (select null from merged m where m.a = f.id)
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

  let fxm bs = either (const  Nothing) Just $ deserialiseOrFail @Fixme bs
  liftIO $ query conn sql cnd <&> fmap fromOnly <&> mapMaybe fxm

  where

    cnd' | null cnd'' = [""]
         | otherwise = cnd''

    cnd = fmap (<> "%") cnd'


instance ToField GitHash where
  toField h = toField (show $ pretty h)

instance ToField FixmeHash where
  toField h = toField (show $ pretty h)

instance FromField FixmeHash where
  fromField = fmap fromString . fromField @String

instance FromField GitHash where
  fromField = fmap fromString . fromField @String


