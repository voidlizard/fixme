{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# Language TemplateHaskell #-}
{-# Language AllowAmbiguousTypes #-}
module Fixme.State where

import Fixme.Git
import Fixme.Types
import Fixme.Hash
import Fixme.Defaults
import Fixme.Prelude

import Data.Foldable(for_)
import Codec.Serialise
import Control.Monad.Reader
import Data.Aeson qualified as Aeson
import Database.SQLite.Simple
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.ToField
import Data.ByteString.Lazy (ByteString)
import Data.ByteString qualified as BS
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Maybe
import Data.String
import Data.Set qualified as Set
import Data.Text.Encoding (encodeUtf8)
import Data.Text qualified as Text
import Data.Text (Text)
import Lens.Micro.Platform
import Prettyprinter
import Text.InterpolatedString.Perl6 (qc)


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

withState :: IO a -> IO a
withState m = do
  e <- newFixmeEnv
  runFixmeState e initState
  m

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

    execute_ conn [qc| create table if not exists
                       deleted ( id text not null
                               , primary key (id)
                               )
                     |]

    execute_ conn [qc|create table if not exists
                      fixmeattr ( id text not null references fixme(id)
                                , attr text not null
                                , value text
                                , primary key (id,attr) );
                     |]

    execute_ conn [qc|create table if not exists
                      fixmeattrlog ( rev text not null
                                   , id text not null references fixme(id)
                                   , attr text not null
                                   , value text
                                   , primary key (rev,id,attr) );
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

setDeleted :: MonadIO m => FixmeHash -> FixmeState m ()
setDeleted fid = do
  conn <- asks (view fixmeEnvDb)

  let sql = [qc|
  insert into deleted (id) values (?) on conflict (id) do nothing
  |]

  liftIO $ execute  conn sql (Only fid)


setAttr ::  MonadIO m  => Pretty a => Maybe GitHash -> FixmeHash -> Text -> a -> FixmeState m ()
setAttr gh' h t v = do
  conn <- asks (view fixmeEnvDb)
  liftIO $ execute conn [qc| insert into fixmeattr (id,attr,value)
                             values (?,?,?) on conflict (id,attr) do update set value = ?
                           |] ( show (pretty h), t, show (pretty v), show (pretty v))


  maybe1 gh' (pure ()) $ \gh -> do

    liftIO $ execute conn [qc| insert into fixmeattrlog (rev,id,attr,value)
                               values (?,?,?,?) on conflict (id,attr) do update set value = ?
                             |] ( show (pretty gh)
                                , show (pretty h)
                                , t
                                , show (pretty v)
                                , show (pretty v)
                                )

    pure ()

putFixme :: MonadIO m => Fixme -> FixmeState m ()
putFixme fxm' = do
  let bs = serialise fxm

  conn <- asks (view fixmeEnvDb)

  here <- existsFixme fxm

  unless here do

    let i = fxm ^. fixmeId

    let sql = [qc|
     insert into fixme (id,fixme) values(?,?) on conflict (id) do nothing
    |]

    liftIO $ execute conn sql (i,bs)

    setAttr Nothing (fxm ^. fixmeId) "tag"   (fxm ^. fixmeTag)
    setAttr Nothing (fxm ^. fixmeId) "title" (fxm ^. fixmeTitle)

  where
    fxm = set fixmeDynAttr mempty fxm'

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

class MonadIO m => LoadFixme q m  where
  loadFixme :: q -> FixmeState m [Fixme]

data Filt m = forall a . (Show a, Pretty a, LoadFixme a m) => Filt a

instance MonadIO m => LoadFixme (Filt m) m where
  loadFixme (Filt a) = loadFixme a

instance MonadIO m => LoadFixme [Text] m where
  loadFixme flt = do

    conn <- asks (view fixmeEnvDb)

    let vals = Text.intercalate "," [ "(?)" | _ <- cnd ]

    let sql = [qc|

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

    -- let fxm bs = either (const  Nothing) Just $ deserialiseOrFail @Fixme bs
    liftIO $ query conn sql cnd <&> mapMaybe makeFixme

    where

      cnd'' = flt

      cnd' | null cnd'' = [""]
           | otherwise = cnd''

      cnd = fmap (<> "%") cnd'

-- TODO: better query DSL processing

instance MonadIO m => LoadFixme [(Text,Text)] m where
  loadFixme [] = pure mempty

  loadFixme flt' | null flt = loadFixme @[Text] mempty <&> invFilt

                 | otherwise = do

    conn <- asks (view fixmeEnvDb)

    let vals = Text.intercalate "," [ "(?,?)" | _ <- flt ]

    let sql = [qc|

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

    liftIO $ query conn sql unflt <&> mapMaybe makeFixme
                                  <&> filter (HashMap.isSubmapOf q . view fixmeDynAttr)
                                  <&> invFilt

    where

      invFilt fme = filter (\x -> Set.null $ Set.fromList (HashMap.toList (view fixmeDynAttr x)) `Set.intersection` nq) fme

      nq = Set.fromList [ (stripDsl k,v) | (k,v) <- flt', Text.isPrefixOf "~" k ]

      flt = [ (stripDsl k, v) | (k,v) <- flt', not (Text.isPrefixOf "~" k) ]
      -- flt = [ (stripDsl k, v) | (k,v) <- flt' ]

      stripDsl = Text.dropWhile (\c -> c `elem` "~? \t")

listAttrs :: MonadIO m => FixmeState m [Text]
listAttrs = do
  conn <- asks (view fixmeEnvDb)

  let sql = [qc|
    select distinct(attr) from fixmeattr;
  |]

  liftIO $ query_  conn sql <&> fmap fromOnly

instance ToField GitHash where
  toField h = toField (show $ pretty h)

instance ToField FixmeHash where
  toField h = toField (show $ pretty h)

instance FromField FixmeHash where
  fromField = fmap fromString . fromField @String

instance FromField GitHash where
  fromField = fmap fromString . fromField @String


