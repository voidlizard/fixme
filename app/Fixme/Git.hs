module Fixme.Git where

import Control.Monad
import Codec.Serialise
import Data.Function
import Control.Monad.IO.Class
import Crypto.Hash
import Data.ByteArray qualified as BA
import Data.ByteString.Base16 qualified as B16
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Lazy.Char8 qualified as LBS
import Data.String (IsString(..))
import GHC.Generics
import Prettyprinter
import System.Process.Typed
import Text.InterpolatedString.Perl6 (qc)
import Data.Text.Encoding qualified as Enc
import Data.Text.Encoding.Error (ignore)
import Data.Text qualified as Text
import Lens.Micro.Platform
import Safe
import Data.Maybe
import Data.List (sortOn)

newtype GitBlob a = Blob a

newtype GitHash =
  GitHash ByteString
  deriving stock (Eq,Ord,Show,Generic)

instance Serialise GitHash

instance Pretty GitHash where
  pretty (GitHash s) = pretty @String [qc|{B16.encode s}|]

instance IsString GitHash where
  fromString s = GitHash (B16.decodeLenient (BS.pack s))

class HasGitHash a where
  gitHash :: a -> GitHash

instance HasGitHash (GitBlob LBS.ByteString) where
  gitHash (Blob s) = GitHash $ BA.convert digest
    where
      hd = LBS.pack $ "blob" <> " " <> show (LBS.length s) <> "\x0"
      digest = hashlazy (hd <> s) :: Digest SHA1


-- FIXME: check-return-code
--   (uuid e0aed358-6757-4054-803c-3cd8066fd7cd)
--
gitListAllBlobs :: MonadIO m => m [(GitHash, FilePath)]
gitListAllBlobs = do
  let cmd = [qc|git rev-list --objects  --all --in-commit-order --filter=object:type=blob|]
  let procCfg = setStdin closed $ setStderr closed (shell cmd)
  (_, out, _) <- readProcess procCfg

  pure $ LBS.lines out & foldMap (fromLine . LBS.words)


  where
    fromLine = \case
      [ha, fname] -> [(fromString (LBS.unpack ha), asUtf8 fname)]
      _           -> []


    asUtf8 x =  Text.unpack (Enc.decodeUtf8With ignore (LBS.toStrict x))


-- FIXME: check-return-code
--   (uuid 3dacb893-6694-4644-ad58-93372a286351)

gitReadObject :: MonadIO m => GitHash -> m LBS.ByteString
gitReadObject h = do
  readProcess (shell [qc|git cat-file blob {pretty h}|]) <&> view _2

gitReadFileFrom :: MonadIO m => GitHash -> FilePath -> m LBS.ByteString
gitReadFileFrom co fp = do
  readProcess (shell [qc|git show {pretty co}:{fp}|]) <&> view _2

getGitCommitsForFile :: MonadIO m => FilePath -> m [(Integer, GitHash)]
getGitCommitsForFile fp = do
  let cmd = shell [qc|git log --all --follow --pretty=format:"%ad %H" --date=unix -- {fp}|]
  (_,output,_) <- readProcess cmd
  pure $ sortOn fst $
    flip mapMaybe (LBS.words <$> LBS.lines output) $ \case
          [t,co] -> (,) <$> readMay (LBS.unpack t)
                  <*> pure (fromString (LBS.unpack co))
          _ -> Nothing


