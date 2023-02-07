module Fixme.Git where

import Codec.Serialise
import Crypto.Hash
import Data.ByteArray qualified as BA
import Data.ByteString.Base16 qualified as B16
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Lazy.Char8 qualified as LBS
import Data.String (IsString(..))
import GHC.Generics
import Prettyprinter
import Text.InterpolatedString.Perl6 (qc)

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

