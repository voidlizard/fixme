module Fixme.Defaults where

import Data.Text (Text)
import System.FilePath
import Text.InterpolatedString.Perl6 (qc)

confDir :: FilePath
confDir = ".fixme"

confFile :: FilePath
confFile = confDir </> "config"


defConfig :: Text
defConfig = [qc|

fixme-comments   // # ; --

fixme-prefix     FIXME:     bugs issues

|]

