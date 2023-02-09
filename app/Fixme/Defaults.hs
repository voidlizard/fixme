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

fixme-comments   // #  --

fixme-prefix     FIXME:     bugs issues   ; defines a fixme entity and it's categories

; fixme-files         ; sets file patterns to scan

; fixme-files-ignore  ; sets file patterns to ignore

|]

