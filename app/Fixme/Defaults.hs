module Fixme.Defaults where

import Data.Text (Text)
import System.FilePath
import Text.InterpolatedString.Perl6 (qc)

confDir :: FilePath
confDir = ".fixme"

confFile :: FilePath
confFile = confDir </> "config"

logFile :: FilePath
logFile = confDir </> "log"


defConfig :: Text
defConfig = [qc|

fixme-comments   // #  --

fixme-prefix     FIXME:     bugs issues   ; defines a fixme entity and it's categories

; fixme-files         ; sets file patterns to scan

; fixme-files-ignore  ; sets file patterns to ignore

|]


defLog :: Text
defLog = [qc|
;; This is a log file. All fixmies status updates go here
|]


