module Fixme.Defaults where

import Data.Text (Text)
import Text.InterpolatedString.Perl6 (qc)


defConfig :: Text
defConfig = [qc|

fixme-comments   // #  "--" ";"

fixme-prefix     FIXME:     bugs issues   ; defines a fixme entity and it's categories

; sets file patterns to scan
; fixme-files **/*.txt **/*.hs

; sets file patterns to ignore
; fixme-files-ignore .direnv/** dist-newstyle/**

|]


defLog :: Text
defLog = [qc|
;; This is a log file. All fixmies status updates go here
|]


