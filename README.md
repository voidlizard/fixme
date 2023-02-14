
# fixme

Git-oriented trackerless distributed issue tracker.
Yes, an another one.

## TL;DR

```
fixme init

vim .fixme/config

fixme update

fixme list



```


This is an utility that scans git repository for TODO/FIXME entries
with a specific format (TBD), and represents them as 'issues/tickes'
in typical bug trackers.

Each issue gets an id as a hash of it's contens and file
name where it were discovered.

Example:

```
[user@host:~/]$ ./bin/fixme list | tail -n 10
3PJ21zvWuC TODO:  skip-empty-fixmies
2L8kgTSCHq TODO:  define-how-to-process-log
FazhT3HjjG FIXME: check-return-code
Dc6J44mwYV FIXME: check-return-code
m8eNczQ5vd TODO:  dont-show-already-existed-fixmies-in-scan
AEMM9wdH6s TODO:  merge-to-null-to-hide-fixmie
CwdnmVYyKE TODO:  inverted-filters
CUYGqJeE7Q TODO:  exact-attrib-filters
9FcnrRf8sN FIXME: play only diff for log ?
HfsxkyaCGc FIXME: don't play log twice(?)
```


"Issues" or "fixmies" come from TODO/FIXME comments/entries from
all (commited) git blobs across the project.

So, therefore "fixmies" are immutable: editing "fixme"
section in a file will cause to a new git blob to appear,
and if the fixme's content is changed --- it will be a
new fixme then.

So, how to manage their state or do something
usefull with them?

All operations over fixmies are done via the log.

This log is located in .fixme/log file and it
replays each time when 'fixme update' is called.

It might be slow at some point, and it may be and will
be optimized later on.

So using this log, we may perform operations over the fixmies:

  1. Change their state (attributes)
  1. Mark them merged
  1. Mark them deleted

"Merge" means that "fixme" X is "fixme" Y now. "Fixme X" won't appear
in the list anymore (but it will persist in a state).

See the example:

```
fixme-merged "BbjfCj7qDD" "H4epFBNr2i"    ;; claims that BbjfCj7qDD fixme is actually H4epFBNr2i

fixme-merged "6D8sQLF6uc" "4rCoVVC94N"


fixme-set "workflow" "#closed" "7AGbMzAHza" ;; sets "workflow" attribute of the fixmie #7AGbMzAHza
                                            ;; to '#closed' value.
                                            ;; yes, it's literally a string with value '#closed'

fixme-del "7AGbMzAHza" ;; puts fixme #7AGbMzAHza into a "deleted" table. so it
                       ;; wan't appear in search  anymore.
                       ;; however it's not deleted.

                       ;; there is no sense to delete anything,
                       ;; 'cause fix update an will scan all blobs in repository
                       ;; from the very beginning.

                       ;; so any deleted fixme may be undeleted with ease.

```


Therefore, there are two streams:

  1. A: Streams of "fixmies" from the all blobs from
     the repository from the very beginning;

  2. B: Stream of the fixmies state updates.

And the state is calculated as an application of operations from
stream "B" over entities from stream "A".

Physically the state is an sqlite database .fixme/state.db

This state is stored only locally and it's not shared across
the git repository.

The .fixme/config and .fixme/log are shared. So,
all participants should have same fixmies attributes
and states as far as they have the same log.


It works as a cache. It might be deleted, it will be re-created next
time 'fixme update' will be running.


What if some fixmies got a similar id?  They will be displayed all.

It should be ignored by 'fixme update' process, so it should not
(supposedly) processed.

It you really need a duplicated fixme, make it unique by adding
something unique into it's description.

If you got two identical fixmies with different ids,
you may

  1. Mark one of them deleted
  1. Mark one of them "merged", i.e claim that fixmie X is actually Y.
     After this, fixme X wan't be displayed.


## The config

The config is located at ./fixme/config and right now support the following
options:

```
;; fixme config file

;; claims the line comment symbols. there are may be some.

fixme-comments   // # --

;; prefixes or tags of fixmies. There are might be some.
;; "bugs" , "issues" are categories of fixmies
;; and they are not supported yet.

;; So, you may define as much tags
;; as you want: FIXME:, TODO:, NOTICE:, whatever

fixme-prefix     FIXME:   bugs issues

fixme-prefix     TODO:    bugs issues

;; file mask to search fixmies.
;; Althought only git blobs scanned, it makes sense
;; to filter the blobs for scanning.

fixme-files **/*.hs

fixme-files doc/devlog

;; Mask of files to exclude from scanning

fixme-files-ignore .direnv/** dist-newstyle/**

;; yes, direnv and dist-newstyle are not in git typically

;; How many characters of hash to show?
;; Fixme id is a 32-bytes cryptographic hash from
;; it's content and some context.
;; But it's big and inconvenient. So it's possible
;; to display and use only part of it.

fixme-id-show-len 10

;; report definition section
;; fixme-report statement:
;;
;; fixme-report report-id report-type
;;
;;  right now report-type is always json

[ fixme-report all json

;; render render-id template-file
;; right now render-id is always  builtin:microstache
;; may be omitted. in this case, plain json will be dumped

  (render builtin:microstache report-wip.tpl)

  (post builtin:columns | 10 10 8 10 _)

;; postprocessor

;; post postprocessor args

;; right now it's only builtin:columns

;; args are: delim col-width*

;; _ means left the columns as is
;; only the enumerated colums will be displayed,

;; i.e if there is less args that colums in the output,
;; remaining colums will be dropped.

;; use _ to enumerate a column

  (query ~workflow:backlog)

;; query part. there are maybe
;; (query ~a:b)  a != b
;; (query ?a:b) (query ?a:c) a == b|| a == c
;; (query a:b) (query b:c)  a == b && b == c
;; this part is still wip, but works somehow.

  ; (query ?workflow:wip)
  ; (query ?workflow:test)
]

;; another example
[ fixme-report wip json
  (render builtin:microstache report-wip.tpl)
  (post builtin:columns | 10 10 8 10 _)
  (query ?workflow:test)
  (query ?workflow:wip)
  (query ?workflow:fixed)
]
```

## Reports

Report is a fixed query + template. It may be set up
with fixme-report statement in .fixme/config.

See the examples above.

Report may be executed by

```
fixme report report-id queries*
```
queries\* are optional extra clauses
for the report.


## Local config

There is also an optional local config

```
$XDG_CONFIG_HOME/fixme/config
```

that allows each user setup their own stuff,
that should not be shared across the project.

Right now is't  only external pager utility and
display context  for "fixme cat" command:


```
fixme-pager bat

;; any external pager. if not specified, fixme will
;; just output lines to STDOUT.
;; fixme recognizes bat utility and pass --file-name
;; argument to it to make  cat dumps looks really nice.

fixme-def-context 2 10

;; default -B and -A arguments for
;; fixme cat command to show fixme in context

```

TODO:  default-local-config-with-def-context

fixme-def-context makes dumps looks really nicer,
  so it should be added by default.


## The FIXME format


```
identation comment space+ tag space space* char char* eol
(identation comment space+ char* eol)+
eol | identation <= fixme.identation
```

Examples:

```
  -- FIXME: to-play-log-function

     run <- forM attrs $ \(i,a,v) -> do

```

Note, that fixmie ends when:

  1. Two blank lines together
  1. The indentation of the following line is lesser,
     than in this fixmie

All of thouse are WIP, and little bit fuzzy.


## Known bugs

YES.

```
fixme list
```
to watch them.


## FAQ

### How do I start?

```
fixme init

git add .fixme/config
git add .fixme/log

echo .fixme/state.db >> .gitignore

git commit -a m 'added fixme'

fixme update
fixme list

```

Right now you may install fixme using nix flake:
https://github.com/voidlizard/fixme/blob/master/flake.nix

Or compile it on your own. Somehow it turned out that fixme uses some (only the one, I guess)
features from GHC 9.2

That's weird. But I think that almost nobody will use fixme before GHC 9.2 become mainstream or even
little bit outdated, so I let it this way so far.

If you want to try fixme and it's an obstacle --- let me know.  If you are NIX user, probably
you will install fixme with ease. If you are not... Oops.

### How do I see the full issue text?

```
fixme-files list -f  <search-string>
```

For now, it will show all issues that match the search-string

### How do I create an issue?


Make sure that .fixme/config contains like

```
fixme-files **/*.hs
```

option to tell fixme what files to scan.

Make sure it also has

```
fixme-comments   // # --
```

section and it has comment prefixes for you  language.
If you want to use ';' characters in comments, use double quotes ";".

also make sure it has something like

```
fixme-prefix     FIXME:   bugs issues
```

Create a FIXME: entry somewhere in project. Example:

```
 // FIXME: write-something-useful   <- This is a FIXME comment
 //   Really, do it.
                                    <- Here is the end of a fixmie.
int main() {
  return -1;
}

```

### How do I list my fixmies?

```
fixme list

fixme list TODO FIXME  write-so

```
It will search thouse as prefixes over all attributes.
More sofisticated behaviour is in process.


### How do I assign a fixmie to a someone?

```
fixme set assigned Alice 9FcnrRf8s
```

It will just set a text attribute 'assigned' to value 'Alice'. How to
check it out?

```
fixme update
fixme list Alice
```

will displays all fixmies that has any attribute that has value
started from 'Alice'

How to filter by attribute exactly 'assigned' == exactly 'Alice' ?

```
fixme list -q assigned:Alice  workflow:wip
```

will print all fixmies with assigned == 'Alice' and workflow == 'wip'


### How to display all attributes set for a fixme?

1. Define a report, like

```
cat >> .fixme/config
[ fixme-report wip-json json
]
^D

fixme report wip-json
```

and see json with all loaded attributes.

### There are many attempts of making "distributed" and "git-oriented" trackers. Why another one?

They (IMO) are all failed.  git-bug is the best of them, however it's approach to make
a separate reference for each issue is not viable. It's noisy and it's inconvenient to
track. IMO, again.

There are some abandoned trackers. They use different approaches, but
it does not matter 'cause they are dead by now.

There are only a few options to keep bugs in git, all have
their own disadvantages.

This approach considered to be lesser evil (IMO, again) --- although
the .fixme/log will produce some noise.

But this noise actually makes sense --- it's operations over the issues:

 1. Change workflow
 2. Close
 3. Merge
 4. Set attributes

and so on.

This is only a one plain-text file with meaningfull content.

Again, this approach is for people who starts writing code
from FIXME/TODO comments right in the code.

It makes a lot of sense to keep issues along with code.

fixme scans files for a specific comments, remembers
and tracks them with the context.

It's oriented to git hashes and blobs, not file names.

It opens a lot of opportunities, like CHANGELOG and other
docs generation and lightweight code review.

Will see.


### Isn't scanning all blobs in repo and playing all the operation log each time slow?

Probably, it is.

Right now, fixme does not process blobs that already processed.

But operations log is played each time. It could be optimized, perhaps.

Will do.

### What is an actual difference between deleted and merged?

Really not a big. But having merge(from,to) relation gives
us possibility to track fixmie's history

### How to see original code where fixme was taken?

```
fixme cat <fixme-id>
```

It supports external pager, that may be configured in
local config file

```

cat ~/.config/fixme/config

fixme-pager bat

```

### Why the config syntax is so weird?

It is not. It is sexps. On the toplevel parens
may be omitted, but it all sexps.

### There is should be restrictions on attribute values and names

Done


### I need feature X.

Let me know! Feedback is appreciated.


### There is a bug

Let me know! Feedback is appreciated.


