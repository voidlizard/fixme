
##  2023-02-28

fixme log-macro feature:

Allows to define "log-macros" to shortcut fixme's operations:


```
fixme test HASJDx2Zio
```

instead of

```
fixme set workflow test HASJDx2Zio
```

to use this feature, define "log-macro" in .fixme/config
file:

```
cat .fixme/config

fixme-log-macro backlog (fixme-set "workflow" "backlog" "$1")
fixme-log-macro test    (fixme-set "workflow" "test" "$1")
fixme-log-macro wip     (fixme-set "workflow" "wip" "$1")
fixme-log-macro assign  (fixme-set "assigned" "$1" "$2")


fixme test HASJDx2Zio
(fixme-set "workflow" "test" "HASJDx2Zio")

```

Use "$1" .. "$32" as command argument substitutions.

Right now, only string substitutions are supported.



##  2023-02-10

```
fixme cat <id>
```

Outputs fixme from git blob where it was taken.
Supports -A and -B keys to show context after and before.

Supports pager, like bat, for an instance. To show the fixmies
with nice syntax highlithing.

```

cat ~/.config/fixme/config

fixme-pager bat

fixme-map-syntax qqq txt

```

fixme-map-syntax replaces and extension to someone,
that pager knows and able to handle.

Right now it's supported only for "bat".



