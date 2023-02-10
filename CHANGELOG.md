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



