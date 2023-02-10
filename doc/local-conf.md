
## Local config

location: $XDGCONF/fixme/config

### fixme-pager

Sets the pager for fixme cat.

Useful for syntax highlighting and
nicer displaying, like bat program.

```
fixme-pager bat
```

fixme knows bat and passes --file-name arg to it
to display file properly.

### fixme-def-context

Sets default display
context (before-lines, after-lines) for fixme cat

```
fixme-def-context 2 10
```


