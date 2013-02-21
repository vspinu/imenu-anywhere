`imenu-anywhere` command pops an IDO interface with all the imenu tags across all
buffers with the same mode as the current one. Thus, it compares to etag
selection, but works only for the open buffers. This is often more convenient as
you don't have to explicitly build your etags table.

To activate, jutt bind imenu-anywhere to a convenient key:

```lisp
(global-set-key (kbd "C-.") 'imenu-anywhere)
```
