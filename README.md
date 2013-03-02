`imenu-anywhere` command pops an IDO interface with all the imenu tags across
all buffers with the same mode as the current one. In a sense it is similar
to etag selection, but works only for the open buffers. This is often more
convenient as you don't have to explicitly build the etags table.

To activate, just bind imenu-anywhere to a convenient key:
```lisp
(global-set-key (kbd "C-.") 'imenu-anywhere)
```
There is also `helm-imenu-anywhere` which is like imenu-anywhere but uses
helm (https://github.com/emacs-helm) interface instead of IDO. Helm library
is not loaded by imenu-anywhere.el and you have to install helm separately.
