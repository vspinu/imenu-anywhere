[![License GPL 3][badge-license]][copying]
[![MELPA][melpa-badge]][melpa-package]
[![MELPA Stable][melpa-stable-badge]][melpa-stable-package]

# imenu-anywhere

`imenu-anywhere` command pops an IDO interface with all the imenu tags across
all buffers with the same mode as the current one. In a sense it is similar
to etag selection, but works only for the open buffers. This is often more
convenient as you don't have to explicitly build the etags table.

## Installation

Available on all major `package.el` community maintained repos -
[MELPA Stable][] and [MELPA][] repos.

MELPA Stable is recommended as it has the latest stable version.
MELPA has a development snapshot for users who don't mind breakage but
don't want to run from a git checkout.

You can install `imenu-anywhere` using the following command:

<kbd>M-x package-install [RET] imenu-anywhere [RET]</kbd>

or if you'd rather keep it in your dotfiles:

```el
(unless (package-installed-p 'imenu-anywhere)
  (package-refresh-contents)
  (package-install 'imenu-anywhere))
```

If the installation doesn't work try refreshing the package list:

<kbd>M-x package-refresh-contents</kbd>

## Usage

Add the following to your Emacs config to enable
`imenu-anywhere`:

```lisp
(global-set-key (kbd "C-.") #'imenu-anywhere)
```

There is also `helm-imenu-anywhere` which is like imenu-anywhere but uses
[Helm](https://github.com/emacs-helm) interface instead of IDO. The Helm library
is not loaded by imenu-anywhere and you have to install it separately.

## License

Copyright © 2011-2016 Vitalie Spinu and [contributors][].

Distributed under the GNU General Public License; type <kbd>C-h C-c</kbd> to view it.

[badge-license]: https://img.shields.io/badge/license-GPL_3-green.svg
[melpa-badge]: http://melpa.org/packages/imenu-anywhere-badge.svg
[melpa-stable-badge]: http://stable.melpa.org/packages/imenu-anywhere-badge.svg
[melpa-package]: http://melpa.org/#/imenu-anywhere
[melpa-stable-package]: http://stable.melpa.org/#/imenu-anywhere
[COPYING]: http://www.gnu.org/copyleft/gpl.html
[contributors]: https://github.com/vspinu/imenu-anywhere/contributors
[melpa]: http://melpa.org
[melpa stable]: http://stable.melpa.org
