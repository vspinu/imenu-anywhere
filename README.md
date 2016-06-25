[![License GPL 3][badge-license]][copying]
[![MELPA][melpa-badge]][melpa-package]
[![MELPA Stable][melpa-stable-badge]][melpa-stable-package]

# imenu-anywhere

`imenu-anywhere` provides navigation for imenu tags across all buffers that
satisfy a filtering criteria. Available criteria are - all buffers with the same
major mode, same project buffers and user defined list of friendly mode buffers.

## Installation

Available from `package.el` community maintained repos -
[MELPA Stable][] and [MELPA][] repos.

Install using the following command:

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

Simply bind `imenu-anywhere` to a key of your choice:

```el
 (global-set-key (kbd "C-.") #'imenu-anywhere)
```

By default `imenu-anywhere` uses `completing-read` for completion. If you are
using `ido-ubiquitous` or `helm-mode` which redefine `completing-read-function`
you are set. Otherwise you can use any of the provided wrappers directly:

  - `ido-imenu-anywhere`,
  - `ivy-imenu-anywhere` or
  - `helm-imenu-anywhere`

For the last two wrappers you will need to install `ivy` or `helm` separately.

## Configuration

By default `imenu-anywhere` makes tags available from buffers with the same
mode, same project and friendly modes defined by `imenu-anywhere-friendly-modes`. 
You can configure the filtering strategies with `imenu-anywhere-buffer-filter-functions`.


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
[MELPA]: http://melpa.org
[MELPA stable]: http://stable.melpa.org
