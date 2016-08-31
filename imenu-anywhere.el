;;; imenu-anywhere.el --- ido/ivy/helm imenu across same mode/project/etc buffers
;;
;; Copyright (C) 2011-2016 Vitalie Spinu
;; Author: Vitalie Spinu  <spinuvit.list[ aaattt ]gmail[ dot ]com>
;; Version: DEV
;; Keywords: ido, imenu, tags
;; URL: https://github.com/vitoshka/imenu-anywhere
;; Package-Requires: ((cl-lib "0.5"))
;;
;; This file is NOT part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; `imenu-anywhere` provides navigation for imenu tags across all buffers that
;; satisfy grouping criteria. Available criteria include - all buffers with the
;; same major mode, same project buffers and user defined list of friendly mode
;; buffers.
;;
;; To activate, just bind `imenu-anywhere' to a convenient key:
;;
;;    (global-set-key (kbd "C-.") 'imenu-anywhere)
;;
;; By default `imenu-anywhere' uses plain `completing-read'. `ido-imenu-anywhere',
;; `ivy-imenu-anywhere' and `helm-imenu-anywhere' are specialized interfaces.
;;
;; Several filtering strategies are available - same-mode buffers, same-project
;; buffers and user defined friendly buffers. See
;; `imenu-anywhere-buffer-filter-functions'.
;;
;;; Code:

(require 'imenu)
(require 'cl-lib)

;;; Customization
(defgroup imenu-anywhere nil
  "Imenu tag selection across multiple buffers."
  :group 'tools
  :group 'convenience)

(defcustom imenu-anywhere-friendly-modes
  '((clojure-mode clojurescript-mode cider-repl-mode)
    (emacs-lisp-mode inferior-emacs-lisp-mode lisp-interaction-mode)
    (ess-mode inferior-ess-mode)
    (python-mode inferior-python-mode))
  "List of lists of friendly modes.
Each sub-lists contains set of modes which are mutually
accessible. That is, if mode A and B are in the same sub-list
then imenu items from buffer with mode B are accessible from
buffer with mode A and vice versa."
  :group 'imenu-anywhere
  :type '(repeat (repeat symbol)))

(defcustom imenu-anywhere-buffer-filter-functions
  '(imenu-anywhere-same-mode-p
    imenu-anywhere-friendly-mode-p
    imenu-anywhere-same-project-p)
  "Functions returning non-nil if buffer's imenu tags are accessible.
Each function takes two arguments CURRENT-BUFFER and
OTHER-BUFFER. If any of the functions returns non-nil, imenu
items from OTHER-BUFFER are accessible from the
CURRENT-BUFFER. Filters defined in this package are:
`imenu-anywhere-same-mode-p', `imenu-anywhere-friendly-mode-p'
and `imenu-anywhere-same-project-p'. See also
`imenu-anywhere-buffer-list-function' for extra flexibility."
  :group 'imenu-anywhere
  :type '(repeat symbol))

(defvar imenu-anywhere-preprocess-entry-function 'imenu-anywhere-preprocess-for-completion
  "Holds a function to process each entry.
Function must accept two arguments - entry and entry name. See
the code for `imenu-anywhere-preprocess-for-completion' and
`imenu-anywhere-preprocess-for-listing' for examples.")

(defvar-local imenu-anywhere-buffer-list-function 'buffer-list
  "Function that returns the list of buffers for `imenu-anywhere' to consider.
Any buffers that are not on this list will be ignored. This
function is called before filters in
`imenu-anywhere-buffer-filter-functions'.")

(defvar imenu-anywhere-delimiter "/")
(defvar imenu-anywhere--project-buffers nil)
(defvar-local imenu-anywhere--cached-candidates nil)
(defvar-local imenu-anywhere--cached-tick nil)
(defvar-local imenu-anywhere--cached-prep-function nil)

(defun imenu-anywhere-same-mode-p (current other)
  "Return non-nil if buffers CURRENT and OTHER have same mode."
  (eq (buffer-local-value 'major-mode current)
      (buffer-local-value 'major-mode other)))

(defun imenu-anywhere-friendly-mode-p (current other)
  "Return non-nil if buffers CURRENT and OTHER have friendly modes.
Friendly modes are defined by `imenu-anywhere-friendly-modes'."
  (let ((cmode (buffer-local-value 'major-mode current))
        (omode (buffer-local-value 'major-mode other)))
    (cl-some (lambda (mlist)
               (and (member cmode mlist)
                    (member omode mlist)))
             imenu-anywhere-friendly-modes)))

(defun imenu-anywhere-same-project-p (current other)
  "Return non-nil if buffers CURRENT and OTHER are part of the same project.
Currently only projectile projects are supported."
  (unless imenu-anywhere--project-buffers
    (when (fboundp 'projectile-project-buffers)
      (let (projectile-require-project-root)
        (setq-local imenu-anywhere--project-buffers
                    (or (funcall 'projectile-project-buffers)
                        (list nil))))))
  (member other imenu-anywhere--project-buffers))

(defun imenu-anywhere--reachable-buffer-p (buffer)
  (cl-some (lambda (fun)
             (funcall fun (current-buffer) buffer))
           imenu-anywhere-buffer-filter-functions))

(defun imenu-anywhere-candidates ()
  "Return an alist of imenu tags from reachable buffers.
Reachable buffers are determined by applying functions in
`imenu-anywhere-buffer-filter-functions' to all buffers returned
by `imenu-anywhere-buffer-list-function'."
  (setq-local imenu-anywhere--project-buffers nil)
  (let ((buffers (cl-loop for b in (funcall imenu-anywhere-buffer-list-function)
                       if (imenu-anywhere--reachable-buffer-p b)
                       collect b)))
    (apply 'append
           (mapcar (lambda (buff)
                     (with-current-buffer buff
                       (let ((tick (buffer-modified-tick buff)))
                         (if (and (eq imenu-anywhere--cached-tick tick)
                                  (eq imenu-anywhere--cached-prep-function
                                      imenu-anywhere-preprocess-entry-function))
                             ;; return cached
                             imenu-anywhere--cached-candidates
                           ;; else update the indexes if in imenu buffer
                           (setq imenu-anywhere--cached-tick tick)
                           (setq imenu-anywhere--cached-prep-function
                                 imenu-anywhere-preprocess-entry-function)
                           (setq imenu-anywhere--cached-candidates
                                 (imenu-anywhere-buffer-candidates))))))
                   buffers))))

(defun imenu-anywhere-buffer-candidates ()
  "Return an alist of candidates in the current buffer."
  ;; avoid imenu throwing ugly messages
  (when (or imenu-generic-expression
            (and imenu-prev-index-position-function
                 imenu-extract-index-name-function)
            (not (eq imenu-create-index-function 'imenu-default-create-index-function)))
    (let* ((old-index imenu--index-alist)
           (index (condition-case err
                      (progn
                        (setq imenu--index-alist nil)
                        (imenu--make-index-alist t))
                    (error
                     (message "Imenu error in %s. Keeping old index. (%s)" (current-buffer) (error-message-string err))
                     (setq imenu--index-alist old-index)))))
      (when index
        (cl-delete-if (lambda (el) (null (car el)))
                      (sort (cl-mapcan 'imenu-anywhere--candidates-from-entry
                                       index)
                            (lambda (a b) (< (length (car a)) (length (car b))))))))))

(defun imenu-anywhere--candidates-from-entry (entry &optional inner-p)
  "Create candidates from imenu ENTRY.
Return a list of entries when entry is a `imenu--subalist-p' or a
list of one entry otherwise."
  (let ((ecdr (cdr entry))
        (prep-fun imenu-anywhere-preprocess-entry-function))
    (cond ((imenu--subalist-p entry)
           (mapcar (lambda (sub-entry)
                     (funcall prep-fun sub-entry (car entry)))
                   (cl-mapcan (lambda (el) (imenu-anywhere--candidates-from-entry el t))
                              (cdr entry))))
          ((integerp ecdr)
           (when (> ecdr 0) ;; drop (*Rescan* . -99)
             (let ((entry (cons (car entry) (copy-marker ecdr))))
               (list (if inner-p entry (funcall prep-fun entry nil))))))
          ((and (listp ecdr) (eq (car ecdr) 'IGNORE)) nil)
          (t (list (if inner-p entry (funcall prep-fun entry nil)))))))

(defun imenu-anywhere-preprocess-for-completion (entry parent-name)
  "Default function for `imenu-anywhere-preprocess-entry-function'.
Concatenate imenu ENTRY and PARENT-NAME in a meaningful way. This
pre-processing is suitable for minibuffer completion mechanisms
such as `completing-read' or `ido-completing-read'."
  (when entry
    (let* ((entry-name (replace-regexp-in-string "\\c-*$" "" (car entry)))
           ;; in python hierarchical entry is defined as ("foo"
           ;; (*function-definition* ...) (sub-name ..)) where "foo" is the
           ;; ENTRY-NAME from above and *function-definition* is PREFIX. In this
           ;; case we need to reverse the position. We detect such entries by
           ;; assuming that they are enclosed in *..* and hope for the best.
           (reverse (string-match-p "^\\*.*\\*$" entry-name))
           (sep (and parent-name imenu-anywhere-delimiter)))
      (setcar entry (if reverse
                        (concat parent-name sep entry-name)
                      (concat entry-name sep parent-name)))
      entry)))

(defun imenu-anywhere-preprocess-for-listing (entry entry-name)
  "Pre-processor of imenu ENTRY suitable for hierarchical listings.
ENTRY-NAME is commonly a class or type of the object. Helm and
IVY backends use this pre-processing strategy."
  (when entry
    (let ((bname (if (markerp (cdr entry))
                     (concat (buffer-name (marker-buffer (cdr entry))) ": ")
                   "")))
      (setcar entry (concat bname
                            entry-name
                            (and entry-name
                                 imenu-anywhere-delimiter)
                            (car entry))))
    entry))

(defun imenu-anywhere--guess-default (index-alist str-at-pt)
  "Guess a default choice from the given imenu list and string at point."
  (let ((name (regexp-quote str-at-pt)))
    (catch 'found
      (dolist (regex (list (concat "\\`" name "\\'")
                           (concat "\\`" name)
                           (concat name "\\'")
                           name))
        (dolist (item index-alist)
          (if (string-match regex (car item))
              (throw 'found (car item))))))))

(defun imenu-anywhere-goto (name position &optional rest)
  "Function used as `imenu-default-goto-function'."
  (let* ((is-overlay (overlayp position))
         (buff (or (and is-overlay (overlay-buffer position))
                   (marker-buffer position)))
         (position (or (and is-overlay (overlay-start position))
                       (marker-position position))))
    (switch-to-buffer buff)
    (when (or (< position (point-min))
              (> position (point-max)))
      (widen))
    (goto-char position)))

;;;###autoload
(defun imenu-anywhere ()
  "Go to imenu tag defined in all reachable buffers.
Reachable buffers are determined by applying functions in
`imenu-anywhere-buffer-filter-functions' to all buffers returned
by `imenu-anywhere-buffer-list-function'.

Sorting is done within each buffer and takes into account items'
length. Thus more recent buffers in `buffer-list' and shorter
entries have higher priority."
  (interactive)
  (let ((imenu-default-goto-function 'imenu-anywhere-goto)
        (index-alist (imenu-anywhere-candidates)))
    (if (null index-alist)
        (message "No imenu tags")
      (let ((selection
             (let* ((str-at-pt (thing-at-point 'symbol))
                    (default (and str-at-pt
                                  (imenu-anywhere--guess-default index-alist str-at-pt)))
                    (names (mapcar 'car index-alist))
                    (name (completing-read "Imenu: " names nil t nil nil default)))
               (assoc name index-alist))))
        (imenu selection)))))


;;; IDO

;;;###autoload
(defun ido-imenu-anywhere ()
  "IDO interface for `imenu-anywhere'.
This is a simple wrapper around `imenu-anywhere' which uses
`ido-completing-read' as `completing-read-function'. If you use
`ido-ubiquitous' you might be better off by using `ido-anywhere'
instead, but there should be little or no difference."
  (interactive)
  (require 'ido)
  (let ((imenu-anywhere-preprocess-entry-function #'imenu-anywhere-preprocess-for-completion)
        (completing-read-function 'ido-completing-read))
    (imenu-anywhere)))


;;; IVY

;;;###autoload
(defun ivy-imenu-anywhere ()
  "IVY interface for `imenu-anywhere'
This is a simple wrapper around `imenu-anywhere' which uses
`ivy-completing-read' as `completing-read-function'."
  (interactive)
  (unless (require 'ivy nil t)
    (error "[imenu-anywhere]: This command requires 'ivy' package"))
  (let ((ivy-sort-functions-alist)
        (imenu-anywhere-preprocess-entry-function #'imenu-anywhere-preprocess-for-listing)
        (completing-read-function 'ivy-completing-read))
    (imenu-anywhere)))


;;; HELM

;; silence byte-compiler
(defvar helm-sources-using-default-as-input)
(defvar helm-imenu-fuzzy-match)
(defvar helm-current-buffer)
(declare-function helm "helm")
(declare-function helm-highlight-current-line "helm-utils")
(declare-function helm-make-source "helm-source")

(eval-after-load "helm"
  '(progn
     (require 'helm-imenu)
     (defvar helm-source-imenu-anywhere
       (helm-make-source "imenu-anywere" 'helm-source-sync
         :candidates #'helm-imenu-anywhere-candidates
         :persistent-action (lambda (elm)
                              (imenu-anywhere-goto "" elm)
                              (unless (fboundp 'semantic-imenu-tag-overlay)
                                (helm-highlight-current-line)))
         :fuzzy-match helm-imenu-fuzzy-match
         :persistent-help "Show this entry"
         :action (lambda (elm) (imenu-anywhere-goto "" elm)))
       "Helm source for `imenu-anywhere' (which see).")
     (add-to-list 'helm-sources-using-default-as-input 'helm-source-imenu-anywhere)))

(defun helm-imenu-anywhere-candidates ()
  ;; don't use macro with-helm-current-buffer
  (with-current-buffer (or (and (buffer-live-p helm-current-buffer) helm-current-buffer)
                           (setq helm-current-buffer (current-buffer)))
    (let ((imenu-anywhere-preprocess-entry-function
           'imenu-anywhere-preprocess-for-listing))
      (imenu-anywhere-candidates))))

;;;###autoload
(defun helm-imenu-anywhere ()
  "`helm' interface for `imenu-anywhere'.
Sorting is in increasing length of imenu symbols within each
buffer.  The pyramidal view allows distinguishing different
buffers."
  (interactive)
  (unless (require 'helm nil t)
    (error "[imenu-anywhere]: This command requires 'helm' package"))
  (let ((imenu-default-goto-function 'imenu-anywhere-goto))
    (helm :sources 'helm-source-imenu-anywhere
          :default (thing-at-point 'symbol)
          :buffer "*helm imenu-anywhere*")))


(provide 'imenu-anywhere)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; imenu-anywhere.el ends here
