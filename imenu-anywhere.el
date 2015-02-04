;;; imenu-anywhere.el --- ido/helm imenu tag selection across all buffers with the same mode
;;
;; Copyright (C) 2011-2015 Vitalie Spinu
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
;; `imenu-anywhere` command pops an IDO interface with all the imenu tags across
;; all buffers with the same mode as the current one. In a sense it is similar
;; to etag selection, but works only for the open buffers. This is often more
;; convenient as you don't have to explicitly build the etags table.
;;
;; To activate, just bind imenu-anywhere to a convenient key:
;;
;;    (global-set-key (kbd "C-.") 'imenu-anywhere)
;;
;; There is also `helm-imenu-anywhere` which is like imenu-anywhere but uses
;; helm (https://github.com/emacs-helm) interface instead of IDO. Helm library
;; is not loaded by imenu-anywhere.el and you have to install helm separately.


(require 'ido nil t)
(require 'imenu)
(eval-when-compile
  (require 'helm nil t)
  (require 'cl-lib))


(defvar imenu-anywhere-use-ido t
  "Use ido even when ido-mode is not enabled.")
(defvar imenu-anywhere-delimiter-ido "/")
(defvar imenu-anywhere-delimiter-helm " / ")
(defvar imenu-anywhere-buffer-list-function 'buffer-list
  "Function that returns the list of buffers for `imenu-anywhere' to consider.
Any buffers that are not on this list will be ignored.")
(make-variable-buffer-local 'imenu-anywhere-buffer-list-function)

(defvar imenu-anywhere-cached-candidates nil
  "An alist of flatten imenu tags from of the form (name . marker)")
(make-variable-buffer-local 'imenu-anywhere-cached-candidates)
(defvar imenu-anywhere-cached-tick nil
  "Value of buffer's tick counter at last imenu-anywere update.")
(make-variable-buffer-local 'imenu-anywhere-cached-tick)


(defun imenu-anywhere-candidates (&optional modes force-update)
  "Return an alist of imenu tags from buffers where imenu is meaningful.
If MODES is nil look only for buffers with the mode equal to the
mode of the current buffer.  If MODES is t return all the buffers
irrespective of mode.  Else MODES must be a _list_ of symbols of
the major modes of interest."
  (when (null modes)
    (setq modes (list major-mode)))
  (apply 'append
         (mapcar (lambda (buff)
                   (when (or (eq modes t) ; all of them
                             (member (buffer-local-value 'major-mode buff) modes))
                     (with-current-buffer buff
                       (let ((tick (buffer-modified-tick buff)))
                         (if (and (eq imenu-anywhere-cached-tick tick)
                                  (not force-update))
                             ;; return cached
                             imenu-anywhere-cached-candidates
                           ;; else update the indexes if in imenu buffer
                           (setq imenu-anywhere-cached-tick tick)
                           (setq imenu-anywhere-cached-candidates
                                 (imenu-anywhere-buffer-candidates)))))))
                 (funcall imenu-anywhere-buffer-list-function))))

(defun imenu-anywhere-buffer-candidates ()
  "Return an alist of candidates in the current buffer."
  ;; avoid imenu throwing ugly messages
  (when (or (and imenu-prev-index-position-function
                 imenu-generic-expression)
            (not (eq imenu-create-index-function 'imenu-default-create-index-function)))
    ;; (ignore-errors
    (setq imenu--index-alist nil)
    (cl-delete-if '(lambda (el) (or (null (car el))
                               (equal (car el) "*Rescan*")))
                  (sort (cl-mapcan 'imenu-anywhere--candidates-from-entry
                                   (imenu--make-index-alist t))
                        (lambda (a b) (< (length (car a)) (length (car b))))))))

(defvar imenu-anywhere--preprocess-entry 'imenu-anywhere--preprocess-entry-ido
  "Holds a function to process each entry.
See the code for `imenu-anywhere--preprocess-entry-ido' and
`imenu-anywhere--preprocess-entry-helm'")

(defun imenu-anywhere--preprocess-entry-ido (entry prefix)
  (setcar entry (concat (replace-regexp-in-string "\\c-*$" "" (car entry))
                        imenu-anywhere-delimiter-ido
                        prefix))
  entry)

(defun imenu-anywhere--preprocess-entry-helm (entry prefix)
  (setcar entry (concat prefix
                        imenu-anywhere-delimiter-helm
                        (car entry)))
  entry)


(defun imenu-anywhere--candidates-from-entry (entry)
  "Create candidates from imenu ENTRY."
  (if (imenu--subalist-p entry)
      (mapcar (lambda (sub-entry)
                (funcall imenu-anywhere--preprocess-entry sub-entry (car entry)))
              (cl-mapcan 'imenu-anywhere--candidates-from-entry (cdr entry)))
    (let ((pos (cdr entry)))
      (unless (markerp pos)
        (setq pos (copy-marker pos))) ;; assumes it is an integer, throw error if not?
      (list (cons (car entry) pos)))))


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

(defun imenu-anywhere--goto-function (name position &optional rest)
  "Function to be used as `imenu-default-goto-function'"
  (let* ((is-overlay (overlayp position))
         (buff (or (and is-overlay (overlay-buffer position))
                   (marker-buffer position)))
         (position (or (and is-overlay (overlay-start position))
                       (marker-position position))))
    (switch-to-buffer buff)
    (if (or (< position (point-min))
            (> position (point-max)))
        ;; widen if outside narrowing
        (widen))
    (goto-char position)))

(defun imenu-anywhere--read (index-alist &optional guess)
  "Read a choice from an INDEX-ALIST of imenu items via IDO."
  (let* ((str-at-pt (thing-at-point 'symbol))
         (default (and guess str-at-pt
                       (imenu-anywhere--guess-default index-alist str-at-pt)))
         (names (mapcar 'car index-alist))
         (name (ido-completing-read "Imenu: " names nil t nil nil default)))
    (assoc name index-alist)))

;;;###autoload
(defun imenu-anywhere (&optional modes)
  "Switch to a buffer-local tag from Imenu via Ido."
  (interactive "P")
  (when (called-interactively-p 'interactive)
    (if modes
        (setq modes t)))
  (let (reset-ido)
    (when  (and (not ido-mode)
                (featurep 'ido )
                imenu-anywhere-use-ido)
      ;; ido initialization
      (ido-init-completion-maps)
      (add-hook 'minibuffer-setup-hook 'ido-minibuffer-setup)
      (add-hook 'choose-completion-string-functions 'ido-choose-completion-string)
      (setq reset-ido t))
    (unwind-protect
        (progn
          ;; set up ido completion list
          (let ((imenu-default-goto-function 'imenu-anywhere--goto-function)
                (index-alist (imenu-anywhere-candidates modes)))
            (if (null index-alist)
                (message "No imenu tags")
              (imenu (imenu-anywhere--read index-alist t)))))
      ;; ido initialization
      (when reset-ido
        (remove-hook 'minibuffer-setup-hook 'ido-minibuffer-setup)
        (remove-hook 'choose-completion-string-functions 'ido-choose-completion-string)
        ))))

;;;###autoload
(defalias 'ido-imenu-anywhere 'imenu-anywhere)

(defvar helm-source-imenu-anywhere
  '((name . "imenu-anywere")
    (candidates . helm-imenu-anywhere-candidates)
    (persistent-action . (lambda (elm)
                           (imenu-anywhere--goto-function "" elm)
                           (unless (fboundp 'semantic-imenu-tag-overlay)
                             (helm-match-line-color-current-line))))
    (persistent-help . "Show this entry")
    (action . (lambda (elm) (imenu-anywhere--goto-function "" elm))))
  "See (info \"(emacs)Imenu\") and `imenu-anywhere'")

(defun helm-imenu-anywhere-candidates ()
  (with-helm-current-buffer
    (let ((imenu-anywhere--preprocess-entry 'imenu-anywhere--preprocess-entry-helm))
      (imenu-anywhere-candidates))))

;;;###autoload
(defun helm-imenu-anywhere ()
  "`helm' source for `imenu-anywhere'.
Sorting is in increasing order of length of imenu symbols. The
pyramidal view allows distinguishing different buffers."
  (interactive)
  (let ((imenu-default-goto-function 'imenu-anywhere--goto-function))
    ;; (imenu-default-goto-function
    ;;  (if (fboundp 'semantic-imenu-goto-function)
    ;;      'semantic-imenu-goto-function
    ;;    'imenu-default-goto-function)))
    (helm :sources 'helm-source-imenu-anywhere
          :default (thing-at-point 'symbol)
          :buffer "*helm imenu-anywhere*")))

(eval-after-load "helm"
  '(add-to-list 'helm-sources-using-default-as-input 'helm-source-imenu-anywhere))

(provide 'imenu-anywhere)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; imenu-anywhere.el ends here
