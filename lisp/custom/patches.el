;;; patches.el --- various patches for emacs

;; Copyright (C) 2004-2015 Yann Hodique

;; Author: Yann Hodique <yann.hodique@gmail.com>
;; Keywords: lisp, extensions

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Some patches I cannot live without. Fix bad default behaviors, and provide
;; missing basic features. Some code is by me, some is hacked by me, and some
;; has nothing to do with me :-)

;;; Code:
(eval-when-compile (require 'cl))

;; Map a condition/action on a list
(defun mapcond (test result list &optional default)
  "Map a TEST function over LIST and return the application of
the RESULT function over the first positive answer. If DEFAULT
is not nil, then in case of no success, this value is returned"
  (loop for n in list
        if (funcall test n) return (funcall result n)
        finally return default))

(defmacro make-double-command (name args doc-string interactive
                                      first-form second-form)
  "Define a new command from 2 behaviors. First invocation runs
  the first one. An immediate second invocation runs the second
  command. Any further invocation keeps running the second
  command."
  (declare (indent 2))
  (let ((int-form (if (not interactive)
                      '(interactive)
                    (list 'interactive interactive))))
    `(progn
       (defun ,name ,args ,doc-string
         ,int-form
         (if (eq last-command this-command)
             ,(if (and (listp second-form) (> (length second-form) 1))
                  (cons 'progn second-form)
                second-form)
           ,first-form)))))

;; Many thanks to utis (Oliver Scholz)
(defmacro defmadvice (flist spec &rest body)
  "Define the same advice for several functions."
  (let ((defs (mapcar
               (lambda (f)
                 `(defadvice ,f
                      ,(append (list (car spec)
                                     (intern
                                      (format "ad-%s-%s-%s"
                                              (symbol-name f)
                                              (symbol-name (cadr spec))
                                              (car spec)))) (cddr spec))
                    ,@body)) flist))) `(progn ,@defs)))

(defun add-mhook (mlist func &optional append local)
  "Add the same function to multiple hooks"
  (dolist (m mlist) (add-hook m func append local)))

(defun clear-hook (hook)
  "Clear a hook"
  (set hook nil))

;; fix split-window behavior
(defmadvice (split-window-below split-window-right)
  (after split act)
  "Open another buffer in the new window"
  (set-window-buffer (next-window) (other-buffer)))

;; make the y or n suffice for a yes or no question
(fset 'yes-or-no-p 'y-or-n-p)

;; Goto matching parenthesis
(defun match-paren ()
  "Will bounce between matching parens just like % in vi"
  (interactive)
  (let ((prev-char (char-to-string (preceding-char)))
	(next-char (char-to-string (following-char))))
	(cond ((string-match "[[{(<]" next-char) (forward-sexp 1))
		  ((string-match "[\]})>]" prev-char) (backward-sexp 1))
		  (t (error "%s" "Not on a paren, brace, or bracket")))))

(global-set-key [(control =)] 'match-paren)

;; ...never switch to overwrite mode, not even accidentally
(global-set-key [insert] 'undefined)

;; disable backups for files in /tmp or in my Mail or News directories.
(defun ecm-backup-enable-predicate (filename)
  (and (not (string= "/tmp/" (substring filename 0 5)))
       (not (string-match "/mail/" filename))
       (not (string-match "/News/" filename))))

(setq backup-enable-predicate 'ecm-backup-enable-predicate)
(setq backup-directory-alist `(("." . "~/.backups") ; centralize backup files
                               (,tramp-file-name-regexp . nil))) ; disable for tramp

(setq
 ;; don't break links
 backup-by-copying t
 ;; use numbered backups
 version-control t
 ;; keep only 20 latest versions
 kept-old-versions 0
 kept-new-versions 20
 ;; and always delete the others without confirmation
 delete-old-versions t)

;; Put autosaves files in a single directory too
(setq auto-save-file-name-transforms
      `((".*"
         ,(expand-file-name "~/.autosaves/") t)))

(setq temporary-file-directory (expand-file-name "~/tmp/"))

;; Why the hell should some commands be disabled?
(setq disabled-command-function nil)

;; Scroll up then down should go back to the start point
(setq scroll-preserve-screen-position t)

;; Adapt open-line behavior when arg <= 0
(defadvice open-line (around open-line-around (arg) act)
  "Open new line(s) at end of line if arg is <= 0."
  (if (<= arg 0)
      (let ((var (if (equal arg 0) -1 arg)))
        (save-excursion
          (end-of-line)
          (open-line (- var))))
    ad-do-it))

(defun yank-rpop (arg)
  (interactive "*p")
  (yank-pop (- arg)))
(global-set-key "\M-Y" 'yank-rpop)

(defun yh/collapse-home-directory (filename)
  "When possible, transform an absolute path into its ~ prefixed form"
  (if (string-match (concat "^" (regexp-quote home-directory)) filename)
      (concat "~/" (file-relative-name filename home-directory))
    filename))

;; Use better names than plop<1> and plop<2> for files with same name
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward
      uniquify-strip-common-suffix nil)

;; Provide modes for common config files
(require 'generic-x)

;; Yes, it's disgusting, but it's a portable way to disable that silly
;; message
(put 'inhibit-startup-echo-area-message 'saved-value
     (setq inhibit-startup-echo-area-message (user-login-name)))

(setq mac-option-modifier 'meta
      mac-command-modifier 'super
      mac-right-option-modifier nil
      mac-right-command-modifier 'alt)

(provide 'patches)
;;; patches.el ends here
