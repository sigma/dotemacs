;;; compile-config.el --- configuration for compile command

;; Copyright (C) 2005-2015 Yann Hodique

;; Author: Yann Hodique <yann.hodique@gmail.com>
;; Keywords:

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

;;

;;; Code:

(eval-when-compile
  (require 'cl))

;; Do what I mean when I compile
;; ie chose the right compilation command
(defvar dwim-compile-alist '(("^\\(?:GNU\\)?[Mm]akefile$" . "make")
                             ("\\.pro$" . "qmake")
                             ("^SConstruct$" . "scons"))
  "Association list between filename patterns and building method")

(defconst compile-default-command "make")

(defun dwim-compile-check (motif)
  "Test a regexp against every file in the current directory. Tries to be smart about what \"current directory\" is."
  (let* ((filename (buffer-file-name (current-buffer)))
         (dir (if filename
                  (file-name-directory filename)
                default-directory)))
    (loop for f in (directory-files dir)
          if (string-match motif f) return f)))

(defun dwim-compile-command ()
  (loop for choice in dwim-compile-alist
        if (dwim-compile-check (car choice)) return (cdr choice)
        finally return compile-default-command))

(use-package compile
    :bind ("C-c c" . compile)
    :config
    (make-variable-buffer-local 'compile-command)
    (setq-default compile-command '(dwim-compile-command)))

(provide 'compile-config)
;;; compile-config.el ends here
