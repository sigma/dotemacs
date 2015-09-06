;;; ediff-config.el --- Configuration for ediff

;; Copyright (C) 2004-2015  Yann Hodique

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

;;; Ediff

(eval-when-compile
  (require 'ediff))

(use-package ediff-wind
    :defer 3
    :config
    (setq ediff-window-setup-function 'ediff-setup-windows-plain)
    (setq ediff-split-window-function 'split-window-horizontally))

;; Some custom configuration to ediff
(defvar my-ediff-bwin-config nil "Window configuration before ediff.")

(defvar my-ediff-bwin-reg ?b
  "*Register to be set up to hold `my-ediff-bwin-config'
    configuration.")

(defvar my-ediff-awin-config nil "Window configuration after ediff.")

(defvar my-ediff-awin-reg ?e
  "*Register to be used to hold `my-ediff-awin-config' window
    configuration.")

(defun my-ediff-bsh ()
  "Function to be called before any buffers or window setup for
    ediff."
  (setq my-ediff-bwin-config (current-window-configuration))
  (set-register my-ediff-bwin-reg
                (list my-ediff-bwin-config (point-marker))))

(defun my-ediff-ash ()
  "Function to be called after buffers and window setup for ediff."
  (setq my-ediff-awin-config (current-window-configuration))
  (set-register my-ediff-awin-reg
                (list my-ediff-awin-config (point-marker))))

(defun my-ediff-qh ()
  "Function to be called when ediff quits."
  (when my-ediff-bwin-config
    (set-window-configuration my-ediff-bwin-config)))

(defun ediff-add-changelog (&optional key)
  (interactive)
  (with-current-buffer
      (ediff-get-buffer
       (ediff-char-to-buftype (or key last-command-event)))
    (add-change-log-entry-other-window)))

(defun yh/install-ediff-changelog-keys ()
  (define-key ediff-mode-map ".a" 'ediff-add-changelog)
  (define-key ediff-mode-map ".b" 'ediff-add-changelog)
  (define-key ediff-mode-map ".c" 'ediff-add-changelog))

(use-package ediff-init
    :defer 3
    :config
    (add-hook 'ediff-before-setup-hook 'my-ediff-bsh)
    (add-hook 'ediff-after-setup-windows-hook 'my-ediff-ash)
    (add-hook 'ediff-quit-hook 'my-ediff-qh t)
    (add-hook 'ediff-keymap-setup-hook 'yh/install-ediff-changelog-keys))

(defun command-line-diff (switch)
  (let ((file1 (pop command-line-args-left))
        (file2 (pop command-line-args-left)))
    (ediff file1 file2)))

(add-to-list 'command-switch-alist '("diff" . command-line-diff))

;; Needed to correctly highlight ediff sessions
(add-to-list 'auto-mode-alist '("\\.\\(base\\|current\\|merged\\)\\'"
                                nil ediff-merge-files))

(provide 'ediff-config)
;;; ediff-config.el ends here
