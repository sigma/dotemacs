;;; buffer-config.el --- Configuration for buffer

;; Copyright (C) 2004  Free Software Foundation, Inc.

;; Author: Yann Hodique <Yann.Hodique@lifl.fr>
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

;;; Buffers

(use-package dired
    :commands dired
    :config
    (progn
      (use-package dired-x
          :config (setq dired-x-hands-off-my-keys t
                        dired-find-subdir nil))
      (use-package dired+
          :ensure t)
      (use-package wdired
          :commands wdired-change-to-wdired-mode
          :init (bind-key "C-c C-c" 'wdired-change-to-wdired-mode
                          dired-mode-map))

      ;; use ediff for diffing
      (defadvice dired-diff (around ad-dired-diff-ediff act)
        (flet ((diff (old new switches) (ediff old new)))
          ad-do-it))

      (defadvice dired-backup-diff (around ad-dired-backup-diff-ediff act)
        (flet ((diff-backup (old switches) (ediff-backup old)))
          ad-do-it))

      (add-hook 'dired-mode-hook
          (lambda ()
            ;; Set dired-x buffer-local variables here.  For example:
            (dired-omit-mode 1)))))

(defun my-ibuffer-stale-p (&optional noconfirm)
  (frame-or-buffer-changed-p 'ibuffer-auto-buffers-changed))

(defun my-ibuffer-auto-revert-setup ()
  (setq-local buffer-stale-function 'my-ibuffer-stale-p)
  (setq-local auto-revert-verbose nil)
  (auto-revert-mode 1))

(use-package ibuffer
    :commands ibuffer
    :bind ("C-x C-b" . ibuffer)
    :config (progn
              (use-package ibuf-ext)
              ;; ibuffer, I like my buffers to be grouped
              (add-hook 'ibuffer-mode-hook
                        (lambda ()
                          (my-ibuffer-auto-revert-setup)
                          (local-set-key (kbd "r") 'ibuffer-update)
                          (ibuffer-switch-to-saved-filter-groups
                           "default")))))

(use-package icomplete+
    :ensure t)

(use-package icomplete
    :commands icomplete-mode
    :defer 1
    :config
    (require 'icomplete+)
    (icomplete-mode 1))

(provide 'buffer-config)
;;; buffer-config.el ends here
