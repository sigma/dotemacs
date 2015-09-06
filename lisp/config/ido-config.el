;;; ido-config.el --- Configuration for ido

;; Copyright (C) 2005  Free Software Foundation, Inc.

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

(eval-when-compile
  (defvar ido-temp-list)
  (defvar ido-entry-buffer)
  (defvar ido-cur-item))

(defvar yh/ido-current-mode-buffers nil)

(defun yh/ido-make-buffer-list ()
  (when yh/ido-current-mode-buffers
    (setq ido-temp-list
          (delq nil
                (mapcar #'(lambda (buf)
                            (and (eq (buffer-local-value 'major-mode (get-buffer buf))
                                     yh/ido-current-mode-buffers)
                                 buf))
                        ido-temp-list)))))

(defun yh/ido-specials-to-end ()
  (let ((specials
         (delq nil (mapcar
                    (lambda (x)
                      (if (and (string-match "\\*\\(?:.*\\)\\*" x)
                               (not (string= x "*scratch*")))
                          x))
                    ido-temp-list))))
    (ido-to-end specials)))

(defun yh/ido-set-mode ()
  (interactive)
  (setq yh/ido-current-mode-buffers
        (if yh/ido-current-mode-buffers nil
          (with-current-buffer ido-entry-buffer
            major-mode)))
  (setq ido-exit 'refresh)
  (exit-minibuffer))

(defun yh/ido-setup ()
  (define-key ido-buffer-completion-map (kbd "M-m") 'yh/ido-set-mode))

(use-package flx-ido
    :ensure t
    :config
    (flx-ido-mode 1)
    (setq flx-ido-use-faces t))

(use-package ido
    :config
  (setq ido-ignore-directories '("\\`\\.\\./" "\\`\\./")
        ido-ignore-files '("\\`#" "\\`.#" "\\`\\.\\./" "\\`\\./" "Icon\n")
        ido-read-file-name-as-directory-commands
        '(ediff-directories ediff-directories3 change-context gnus-summary-save-parts)
        ido-use-filename-at-point 'guess
        ido-use-url-at-point t
        ido-max-dir-file-cache 20
        ido-create-new-buffer 'always
        ido-confirm-unique-completion t
        ido-everywhere t)

  (add-hook 'ido-make-buffer-list-hook 'yh/ido-specials-to-end)
  (add-hook 'ido-make-buffer-list-hook 'ido-summary-buffers-to-end)
  (add-hook 'ido-make-buffer-list-hook 'yh/ido-make-buffer-list)

  (add-hook 'ido-setup-hook 'yh/ido-setup)
  
  (ido-mode 1)
  
  ;; disable ido faces to see flx highlights.
  (setq ido-enable-flex-matching t)
  (setq ido-use-faces t))

(provide 'ido-config)
;;; ido-config.el ends here
