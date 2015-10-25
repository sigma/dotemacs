;;; latex-config.el --- Configuration for latex

;; Copyright (C) 2004-2015  Yann Hodique

;; Author: Yann Hodique <Yann.Hodique@gmail.com>
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

;;; LaTeX

(defun turn-on-reftex-if-available ()
  (when (require 'reftex nil t)
    (turn-on-reftex)))

(defun LaTeX-preview-setup-if-available ()
  (when (require 'preview nil t)
    (LaTeX-preview-setup)))

(use-package auctex
    :ensure t
    :defer t
    :config
    ;; reftex helps managing references, toc, ...
    (add-hook 'LaTeX-mode-hook 'turn-on-reftex-if-available)
    ;; show/hide parts of your document
    (add-hook 'LaTeX-mode-hook 'outline-minor-mode)
    ;; preview-latex is great
    (add-hook 'LaTeX-mode-hook 'LaTeX-preview-setup-if-available)
    ;; point my typos
    (add-hook 'LaTeX-mode-hook 'flyspell-mode)
    ;; use abbrev
    (add-hook 'LaTeX-mode-hook 'abbrev-mode))

(eval-after-load 'tex
  '(setq TeX-auto-save t
    TeX-newline-function 'newline-and-indent
    TeX-parse-self t))

(eval-after-load 'latex
  '(add-to-list 'LaTeX-command-style '("omega" "lambda")))

(eval-after-load 'font-latex
  '(setq font-latex-fontify-script nil))

(use-package reftex
    :ensure t
    :defer t
    :config
    (setq reftex-plug-into-AUCTeX t
          reftex-enable-partial-scans t
          reftex-save-parse-info t
          reftex-use-multiple-selection-buffers t
          reftex-extra-bindings nil
          reftex-index-follow-mode t
          reftex-toc-follow-mode t))

(provide 'latex-config)
;;; latex-config.el ends here
