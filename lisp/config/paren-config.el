;;; paren-config.el --- Configuration for paren

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

;;; Parenthesis

;; Use mic-paren in replacement of standard paren.el
(use-package mic-paren
    :ensure t
    :config
    (paren-activate)                      ; activating
    (add-hook 'c-mode-common-hook
              (function (lambda ()
                (paren-toggle-open-paren-context 1))))
    ;; In LaTeX-mode we want this
    (add-hook 'LaTeX-mode-hook
              (function (lambda ()
                (paren-toggle-matching-quoted-paren 1)
                (paren-toggle-matching-paired-delimiter 1)))))

;; Fancy paren highlighting
(use-package cparen
    :ensure t
    :config
    (setq cparen-mini-font-lock-keywords cparen-font-lock-keywords)
    (cparen-activate))

(provide 'paren-config)
;;; paren-config.el ends here
