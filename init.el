;; -*- mode: emacs-lisp; auto-compile-lisp: nil; -*-

;;; init.el --- Emacs configuration

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

;; (toggle-debug-on-error)

(require 'package)

(setq package-archives
      '(("org" . "http://orgmode.org/elpa/")
        ("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.milkbox.net/packages/")
        ("melpa-stable" . "http://melpa-stable.milkbox.net/packages/")
        ("marmalade" . "https://marmalade-repo.org/packages/")
        ("elpy" . "http://jorgenschaefer.github.io/packages/")))

(package-initialize)

(setq load-path (append (directory-files "~/.emacs.d/lisp" t "^[^.]")
                        load-path))

(require 'patches)
(require 'visual)
(require 'use-package)

(use-package diminish
    :ensure t)

(prefer-coding-system 'utf-8)

(defun yh/load-configuration-file (filename)
  (let ((file (expand-file-name filename)))
    (if (file-exists-p file)
        (load-file file))))

;; load code specific to some major version
(yh/load-configuration-file (format "~/.emacs.d/%d.el" emacs-major-version))

;; load code specific to some minor version
(yh/load-configuration-file (format "~/.emacs.d/%d-%d.el"
                                    emacs-major-version emacs-minor-version))

;; Load site-specific stuff: paths, accounts, projects...
(yh/load-configuration-file "~/.emacs.d/local.el")

;; Customizations are in a separate file
(yh/load-configuration-file "~/.emacs.d/cust.el")

(require 'modes-config)
(require 'path-config)
(require 'savehist-config)
(require 'smex-config)
(require 'time-stamp-config)
(require 'avoid-config)
(require 'abbrev-config)
(require 'diminish-config)
(require 'buffer-config)
(require 'org-config)
(require 'calendar-config)
;; (require 'hideshow-config)
(require 'ido-config)
(require 'bbdb-config)
(require 'eshell-config)
;; (require 'help-config)
(require 'moccur-config)
;; (require 'bm-config)
;; (require 'cc-config)
(require 'compile-config)
(require 'ediff-config)
;; (require 'hl-line-config)
(require 'info-config)
(require 'latex-config)
;; (require 'highlight-changes-config)
(require 'paren-config)
(require 'lisp-config)
;; (require 'slime-config)
(require 'shell-config)
(require 'go-config)
(require 'python-config)
;; (require 'tags-config)
;; (require 'completion-config)
(require 'crontab-config)
(require 'yasnippet-config)
(require 'web-config)
;; (require 'magit-config)
;; (require 'scratch-config)
(require 'notmuch-config)
(require 'gist-config)
(require 'markdown-config)
(require 'trello-config)

(use-package avy
    :ensure t)

(use-package ido-completing-read+
    :ensure t)

(use-package magit
    :ensure t)

(use-package magit-gh-pulls
    :ensure t)

(use-package helm
    :ensure t)

(use-package helm-projectile
    :ensure t)

(use-package helm-ag
    :ensure t)

(use-package projectile
    :ensure t
    :config
    (setq projectile-enable-caching t)
    (projectile-global-mode)
    (setq projectile-completion-system 'helm)
    (helm-projectile-on)
    (setq projectile-switch-project-action 'helm-projectile))

(use-package ag
    :ensure t)

(use-package noflet
    :ensure t)

(use-package boxquote
    :ensure t)

(use-package multiple-cursors
    :ensure t
    :config
    (bind-key "C-+" 'mc/insert-numbers mc/keymap))

(use-package dockerfile-mode
    :ensure t
    :defer t
    :mode "\\Dockerfile\\'")

(use-package jinja2-mode
    :ensure t
    :defer t
    :mode "\\.j2\\'")

(add-to-list 'tramp-methods
             '("vcsh"
               (tramp-login-program "/usr/local/bin/vcsh")
               (tramp-login-args
                (("enter")
                 ("%h")))
               (tramp-remote-shell "/bin/sh")
               (tramp-remote-shell-args
                ("-c"))))

;; TODO: empty that file, and remove
(require 'yh-misc)

(provide 'init)
;;; init.el ends here
