;;; lisp-config.el --- Configuration for lisp

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

;;; Lisp

;; byte-compile current elisp buffer
(defvar auto-compile-lisp t)

(defun byte-compile-elisp ()
  "Byte compile the current buffer if possible"
  (if (and auto-compile-lisp
           (not (backup-file-name-p (buffer-file-name))))
      (cond
       ((eq major-mode 'sawfish-mode)
        (sawfish-compile-this-file))
       ((eq major-mode 'emacs-lisp-mode)
        (byte-compile-file (buffer-file-name))))))

(use-package edebug)

(use-package paredit
    :ensure t
    :config
    (defun paredit-barf-all-the-way-backward ()
      (interactive)
      (paredit-split-sexp)
      (paredit-backward-down)
      (paredit-splice-sexp))

    (defun paredit-barf-all-the-way-forward ()
      (interactive)
      (paredit-split-sexp)
      (paredit-forward-down)
      (paredit-splice-sexp)
      (if (eolp) (delete-horizontal-space)))

    (defun paredit-slurp-all-the-way-backward ()
      (interactive)
      (catch 'done
        (while (not (bobp))
          (save-excursion
            (paredit-backward-up)
            (if (eq (char-before) ?\()
                (throw 'done t)))
          (paredit-backward-slurp-sexp))))

    (defun paredit-slurp-all-the-way-forward ()
      (interactive)
      (catch 'done
        (while (not (eobp))
          (save-excursion
            (paredit-forward-up)
            (if (eq (char-after) ?\))
                (throw 'done t)))
          (paredit-forward-slurp-sexp))))

    (nconc paredit-commands
           '("Extreme Barfage & Slurpage"
             (("C-M-)")
              paredit-slurp-all-the-way-forward
              ("(foo (bar |baz) quux zot)"
               "(foo (bar |baz quux zot))")
              ("(a b ((c| d)) e f)"
               "(a b ((c| d)) e f)"))
             (("C-M-}" "M-F")
              paredit-barf-all-the-way-forward
              ("(foo (bar |baz quux) zot)"
               "(foo (bar|) baz quux zot)"))
             (("C-M-(")
              paredit-slurp-all-the-way-backward
              ("(foo bar (baz| quux) zot)"
               "((foo bar baz| quux) zot)")
              ("(a b ((c| d)) e f)"
               "(a b ((c| d)) e f)"))
             (("C-M-{" "M-B")
              paredit-barf-all-the-way-backward
              ("(foo (bar baz |quux) zot)"
               "(foo bar baz (|quux) zot)"))))
    (paredit-define-keys)
    (paredit-annotate-mode-with-examples)
    (paredit-annotate-functions-with-examples)

    (define-key paredit-mode-map (kbd "<C-right>") 'forward-word)
    (define-key paredit-mode-map (kbd "<C-left>") 'backward-word)
    (define-key paredit-mode-map (kbd "<C-M-right>") 'forward-sexp)
    (define-key paredit-mode-map (kbd "<C-M-left>") 'backward-sexp))

(defun yh/lisp-hook()
  (turn-on-eldoc-mode)
  (hs-minor-mode 1)
  (paredit-mode 1)
  (eldoc-add-command
   'paredit-backward-delete
   'paredit-close-round))

(defun yh/emacs-lisp-hook ()
  ;; make sure modifs are taken into account (use with caution)
  (add-hook 'after-save-hook 'byte-compile-elisp t t))

(add-mhook '(emacs-lisp-mode-hook lisp-interaction-mode-hook ielm-mode-hook
             lisp-mode-hook slime-repl-mode-hook)
           'yh/lisp-hook t)

(add-hook 'emacs-lisp-mode-hook 'yh/emacs-lisp-hook)

(defun yh/insert-elisp-key ()
  (interactive)
  (insert (concat "(kbd \""
                  (help-key-description (read-key-sequence "Key: ") nil)
                  "\")")))

(define-key emacs-lisp-mode-map (kbd "C-c C-c") 'emacs-lisp-byte-compile)
(define-key emacs-lisp-mode-map (kbd "C-c k") 'yh/insert-elisp-key)

(font-lock-add-keywords 'emacs-lisp-mode
                        `((,(concat "\\<" (regexp-opt
                                           '("add-hook" "add-mhook"
                                             "autoload" "defmadvice"
                                             "aset" "set" "fset"
                                             "remove-hook" "clear-hook"
                                             "request" "make-double-command"
                                             "ert-deftest" "deftest"
                                             "defcodex" "in-codex") t)
                                    "\\>[ 	']*\\(\\sw+\\)?")
                            (1 font-lock-keyword-face)
                            (2 font-lock-constant-face nil t))
                          ))

(defun define-lisp-indent-function (sym val)
  (put sym 'lisp-indent-function val))

(put 'define-lisp-indent-function 'safe-local-eval-function t)
(put 'font-lock-add-keywords 'safe-local-eval-function t)

(use-package cl-indent
    :config
  (let ((l '((flet ((&whole 4 &rest (&whole 1 &lambda &body)) &body))
             (cl-flet* . flet)
             (labels . flet)
             (cl-flet . flet)
             (cl-labels . flet)
             (cl-macrolet . flet))))
    (dolist (el l)
      (put (car el) 'common-lisp-indent-function
           (if (symbolp (cdr el))
               (get (cdr el) 'common-lisp-indent-function)
               (car (cdr el)))))))

(dolist (mode '(emacs-lisp-mode lisp-interaction-mode))
  (font-lock-add-keywords
   mode
   '(("(\\<\\(cl-flet[*]?\\|cl-labels\\|cl-macrolet\\)\\>" 1
      font-lock-keyword-face)
     ("(\\<\\(cl-loop\\|cl-dolist\\)\\>" 1 font-lock-keyword-face))))

(setq lisp-indent-function 'common-lisp-indent-function)

(put 'progn 'common-lisp-indent-function '(&rest 2))
(put 'quote 'common-lisp-indent-function '(&rest 2))
(put 'if 'common-lisp-indent-function '(4 4 &rest 2))

(defalias 'λ 'lambda)

;; Elisp go-to-definition with M-. and back again with M-,
(use-package elisp-slime-nav
    :ensure t
    :diminish elisp-slime-nav-mode
    :config
    (add-hook 'emacs-lisp-mode-hook (lambda () (elisp-slime-nav-mode t))))

(provide 'lisp-config)
;;; lisp-config.el ends here
