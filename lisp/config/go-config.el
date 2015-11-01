;;; go-config.el --- Configuration for go language

;; Copyright (C) 2015  Yann Hodique

;; Author: Yann Hodique <hodiquey@vmware.com>
;; Keywords: languages

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

(defun load-file-from-gopath (fname)
  (let ((gopath-items (split-string (getenv "GOPATH") ":")))
    (loop for prefix in gopath-items
       if (file-exists-p (concat prefix "/src/" fname))
         return (load-file (concat prefix "/src/" fname)))))

(defun load-file-from-gopath-or-download (pkg file)
  (let ((fname (concat pkg "/" file)))
    (unless (load-file-from-gopath fname)
      (shell-command (format "go get -u %s" pkg))
      (load-file-from-gopath fname))))

(defun yh/go-mode-hook ()
  ;; Use goimports instead of go-fmt
  (setq gofmt-command "goimports")
  ;; Call Gofmt before saving
  (add-hook 'before-save-hook 'gofmt-before-save t)

  ;; Customize compile command to run go build
  (if (not (and (stringp compile-command)
                (string-match "go" compile-command)))
      (set (make-local-variable 'compile-command)
           "go build -v && go test -v && go vet && golint"))

  ;; company-go
  (set (make-local-variable 'company-backends) '(company-go))
  (company-mode)

  ;; go-eldoc
  (go-eldoc-setup)

  (subword-mode 1)
  (glasses-mode 1))

(use-package go-eldoc
    :ensure t)

(use-package company-go
    :config (setq company-go-show-annotation t)
    :ensure t)

(use-package go-mode
    :config (progn
              (load-file-from-gopath-or-download
               "code.google.com/p/go.tools/cmd/oracle" "oracle.el")
              (load-file-from-gopath-or-download
               "github.com/dougm/goflymake" "go-flymake.el")
              (load-file-from-gopath-or-download
               "github.com/golang/lint" "misc/emacs/golint.el")

              (bind-key "M-." 'godef-jump go-mode-map)
              (require 'go-flymake)
              (add-hook 'go-mode-hook 'yh/go-mode-hook))
    :ensure t)

(provide 'go-config)
;;; go-config.el ends here
