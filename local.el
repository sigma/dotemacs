;;; local.el --- Local configuration

;; Copyright (C) 2015  Yann Hodique

;; Author: Yann Hodique <hodiquey@vmware.com>
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

(setq user-mail-address "hodiquey@vmware.com")
(setq message-alternative-emails (rx "yann.hodique@gmail.com"))
(setq home-directory (getenv "HOME"))

(setenv "GOPATH" "/Users/hodiquey/Projects/go/:/Users/hodiquey/Projects/git/gocode")
(eval-after-load 'go-mode
  '(progn
    (load-file "~/Projects/go/src/code.google.com/p/go.tools/cmd/oracle/oracle.el")
    (load-file "~/Projects/go/src/github.com/dougm/goflymake/go-flymake.el")
    (load-file "~/Projects/go/src/github.com/golang/lint/misc/emacs/golint.el")))

(provide 'local)
;;; local.el ends here
