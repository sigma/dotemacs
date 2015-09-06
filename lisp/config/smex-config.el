;;; smex-config.el --- Smex configuration

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

(use-package smex
    :defer t
    :bind (("M-x" . smex)
           ("M-X" . smex-major-mode-commands)
           ;; This is your old M-x.
           ("C-c M-x" . execute-extended-command)
           ("A-x" . execute-extended-command))
    :config
    (smex-initialize)
    :ensure t)

(provide 'smex-config)
;;; smex-config.el ends here
