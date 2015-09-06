;;; avoid-config.el --- Avoid configuration

;; Copyright (C) 2015  Yann Hodique

;; Author: Yann Hodique;; Throw away the mouse when typing <hodiquey@vmware.com>
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

(use-package avoid
    :commands mouse-avoidance-mode
    :defer 2
    :config (mouse-avoidance-mode 'exile))

(provide 'avoid-config)
;;; avoid-config.el ends here
