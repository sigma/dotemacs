;;; time-stamp-config.el --- Time-stamp configuration

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

(use-package time-stamp
    :commands time-stamp
    :init (add-hook 'before-save-hook 'time-stamp)
    :config (progn
              ;; Turn on time stamping
              (setq time-stamp-active t)
              ;; Sets new format for the time stamp, also used with the creation tag.
              (setq time-stamp-format "%02d/%02m/%:y %02H:%02M:%02S %U")))

(provide 'time-stamp-config)
;;; time-stamp-config.el ends here
