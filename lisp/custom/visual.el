;;; visual.el --- 

;; Copyright (C) 2015  Yann Hodique

;; Author: Yann Hodique;; Frame appearence <hodiquey@vmware.com>
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

(setq default-frame-alist '((left-fringe)
                            (right-fringe)
                            (menu-bar-lines . 0)
                            (vertical-scroll-bars)
                            (tool-bar-lines . 0)
                            (font . "Source Code Pro-13")))

(setq window-system-default-frame-alist
      '((x
         (width . 100)
         (height . 50)
         (foreground-color . "wheat")
         (background-color . "black")
         (cursor-color . "yellow"))
        (ns
         (width . 100)
         (height . 50)
         (foreground-color . "wheat")
         (background-color . "black")
         (cursor-color . "yellow")
         (font . "Source Code Pro-13"))))

(tool-bar-mode -1)
(set-scroll-bar-mode nil)
(menu-bar-mode -1)

(setq-default even-window-heights nil
              resize-mini-windows nil)

(setq-default indicate-buffer-boundaries 'left)
(setq-default indicate-empty-lines t)

(provide 'visual)
;;; visual.el ends here
