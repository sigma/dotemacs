;;; magit-config.el --- configuration for Magit      -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Yann Hodique

;; Author: Yann Hodique <hodiquey@vmware.com>
;; Keywords: 

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(use-package magit
    :ensure t
    :config
    (when (fboundp 'magit-load-config-extensions)
      (add-hook 'magit-mode-hook 'magit-load-config-extensions)))

(use-package magit-gh-pulls
    :ensure t)

(use-package magit-topgit
    :ensure t)

(provide 'magit-config)
;;; magit-config.el ends here
