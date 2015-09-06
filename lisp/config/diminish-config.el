;;; diminish-config.el --- configuration for diminish.el

;; Copyright (C) 2012  Yann Hodique

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

(use-package diminish
    :commands diminish
    :ensure diminish)

(defmacro yh/diminish-minor-after (file mode &optional to)
  `(eval-after-load ,file '(and
                            (fboundp 'diminish)
                            (diminish ,mode ,to))))

(defmacro yh/diminish-major-after (file mode &optional to)
  `(eval-after-load ,file
     '(add-hook (intern (format "%s-hook" (symbol-name ,mode)))
                (lambda () (setq mode-name (or ,to ""))))))

(yh/diminish-minor-after "filladapt" 'filladapt-mode)
(yh/diminish-minor-after "paredit" 'paredit-mode "()")
(yh/diminish-minor-after "eldoc" 'eldoc-mode "doc")
(yh/diminish-minor-after "hideshow" 'hs-minor-mode " ±")
(yh/diminish-minor-after "whitespace" 'whitespace-mode)
(yh/diminish-minor-after "whitespace" 'global-whitespace-mode)
(yh/diminish-minor-after "org" 'orgstruct-mode " …")
(yh/diminish-minor-after "undo-tree" 'undo-tree-mode " ⎌")
;;(yh/diminish-minor-after "magit" 'magit-auto-revert-mode)
(yh/diminish-minor-after "subword" 'subword-mode)

(yh/diminish-minor-after "magit-topgit" 'magit-topgit-mode "top")
(yh/diminish-minor-after "magit-gh-pulls" 'magit-gh-pulls-mode "pull")

(yh/diminish-major-after "lisp-mode" 'emacs-lisp-mode "Elisp")

(provide 'diminish-config)
;;; diminish-config.el ends here
