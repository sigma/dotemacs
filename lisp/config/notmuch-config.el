;;; notmuch-config.el --- configuration for notmuch

;; Copyright (C) 2011  Yann Hodique.
;; Copyright (C) 2009  Tassilo Horn.

;; Author: Yann Hodique <yann.hodique@gmail.com>
;; Keywords: mail

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

;; Inpired by code found on the net:
;; - http://article.gmane.org/gmane.emacs.gnus.user/13308
;; - http://www.emacswiki.org/emacs/NotMuch

;; This is an attempt to generalize the approach and become as
;; setup-independant as possible.
;; To achieve this we rely on the gnus configuration (select methods) to find
;; out what is indexed where.

;;; Code:

(use-package notmuch
    :ensure t
    :defer t
    :config
    (defvar yh/notmuch-gnus-dirs '())

    (defun yh/notmuch-compute-gnus-dirs ()
      "Use defined select methods to discover mapping between
filesystem and gnus groups.

Explicit information can be added in select methods definitions, in case the filesystem mapping is not direct, like:
 (add-to-list 'gnus-secondary-select-methods
              '(nnimap \"imap\"
                       ...
                       (notmuch-root \"/home/user/Maildir\")
                       (notmuch-dir-levels 2)))

Otherwise, for filesystem-based backends (nnml and nnmaildir for
now), we try to guess the relevant information from method definition."
      (let ((mapping nil))
        (dolist (smethod (append (list gnus-select-method gnus-message-archive-method)
                                 gnus-secondary-select-methods))
          (let ((backend (car smethod))
                (server (cadr smethod))
                (props (cddr smethod)))
            (let* ((explicit-target (cadr (assoc 'notmuch-root props)))
                   (explicit-dir-levels (cadr (assoc 'notmuch-dir-levels props)))
                   (target
                    (or explicit-target
                        (cond ((eq backend 'nnml)
                               (cadr (assoc 'nnml-directory props)))
                              ((eq backend 'nnmaildir)
                               (cadr (assoc 'directory props))))))
                   (levels
                    (or explicit-dir-levels
                        (cond ((eq backend 'nnml) 1)
                              ((eq backend 'nnmaildir) 2))
                        1)))
              (when target
                (push (list (expand-file-name target)
                            (concat (format "%s" backend)
                                    (when (and server (not (string-equal server "")))
                                      (format "+%s" server)))
                            levels)
                      mapping)))))
        (setq yh/notmuch-gnus-dirs mapping)))

    (defun yh/notmuch-gnus-group-path (file n)
      (let ((path file))
        (dotimes (x n)
          (setq path (directory-file-name (file-name-directory path))))
        path))

    (defun yh/notmuch-file-to-group (file)
      "Calculate the Gnus group name from the given file name."
      (unless yh/notmuch-gnus-dirs
        (yh/notmuch-compute-gnus-dirs))

      (let ((group nil))
        (dolist (group-dir yh/notmuch-gnus-dirs)
          (let* ((path (nth 0 group-dir))
                 (gr (nth 1 group-dir))
                 (lev (nth 2 group-dir))
                 (group-path (yh/notmuch-gnus-group-path file lev)))
            (when (string-match (concat "^\\("
                                        (regexp-quote path)
                                        "\\)\\(.*\\)")
                                group-path)
              (setq group (concat gr ":" (match-string 2 group-path))))))
        group))

    (defun yh/notmuch-goto-message-in-gnus ()
      "Open a summary buffer containing the current notmuch
article."
      (interactive)
      (unless (gnus-alive-p) (with-temp-buffer (gnus)))
      (let ((group (yh/notmuch-file-to-group (notmuch-show-get-filename)))
            (message-id
             (replace-regexp-in-string "\"" ""
                                       (replace-regexp-in-string "^id:" ""
                                                                 (notmuch-show-get-message-id)))))
        (if (and group message-id)
            (org-gnus-follow-link group message-id)
            (message "Couldn't get relevant infos for switching to Gnus."))))

    (defun yh/gnus-goto-message-in-notmuch ()
      (interactive)
      (let ((message-id (mail-header-id
                         (gnus-summary-article-header))))
        (notmuch-search (format "id:%s" (substring message-id 1 (1- (length message-id)))))))

    (define-key notmuch-show-mode-map (kbd "C-c C-c") 'yh/notmuch-goto-message-in-gnus)
    (define-key gnus-summary-mode-map (kbd "C-c C-c") 'yh/gnus-goto-message-in-notmuch)
    )

(provide 'notmuch-config)
;;; notmuch-config.el ends here
