;;; bbdb-config.el ---

;; Copyright (C) 2008  Free Software Foundation, Inc.

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

(require 'rx)

(defun yh/bbdb-add-real-addresses ()
  "Don't polute my bbdb with generated addresses"
  (let ((addr net)
        (exclude-re (rx (or "@public.gmane.org"
                            "@socialcast.com"
                            (and "buzz+" (+ any) "@gmail.com")
                            "@reply.github.com"
                            "messenger@webex.com"
                            "@weboob.example.org"))))
    (not (string-match exclude-re addr))))

(autoload 'gnus-convert-face-to-png "gnus-fun")
(defun my-bbdb-display-faces ()
  "Search for face properties and display the faces."
  (let ((inhibit-read-only t)
        (default-enable-multibyte-characters nil)
        (all-records bbdb-records)
        face x-face record)
    (goto-char (point-min))
    (mapc
     (lambda (record)
       (setq x-face (bbdb-record-getprop (car record) 'x-face)
             face (bbdb-record-getprop (car record) 'face))
       ;; Display Face
       (when face
         (insert-image (create-image (gnus-convert-face-to-png face)
                                     nil t))
         (insert " "))
       ;; Display X-Face
       (when x-face
         (insert-image (gnus-create-image (uncompface x-face)
                                          nil t :face 'tooltip))
         (insert " "))
       ;; Move to the next record, suppress error on reaching last
       (condition-case nil
           (bbdb-next-record 1)
         (error nil)))
     all-records)
    ;; Remove all x-face and face lines from the display
    (goto-char (point-min))
    (save-match-data
      (while (re-search-forward "^ *\\(x-\\)?face: " nil t)
        (beginning-of-line)
        (kill-line 1)))))

(use-package bbdb
    :ensure t
    :defer t
    :config
    (bbdb-initialize 'gnus 'message)
    (setq bbdb-always-add-addresses 'yh/bbdb-add-real-addresses)

    (eval-after-load 'gnus
      '(progn
        (add-hook 'gnus-startup-hook 'bbdb-insinuate-gnus)
        (add-hook 'mail-setup-hook 'bbdb-insinuate-sendmail)

        (setq gnus-score-find-score-files-function
         '(gnus-score-find-bnews bbdb/gnus-score))
        (setq bbdb/gnus-score-default nil)))

    (eval-after-load 'message
      '(progn
        (bbdb-insinuate-message)
        (let ((func (or (and (fboundp 'bbdb-define-all-aliases)
                             'bbdb-define-all-aliases)
                        'bbdb-mail-aliases)))
          (add-hook 'message-setup-hook func))
        (add-hook 'message-mode-hook
         (lambda () (local-set-key [(meta tab)] 'bbdb-complete-name)))))

    (setq
     bbdb-offer-save 'yes
     bbdb-electric-p t
     bbdb-pop-up-target-lines 5
     bbdb-use-pop-up nil
     bbdb-north-american-phone-numbers-p nil)

    (add-hook 'bbdb-list-hook 'my-bbdb-display-faces)

    ;; Pick these headers from email messages and store them
    (setq bbdb-auto-notes-alist '(("X-Face" (".+" x-face 0 'replace))
                                  ("Face" (".+" face 0 'replace))))
    (add-hook 'bbdb-notice-hook 'bbdb-auto-notes-hook))

(provide 'bbdb-config)
;;; bbdb-config.el ends here
