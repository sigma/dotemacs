;;; multi-smtpmail.el --- support for multiple SMTP servers

;; Copyright (C) 2011  Free Software Foundation, Inc.

;; Author: Yann Hodique <yhodique@vmware.com>
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

;; Configuration example:

;; (setq smtpmail-starttls-credentials '(("smtp.gmail.com" 587 "" "")
;;                                       ("" 25 "" "")))
;; (add-to-list 'multi-smtpmail-profiles
;;              `(example ,(rx "me@example.com")
;;                        (smtpmail-smtp-server "smtp.example.com")
;;                        (smtpmail-smtp-service 25)
;;                        (smtpmail-local-domain "example.com")))
;; (add-to-list 'multi-smtpmail-profiles
;;              `(gmail ,(rx "me@gmail.com")
;;                      (smtpmail-smtp-server "smtp.gmail.com")
;;                      (smtpmail-smtp-service 587)
;;                      (smtpmail-local-domain "home")))
;; (multi-smtpmail-set-default-profile 'gmail)

;; Then, depending on the "From" email address, the right profile will be
;; chosen. In case there is no match, we can fallback to a predefined profile
;; ('gmail in this case)

;;; Code:

(defvar multi-smtpmail-profiles nil)

(defvar multi-smtpmail-default-profile-name nil)

(defvar multi-smtpmail-temporary-profile nil)

(defun multi-smtpmail-activate-profile (profile)
  (setq multi-smtpmail-temporary-profile nil)
  (let* ((p (assoc profile multi-smtpmail-profiles))
         (vars (cddr p)))
    (dolist (v vars)
      (let ((sym (car v)))
        (add-to-list sym (symbol-value sym))
        (set sym (cadr v))))))

(defun multi-smtpmail-deactivate-profile ()
  (when multi-smtpmail-temporary-profile
    (dolist (v multi-smtpmail-temporary-profile)
      (set (car v) (cadr v)))))

(defun multi-smtpmail-set-default-profile (profile)
  (setq multi-smtpmail-default-profile-name profile)
  (multi-smtpmail-activate-profile profile))

(defun multi-smtpmail-get-default-profile ()
  multi-smtpmail-default-profile-name)

(defun multi-smtpmail-find-profile (email)
  (catch 'found
    (dolist (profile multi-smtpmail-profiles)
      (let ((rxp (nth 1 profile)))
        (when (string-match rxp email)
          (throw 'found (nth 0 profile)))))
    (multi-smtpmail-get-default-profile)))

(defadvice smtpmail-via-smtp
  (around smtpmail-via-smtp-around
          (recipient smtpmail-text-buffer &optional ask-for-password) act)
  (let ((from (mail-fetch-field "From")))
    (when from
      (multi-smtpmail-activate-profile
       (multi-smtpmail-find-profile
        (nth 1 (mail-extract-address-components from)))))
    (prog1
        ad-do-it
      (multi-smtpmail-deactivate-profile))))

(provide 'multi-smtpmail)
;;; multi-smtpmail.el ends here
