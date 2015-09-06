;;; rst-config.el --- ReST configuration

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

(eval-after-load "rst"
  '(progn
    (defun yh/orgtbl-to-rst-paddings (table)
      (let* ((pruned-table (remove 'hline table))
             (size-table (mapcar (lambda (row)
                                   (mapcar #'length row))
                                 pruned-table)))
        (apply #'mapcar* #'max size-table)))

    (defun yh/orgtbl-padded-hline (paddings &optional chr)
      (let ((chr (or chr ?-)))
        (concat (format "+%c" chr)
                (mapconcat (lambda (size)
                             (make-string size chr)) paddings
                             (format "%c+%c" chr chr))
                (format "%c+" chr))))

    (defun yh/orgtbl-to-rst (table params)
      "Convert the Orgtbl mode TABLE to ReST."
      (let* ((indent (make-string (or (plist-get params :indent) 0) ?\ ))
             (paddings (yh/orgtbl-to-rst-paddings table))
             (hline (concat indent (yh/orgtbl-padded-hline paddings)))
             (hlend (concat indent (yh/orgtbl-padded-hline paddings ?=)))
             (lfmt (concat indent "| "
                           (mapconcat (lambda (size)
                                        (format "%%-%ds" size)) paddings
                                        " | ") " |"))
             (hlfmt (concat lfmt "\n" hlend))
             (params2
              (list
               :tstart (concat "\n" hline) :tend (concat hline "\n") :hline hline
               :lfmt lfmt :hlfmt hlfmt :skipheadrule t)))
        (orgtbl-to-generic table (org-combine-plists params2 params))))))

(provide 'rst-config)
;;; rst-config.el ends here
