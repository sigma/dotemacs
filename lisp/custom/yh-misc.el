(make-double-command my-home ()
  "Go to beginning of line, or beginning of buffer."
  nil
  (beginning-of-line)
  (beginning-of-buffer))

(make-double-command my-end ()
  "Go to end of line, or end of buffer."
  nil
  (end-of-line)
  (end-of-buffer))

;; make my scripts executable
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;; I hate trailing whitespaces (use with caution)
;;(add-hook 'write-file-hooks 'delete-trailing-whitespace)

;; I also hate additional blank-lines (use with extreme caution)
;;(add-hook 'write-file-hooks 'simplify-blank-lines)

(defvar main-frame nil)

;; Adaptative "exit" behavior
(defun exit-no-gnuserv-or-frame ()
  "If in a gnuserv-edit session, close it. If in a secondary
frame, close it. Else, die"
  (interactive)
  (cond ((and (boundp 'gnuserv-minor-mode) gnuserv-minor-mode)
         (gnuserv-edit))
        ((or (not main-frame) (eq (selected-frame) main-frame))
         (save-buffers-kill-emacs))
        ((delete-frame))))

;; make active frame the main one
(defun make-main-frame ()
  "Make the current frame the primary one"
  (interactive)
  (setq main-frame (selected-frame)))

(require 'server)

(make-main-frame)

;; tidy up diffs when closing the file
(defun kill-associated-diff-buf ()
  (let ((buf (get-buffer (concat "*Assoc file diff: "
                                 (buffer-name)
                                 "*"))))
    (when (bufferp buf)
      (kill-buffer buf))))

(add-hook 'kill-buffer-hook 'kill-associated-diff-buf)

;;;_* Global key bindings

(global-set-key (kbd "<C-backspace>") 'kill-syntax-backward)

(global-set-key (kbd "C-c +") 'incr-dwim)
(global-set-key (kbd "C-c -") 'decr-dwim)

;; Depending on your keyboard you may want another one binding
(global-set-key (kbd "C-x ~") 'previous-error)
(global-set-key (kbd "C-c s") 'eshell)
(global-set-key (kbd "C-c c") 'compile)
(global-set-key (kbd "C-c o") 'my-occur)
(global-set-key (kbd "C-c e") 'fc-eval-and-replace)
(global-set-key (kbd "C-c f") 'find-function)
(global-set-key (kbd "C-c F") 'find-function-on-key)
(global-set-key (kbd "C-c v") 'find-variable)

;; Enter a recursive edit. C-M-c will bring back exactly there
(global-set-key (kbd "C-c r") (lambda ()
                                (interactive)
                                (save-window-excursion
                                  (save-excursion
                                    (recursive-edit)))))

(global-set-key (kbd "<C-tab>") 'other-window)

;; These were traditional bindings, why did they change??
(global-set-key (kbd "<home>") 'beginning-of-buffer)
(global-set-key (kbd "<end>") 'end-of-buffer)
(global-set-key (kbd "<delete>") 'delete-char)

;; "intelligent" home and end
(global-set-key (kbd "<C-home>") 'my-home)
(global-set-key (kbd "<C-end>") 'my-end)

;; rectangle bindings. don't mix with registers! :)
(global-set-key (kbd "C-x r C-SPC") 'rm-set-mark)
(global-set-key (kbd "C-x r C-x") 'rm-exchange-point-and-mark)
(global-set-key (kbd "C-x r C-w") 'rm-kill-region)
(global-set-key (kbd "C-x r M-w") 'rm-kill-ring-save)
(global-set-key (kbd "C-x r C-y") 'yank-rectangle)

(global-set-key (kbd "C-x C-c") 'exit-no-gnuserv-or-frame)
(global-set-key (kbd "C-x 5 3") 'make-main-frame)
(global-set-key (kbd "C-c d") 'diff-buffer-with-associated-file)
(global-set-key (kbd "C-x k") 'de-context-kill)
(global-set-key (kbd "C-x K")
                (lambda () (interactive)
                  (dolist (buf (buffer-list))
                    (when (buffer-file-name buf)
                      (kill-buffer buf)))))

(global-set-key (kbd "C-c h") 'auto-insert)

(define-key isearch-mode-map (kbd "C-o")
  (lambda ()
    (interactive)
    (let ((case-fold-search isearch-case-fold-search))
      (occur (if isearch-regexp isearch-string
               (regexp-quote isearch-string))))))

;; versioning keys
(defvar yh/vcs-backends
  '((git . magit-status)
    (svn . svn-status)
    (cvs . cvs-status)))

;; fallback to git by default
(require 'vc-git)
(require 'vc-svn)
(require 'vc-cvs)
(defun yh/vcs-backend (file)
  (cond ((null file) 'git)
        ((vc-git-root file) 'git)
        ((vc-svn-registered file) 'svn)
        ((vc-cvs-registered file) 'cvs)
        (t 'git)))

(defun yh/vcs-status ()
  (interactive)
  (let ((backend (yh/vcs-backend (or buffer-file-name default-directory))))
    (call-interactively (cdr (assoc backend yh/vcs-backends)))))

(global-set-key (kbd "C-c g") 'yh/vcs-status)

;;; windmove :)
(mapc #'eval
      (mapcar #'(lambda (dir)
                  (let ((name (symbol-name dir)))
                    `(global-set-key (kbd ,(format "C-x <%s>" name))
                                     ',(intern (concat "windmove-" name)))))
              '(left right up down)))

;; xterm settings
(when (string= "xterm" (getenv "TERM"))
  (require 'xterm-extras)
  (xterm-extra-keys))

;;;_* Experimental

(use-package cmake-mode
    :mode "\\(CMakeLists\\.txt\\|\\.cmake\\)\\'"
    :defer t
    :ensure t)

;; Just in case compose is broken...
(define-key key-translation-map (kbd "<Multi_key>") 'iso-transl-ctl-x-8-map)
(autoload 'iso-transl-ctl-x-8-map "iso-transl"
  "Keymap for C-x 8 prefix." t 'keymap)

(use-package pp-c-l
    :ensure t
    :defer t
    :config
    (pretty-control-l-mode 1))

(use-package graphviz-dot-mode
    :mode "\\.dot\\'"
    :ensure t
    :defer t)

;; (require 'incr)
;; (delq 'rotate incr-enable-feature)

(add-to-list 'auto-mode-alist '("\\.djava\\'" . java-mode))
(add-to-list 'auto-mode-alist '("\\.pro\\'" . makefile-mode))
(add-to-list 'auto-mode-alist '("\\.make\\'" . makefile-mode))

(global-set-key (kbd "C-x t") 'anchored-transpose)
(autoload 'anchored-transpose "anchored-transpose" nil t)

(setq server-socket-file "/tmp/emacs1000/server")
(unless (file-exists-p server-socket-file)
  (server-start))

;;; Startup code
(when (and (boundp 'org-default-notes-file)
           (file-exists-p org-default-notes-file))
  (find-file org-default-notes-file)
  (require 'calendar)
  (call-interactively 'org-agenda-list))

(eval-after-load "js2-mode"
  '(add-hook 'js2-mode (lambda ()
                         (glasses-mode 1)
                         (subword-mode 1))))

(setq js2-use-font-lock-faces t)

(require 'epa-dired)
(epa-file-enable)
(setq epa-file-cache-passphrase-for-symmetric-encryption t)
(add-hook 'mail-mode-hook 'epa-mail-mode)

(autoload 'kill-ring-search "kill-ring-search"
  "Search the kill ring in the minibuffer."
  (interactive))
(global-set-key "\M-\C-y" 'kill-ring-search)

(when (require 'haskell-mode nil t)
  (add-hook 'haskell-mode-hook
            #'(lambda ()
                (setq comment-padding " ")
                (setq comment-start "--"))))

(eval-after-load 'nxml-mode
  '(defun bf-pretty-print-xml-region (begin end)
     "Pretty format XML markup in region. You need to have nxml-mode
http://www.emacswiki.org/cgi-bin/wiki/NxmlMode installed to do
this.  The function inserts linebreaks to separate tags that have
nothing but whitespace between them.  It then indents the markup
by using nxml's indentation rules."
     (interactive "r")
     (save-excursion
       (nxml-mode)
       (goto-char begin)
       (while (search-forward-regexp "\>[ \\t]*\<" nil t)
         (backward-char) (insert "\n"))
       (indent-region begin end))
     (message "Ah, much better!")))

(when (require 'test-case-mode nil t)
  (add-hook 'find-file-hook 'enable-test-case-mode-if-test))

(when (and (require 'folding nil t)
           (require 'fold-dwim nil t))
  (global-set-key (kbd "<A-tab>") 'fold-dwim-toggle))


;; Detect endianness of UTF-16 containing a Byte Order Mark U+FEFF
(add-to-list 'auto-coding-regexp-alist '("^\xFF\xFE" . utf-16-le) t)
(add-to-list 'auto-coding-regexp-alist '("^\xFE\xFF" . utf-16-be) t)
;; Add missing support functions
(defun utf-16-le-pre-write-conversion (start end) nil)
(defun utf-16-be-pre-write-conversion (start end) nil)

;; Detect endianness of UTF-16 containing a Byte Order Mark U+FEFF
;; Detect EOL mode by looking for CR/LF on the first line
(add-to-list 'auto-coding-regexp-alist
             '("^\xFF\xFE.*\x0D\x00$" . utf-16-le-dos) t)
(add-to-list 'auto-coding-regexp-alist
             '("^\xFE\xFF.*\x0D\x00$" . utf-16-be-dos) t)
(add-to-list 'auto-coding-regexp-alist '("^\xFF\xFE" . utf-16-le) t)
(add-to-list 'auto-coding-regexp-alist '("^\xFE\xFF" . utf-16-be) t)

(let ((arch-regexp "\\.\\(war\\|ear\\|sar\\|egg\\|dar\\|package\\)\\'"))
  (add-to-list 'auto-mode-alist `(,arch-regexp . archive-mode))
  (modify-coding-system-alist 'file arch-regexp 'no-conversion))

(setq auto-coding-functions (delete 'sgml-xml-auto-coding-function
                                    auto-coding-functions))

(use-package indirect-config
    :commands indirect-region
    :bind ("C-x n N" . indirect-region))

(use-package data-debug
    :commands data-debug-eval-expression
    :bind (("M-:" . data-debug-eval-expression)))

(use-package iedit
    :commands iedit-mode
    :ensure t
    :defer t
    :bind (("C-;" . iedit-mode)))

(use-package yaml-mode
    :ensure t
    :defer t
    :mode ("\\.ya?ml\\'" . yaml-mode))

(use-package puppet-mode
    :ensure t
    :defer t
    :mode ("\\.pp\\'" . puppet-mode))

(ignore-errors
  (require 'ansi-color)
  (defun my-colorize-compilation-buffer ()
    (when (eq major-mode 'compilation-mode)
      (ansi-color-apply-on-region compilation-filter-start (point-max))))
  (add-hook 'compilation-filter-hook 'my-colorize-compilation-buffer))

;; TODO: needs some work
;;;###autoload
(defun yh/c-rearrange-electrics ()
  "Rearrange electric chars according to current c-style"
  (interactive)
  (save-excursion
    (mapcar (lambda (symb)
              (goto-char (point-min))
              (while (search-forward (car symb) (point-max) t)
                (let ((p (- (point) 1)))
                  (back-to-indentation)
                  (if (equal p (point))
                      (progn
                        (delete-indentation)
                        (if (eq (cdr symb) '+)
                            (forward-char)))
                    (goto-char p))
                  (delete-char 1)
                  (execute-kbd-macro (car symb)))))
            '(("{" +) ("}" -)))))

;;;###autoload
(defun simplify-blank-lines ()
  "Delete extra blank lines"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (search-forward-regexp (string ?^ 10 10) nil t)
      (backward-char)
      (delete-blank-lines))))

;;;###autoload
(defun find-file-guessing (arg)
  "Call find-file with file at point if valid. With a universal argument, force call to find-file"
  (interactive "P")
  (let ((target (and (not arg) (request 'ffap) (ffap-guesser))))
    (if target
        (ffap)
      (call-interactively 'find-file))))

;; convert a buffer from dos ^M end of lines to unix end of lines
;;;###autoload
(defun dos2unix ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (search-forward "\r" nil t) (replace-match ""))))

;; vice versa
;;;###autoload
(defun unix2dos ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (search-forward "\n" nil t) (replace-match "\r\n"))))

;; ASCII table function
;;;###autoload
(defun ascii-table ()
  "Print the ascii table. Based on a defun by Alex Schroeder <asc@bsiag.com>"
  (interactive)  (switch-to-buffer "*ASCII*")  (erase-buffer)
  (insert (format "ASCII characters up to number %d.\n" 254))
  (let ((i 0))
    (while (< i 254)      (setq i (1+ i))
           (insert (format "%4d %c\n" i i))))  (beginning-of-buffer))

;;;###autoload
(defun yank-and-forward-line ()
  (interactive)
  (let ((old-col (current-column)))
    (yank)
    (forward-line)
    (while (and (not (eolp)) (> old-col 0))
      (forward-char)
      (setq old-col (1- old-col)))))

;;;###autoload
(defun totd ()
  (interactive)
  (with-output-to-temp-buffer "*Tip of the day*"
    (let* ((commands (loop for s being the symbols
                           when (commandp s) collect s))
           (command (nth (random (length commands)) commands)))
      (princ
       (concat "Your tip for the day is:\n========================\n\n"
               (describe-function command)
               "\n\nInvoke with:\n\n"
               (with-temp-buffer
                 (where-is command t)
                 (buffer-string)))))))

;;;###autoload
(defun kill-syntax-forward ()
  "Kill characters with syntax at point."
  (interactive)
  (let ((beg (point)))
    (skip-syntax-forward (string (char-syntax (char-after))))
    (kill-region beg (point))))

;;;###autoload
(defun kill-syntax-backward ()
  "Kill characters with syntax at point."
  (interactive)
  (let ((beg (point)))
    (skip-syntax-backward (string (char-syntax (char-before))))
    (kill-region beg (point))))

;;;###autoload
(defun my-occur ()
  "Switch to *Occur* buffer, or run `moccur'."
  (interactive)
  (if (get-buffer "*Moccur*")
      (switch-to-buffer "*Moccur*")
    (call-interactively 'moccur)))

;;;###autoload
(defun diff-buffer-with-associated-file ()
  "View the differences between BUFFER and its associated file.
This requires the external program \"diff\" to be in your `exec-path'.
Returns nil if no differences found, 't otherwise."
  (interactive)
  (let ((buf-filename buffer-file-name)
        (buffer (current-buffer)))
    (unless buf-filename
      (error "Buffer %s has no associated file" buffer))
    (let ((diff-buf (get-buffer-create
                     (concat "*Assoc file diff: "
                             (buffer-name)
                             "*"))))
      (with-current-buffer diff-buf
        (setq buffer-read-only nil)
        (erase-buffer))
      (let ((tempfile (make-temp-file "buffer-to-file-diff-")))
        (unwind-protect
            (progn
              (with-current-buffer buffer
                (write-region (point-min) (point-max) tempfile nil 'nomessage))
              (if (zerop
                   (apply #'call-process ediff-custom-diff-program
                          nil diff-buf nil
                          (append
                           (when (and (boundp 'ediff-custom-diff-options)
                                      (stringp ediff-custom-diff-options))
                             (split-string ediff-custom-diff-options))
                           (list buf-filename tempfile))))
                  (progn
                    (message "No differences found")
                    nil)
                (progn
                  (with-current-buffer diff-buf
                    (goto-char (point-min))
                    (if (fboundp 'diff-mode)
                        (diff-mode)
                      (fundamental-mode)))
                  (display-buffer diff-buf)
                  t)))
          (when (file-exists-p tempfile)
            (delete-file tempfile)))))))

;;;###autoload
(defun de-context-kill (arg)
  "Kill buffer, taking gnuclient into account."
  (interactive "p")
  (catch 'return
    (when (and (buffer-modified-p)
               buffer-file-name
               (not (string-match "\\*.*\\*" (buffer-name)))
               ;; erc buffers will be automatically saved
               (not (eq major-mode 'erc-mode))
               (= 1 arg))
      (let ((differences 't))
        (when (file-exists-p buffer-file-name)
          (setq differences (diff-buffer-with-associated-file)))
        (message (if differences
                   "Buffer has unsaved changes"
                 "Buffer has unsaved changes, but no differences wrt. the file"))
        (throw 'return nil)
        ))
    (if (and (boundp 'gnuserv-minor-mode)
             gnuserv-minor-mode)
        (gnuserv-edit)
      (set-buffer-modified-p nil)
      (kill-buffer (current-buffer)))))

;;;###autoload
(defun increment-number-at-point (arg)
  (interactive "p")
  (let ((inc (or arg 1)))
    (skip-chars-backward "0123456789xABCDEFabcdef")
    (cond ((looking-at "0x\\([0123456789ABCDEFabcdef]+\\)")
           (replace-match (format "0x%x" (+ inc (string-to-number (match-string 1) 16)))))
          ((looking-at "[0123456789]+")
           (replace-match (number-to-string (+ inc (string-to-number (match-string 0))))))
          (error "No number at point"))))

;;;###autoload
(defun fc-eval-and-replace (arg)
  "Replace the preceding sexp with its value."
  (interactive "P")
  (interactive)
  (backward-kill-sexp)
  (let ((res (eval (read (current-kill 0)))))
    (unless arg
      (prin1 res
             (current-buffer)))))

;; coreos stuff
(let ((unit-regexp "\\.\\(service\\|mount\\|socket\\)\\'"))
  (add-to-list 'auto-mode-alist `(,unit-regexp . ini-generic-mode)))

(define-key global-map (kbd "C-c SPC") 'avy-goto-subword-1)
(define-key global-map (kbd "M-g M-g") 'avy-goto-line)
(define-key global-map (kbd "C-x C-o") 'ace-window)

(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

(message ".emacs loaded")
(switch-to-buffer "*Messages*")

(provide 'yh-misc)
