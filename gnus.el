;; -*- mode: emacs-lisp; mode: hi-lock; mode: page-break; auto-compile-lisp: nil; -*-

;;; Basis
;; Load site-specific stuff
;; See http://gist.github.com/97986 for an example
(let ((local-file (locate-user-emacs-file "gnus-local.el" ".gnus-local.el")))
  (when (file-exists-p local-file)
    (load-file local-file)))

(require 'gnuslog)

(require 'gnus-dired)
(add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode)

(require 'message-x)

;; global parameters
(setq
 ;; don't annoy me with confirmation requests
 gnus-expert-user t
 ;; 5 lines of signature
 gnus-signature-limit '(5.0 "^---*Forwarded article"))

(setq
 gnus-check-new-newsgroups 'ask-server
 gnus-read-active-file 'some
 gnus-save-killed-list t
 gnus-save-newsrc-file nil
 gnus-read-newsrc-file nil
 gnus-subscribe-newsgroup-method 'gnus-subscribe-topics
 message-generate-headers-first t
 gnus-gcc-mark-as-read t
 gnus-inhibit-startup-message t
 gnus-use-cache t
 gnus-agent t
 nnmail-message-id-cache-file (concat nbc-gnus-dir "nnmail-cache")

 ;; Split mails
 nnmail-message-id-cache-length 10000
 nnmail-cache-accepted-message-ids t
 nnmail-treat-duplicates 'warn)

(add-hook 'gnus-select-article-hook 'gnus-agent-fetch-selected-article)

(defun my-gnus-expiry-target (group)
  (if (string-match "-archive" group)
      (concat my-old-group-backend ":old-" group)
    (concat my-archived-group-backend ":"
            group
            "-archive."
            (format-time-string "%m-%Y" (my-gnus-get-article-date)))))

(defun my-gnus-get-article-date ()
  "Extracts the date from the current article and converts it to Emacs time"
  (save-excursion
    (goto-char (point-min))
    (condition-case nil
        (gnus-date-get-time (message-fetch-field "date"))
      (error (current-time)))))

(setq
 ;; archiving backend
 my-archived-group-backend "nnml+archive"
 my-old-group-backend "nnfolder+old"
 ;; set expiry target to a function call
 nnmail-expiry-target 'my-gnus-expiry-target)

(setq
 gnus-total-expirable-newsgroups "^nnml.*"
 nnmail-use-long-file-names t
 gnus-uncacheable-groups "^nnml")

;; increase score for most read groups
(add-hook 'gnus-summary-exit-hook 'gnus-summary-bubble-group)

;; Use topics
(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)

(setq
 gnus-thread-hide-subtree t
 gnus-suppress-duplicates t
 gnus-auto-select-first nil
 gnus-large-newsgroup 100)

(setq gnus-default-adaptive-score-alist
      '((gnus-unread-mark)
        (gnus-ticked-mark (from 4))
        (gnus-dormant-mark (from 5))
        (gnus-del-mark (from -4) (subject -1))
        (gnus-read-mark (from 4) (subject 2))
        (gnus-expirable-mark (from -1) (subject -1))
        (gnus-killed-mark (from -1) (subject -3))
        (gnus-kill-file-mark)
        (gnus-ancient-mark)
        (gnus-low-score-mark)
        (gnus-catchup-mark (from -1) (subject -1))))

(add-hook 'message-sent-hook 'gnus-score-followup-thread)


;;; Summary buffer
(setq gnus-thread-sort-functions
      '(gnus-thread-sort-by-number
        (not gnus-thread-sort-by-most-recent-date)
        gnus-thread-sort-by-total-score))

(setq
 gnus-extra-headers '(To Cc Newsgroups Keywords)
 nnmail-extra-headers gnus-extra-headers)

;; Personal threading view
(defun gnus-user-format-function-Z (ok)
  (format "%s%s" (replace-regexp-in-string "\\(.*\\)    " "\\1   >" gnus-tmp-indentation) gnus-tmp-subject-or-nil))

;; this corresponds to a topic line format of "%n %A"
(defun gnus-user-format-function-topic-line (dummy)
  (let ((topic-face (if (zerop total-number-of-articles)
                        'italic
                      'bold)))
    (propertize
     (format "%s %d" name total-number-of-articles)
     'face topic-face)))

;; ASCII-Art (default)
(setq gnus-sum-thread-tree-indent "  ")
(setq gnus-sum-thread-tree-root "")
(setq gnus-sum-thread-tree-false-root "o ")
(setq gnus-sum-thread-tree-single-indent "")
(setq gnus-sum-thread-tree-leaf-with-other "+-> ")
(setq gnus-sum-thread-tree-vertical "| ")
(setq gnus-sum-thread-tree-single-leaf "`-> ")

;; Unicode
;; (when window-system
;;   (setq gnus-sum-thread-tree-indent "  ")
;;   (setq gnus-sum-thread-tree-root "● ")
;;   (setq gnus-sum-thread-tree-false-root "◯ ")
;;   (setq gnus-sum-thread-tree-single-indent "◎ ")
;;   (setq gnus-sum-thread-tree-leaf-with-other "├─► ")
;;   (setq gnus-sum-thread-tree-vertical "│")
;;   (setq gnus-sum-thread-tree-single-leaf "╰─► "))

(setq gnus-summary-same-subject "")

(copy-face 'default 'mysubject)
(setq gnus-face-1 'mysubject)

(copy-face 'default 'mytime)
(set-face-foreground 'mytime "green")
(setq gnus-face-2 'mytime)

(copy-face 'default 'mythreads)
(set-face-foreground 'mythreads "red")
(setq gnus-face-3 'mythreads)

(copy-face 'default 'mygrey)
(set-face-foreground 'mygrey "grey")
(setq gnus-face-4 'mygrey)

(copy-face 'default 'myblack)
(set-face-foreground 'myblack "grey60")
(setq gnus-face-5 'myblack)

(copy-face 'default 'mybiggernumbers)
(set-face-foreground 'mybiggernumbers "red")
(setq gnus-face-6 'mybiggernumbers)

(setq gnus-user-date-format-alist
      '(((gnus-seconds-today)
         . "-->  %H:%M")
        (604800 . "%a, %H:%M")
        (t . "%d/%m/%Y")))

(setq gnus-summary-line-format (concat
                                "%3{%1uM%}%*%5{%U%R%z%}"
                                "%4{|%}"
                                "%2{%-10,10&user-date;%}"
                                "%4{|%} %(%-24,24uB"
                                "%4{|%}"
                                "%6{%5i%}"
                                "%4{|%}"
                                "%6{%5k %}%)"
                                "%4{|%} %3{%B%}%1{%s%}\n"))

(setq
 gnus-group-line-format "%M%S%p%P%5y: %(%G%) %2{(%t)%}\n"
 gnus-group-mode-line-format "Gnus: %%b"
 gnus-summary-mode-line-format "Gnus: %g [%r/%U]"
 gnus-article-mode-line-format "Gnus: %g [%r/%U] %m"
 gnus-topic-line-format "%i[ %u&topic-line; ] %v\n")

(setq
 nbc-gnus-visible-headers
 '("^From:\\|^Organization:\\|^To:\\|^Cc:\\|^Reply-To:\\|^Subject:\\|^Sender:"
   "^Newsgroups:.+[,]+.*$"
   "^X-Mailer:\\|^X-Newsreader:\\|^user-Agent\\|^X-Posting-Agent"
   "^Followup-To:\\|^Date:"))

(setq
 gnus-boring-article-headers '(empty followup-to reply-to))

;; Format display
(add-hook 'gnus-article-display-hook 'gnus-article-highlight)
(add-hook 'gnus-article-display-hook 'gnus-article-hide-headers-if-wanted)
(add-hook 'gnus-article-display-hook 'gnus-article-hide-boring-headers)
(add-hook 'gnus-article-display-hook 'gnus-article-de-quoted-unreadable)
(add-hook 'gnus-article-display-hook 'gnus-article-strip-leading-blank-lines)
(add-hook 'gnus-article-display-hook 'gnus-article-remove-trailing-blank-lines)
(add-hook 'gnus-article-display-hook 'gnus-article-strip-multiple-blank-lines)
(add-hook 'gnus-article-display-hook 'gnus-article-emphasize)

;; ignore vcards
(setq gnus-ignored-mime-types '("text/x-vcard"))

;; I want plain/text mails
(setq mm-discouraged-alternatives '("text/html" "text/richtext"))

(setq gnus-default-charset (quote iso-8859-1)
      message-default-charset (quote iso-8859-1)
      mm-coding-system-priorities '(us-ascii iso-latin-1 iso-latin-9 utf-8))

(add-to-list 'mm-body-charset-encoding-alist '(iso-8859-1 . 8bit))
(add-to-list 'mm-body-charset-encoding-alist '(iso-8859-15 . 8bit))

(add-to-list 'mm-charset-synonym-alist '(iso8859-15 . iso-8859-15))
(add-to-list 'mm-charset-synonym-alist '(iso885915 . iso-8859-15))
(add-to-list 'mm-charset-synonym-alist '(ISO-8859-1 . iso-8859-1))
(add-to-list 'mm-charset-synonym-alist '(fr_FR . iso-8859-1))

(add-hook 'gnus-message-setup-hook 'font-lock-fontify-buffer)


;;; Mail sending

(add-hook 'message-mode-hook 'turn-on-auto-fill)

;; (autoload 'bbdb/send-hook "moy-bbdb"
;;   "Function to be added to `message-send-hook' to notice records when sending messages" t)

;;(add-hook 'message-send-hook 'bbdb/send-hook)

(add-hook 'mail-mode-hook 'turn-on-orgstruct++)
(add-hook 'message-mode-hook 'turn-on-orgstruct++)

;(add-hook 'message-mode-hook 'bbdb-define-all-aliases)

(setq
 message-cite-function 'trivial-cite

 gnus-signature-separator  '("^-- $"         ; The standard
                             "^--$"          ; Die OE Die !
                             "^-- *$"        ; A common mangling
                             "^-------*$"    ; Many people just use a looong
                                        ; line of dashes.  Shame!
                             "^ *--------*$" ; Double-shame!
                             "^________*$"   ; Underscores are also popular
                             "^========*$"   ; Pervert!
                             ))


;;; Posting Styles

(setq message-citation-line-function 'message-insert-formatted-citation-line)

;; Tell gnus into which group to store messages
(setq gnus-message-archive-group
      '((if (message-news-p)
            (concat "news." (format-time-string "%Y-%m" (current-time)))
          (list ;; gnus-newsgroup-name
                (concat "mail." (format-time-string "%Y-%m" (current-time)))))))

(setq
 gnus-prompt-before-saving nil
 gnus-default-article-saver  'gnus-summary-save-in-rmail)

;;; Misc

(defadvice gnus-summary-reply (around reply-in-news activate)
  (interactive)
  (when (or (not (gnus-news-group-p gnus-newsgroup-name))
            (y-or-n-p "Really reply to author ? "))
    ad-do-it))

(defun my-gnus-summary-show-thread ()
  "Show thread without changing cursor positon."
  (interactive)
  (gnus-summary-show-thread)
  (beginning-of-line)
  (forward-char 1))

(define-key gnus-summary-mode-map [(right)] 'my-gnus-summary-show-thread)
(define-key gnus-summary-mode-map [(left)]  'gnus-summary-hide-thread)


;;; Gnus extensions

;(add-hook 'message-mode-hook 'flyspell-mode)

(setq message-signature 'fortune)

(defvar fortune-program nil
  "*Program used to generate epigrams, default \"fortune\".")

(defvar fortune-switches nil
  "*List of extra arguments when `fortune-program' is invoked.")

(setq fortune-program (executable-find "fortune"))

(add-to-list 'fortune-switches "chapterhouse-dune")
(add-to-list 'fortune-switches "children-of-dune")
(add-to-list 'fortune-switches "dune")
(add-to-list 'fortune-switches "dune-messiah")
(add-to-list 'fortune-switches "god-emperor")
(add-to-list 'fortune-switches "heretics-of-dune")
(add-to-list 'fortune-switches "house-atreides")
(add-to-list 'fortune-switches "house-harkonnen")

(defun fortune (&optional long-p)
  "Generate a random epigram.
An optional prefix argument generates a long epigram.
The epigram is inserted at point if called interactively."
  (interactive "*P")
  (let ((fortune-buffer (generate-new-buffer " fortune"))
        (fortune-string "Have an adequate day."))
    (unwind-protect
        (save-excursion
          (set-buffer fortune-buffer)
          (apply 'call-process
                 (append (list (or fortune-program "fortune") nil t nil)
                         (list (if long-p "-l" "-s")) fortune-switches))
          (dos2unix)
          (skip-chars-backward "\n\t ")
          (setq fortune-string (buffer-substring (point-min) (point))))
      (kill-buffer fortune-buffer))
    (if (interactive-p)
        (insert fortune-string))
    fortune-string))

(setq gnus-group-highlight
      '(((and (= unread 0) (not mailp) (eq level 1)) . gnus-group-news-1-empty-face)
        ((and (not mailp) (eq level 1)) . gnus-group-news-1-face)
        ((and (= unread 0) (not mailp) (eq level 2)) . gnus-group-news-2-empty-face)
        ((and (not mailp) (eq level 2)) . gnus-group-news-2-face)
        ((and (= unread 0) (not mailp) (eq level 3)) . gnus-group-news-3-empty-face)
        ((and (not mailp) (eq level 3)) . gnus-group-news-3-face)
        ((and (= unread 0) (not mailp) (eq level 4)) . gnus-group-news-4-empty-face)
        ((and (not mailp) (eq level 4)) . gnus-group-news-4-face)
        ((and (= unread 0) (not mailp) (eq level 5)) . gnus-group-news-5-empty-face)
        ((and (not mailp) (eq level 5)) . gnus-group-news-5-face)
        ((and (= unread 0) (not mailp) (eq level 6)) . gnus-group-news-6-empty-face)
        ((and (not mailp) (eq level 6)) . gnus-group-news-6-face)
        ((and (= unread 0) (not mailp)) . gnus-group-news-low-empty-face)
        ((and (not mailp)) . gnus-group-news-low-face)
        ((and (= unread 0) (eq level 1)) . gnus-group-mail-1-empty-face)
        ((eq level 1) . gnus-group-mail-1-face)
        ((and (= unread 0) (eq level 2)) . gnus-group-mail-2-empty-face)
        ((eq level 2) . gnus-group-mail-2-face)
        ((and (= unread 0) (eq level 3)) . gnus-group-mail-3-empty-face)
        ((eq level 3) . gnus-group-mail-3-face)
        ((= unread 0) . gnus-group-mail-low-empty-face)
        (t . gnus-group-mail-low-face)))

(defun my-setup-hl-line ()
  (hl-line-mode 1)
  (setq cursor-type nil) ; make the cursor invisible
  )

(add-hook 'gnus-summary-mode-hook 'my-setup-hl-line)
(add-hook 'gnus-group-mode-hook 'my-setup-hl-line)


;;; Supercite

(autoload 'sc-cite-original     "supercite" "Supercite 3.1" t)
(autoload 'sc-submit-bug-report "supercite" "Supercite 3.1" t)

(setq message-cite-function 'sc-cite-original)


;;; Git apply (adapted from Dimitri Fontaine's code)
(defvar yh/gnus-group-git-repos nil
  "A plist of repositories and dir where to apply git patches")

(defun yh/gnus-group-git-read-repo ()
  "Ask use where to apply the current patch"
  (completing-read
   "Choose a repository where to apply: "
   (loop for (r p) on yh/gnus-group-git-repos by 'cddr collect (symbol-name r)) nil t))

(defun yh/gnus-group-git-am (repo)
  (interactive (list (yh/gnus-group-git-read-repo)))
  (let ((git-dir
         (expand-file-name
          (plist-get yh/gnus-group-git-repos (intern repo)))))
    (when git-dir
      (gnus-summary-save-in-pipe
       (format "cd %s ; git am -3 -s" git-dir) 'raw))))

(define-key gnus-summary-save-map (kbd "g") 'yh/gnus-group-git-am)


;;; Crypto

;; Here we make button for the multipart
(setq gnus-buttonized-mime-types '("multipart/encrypted" "multipart/signed" "multipart/alternative"))

(setq mm-verify-option 'known)
(setq mm-decrypt-option 'known)
(setq gnus-article-emulate-mime t) ; already set in my gnus but you may need it.

(require 'bbdb-pgp)
;;(add-hook 'gnus-message-setup-hook 'mml-secure-message-sign-pgpmime)

(defadvice mml2015-sign (after mml2015-sign-rename (cont) act)
  (save-excursion
    (search-backward "Content-Type: application/pgp-signature")
    (goto-char (point-at-eol))
    (insert "; name=\"signature.asc\"")))

;;; Misc
(require 'gnus-sum)
