;; -*- mode: emacs-lisp; auto-compile-lisp: nil; -*-

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Info-additional-directory-list nil)
 '(LaTeX-item-regexp "\\(entry\\|\\(bib\\)?item\\)\\b")
 '(Man-notify-method (quote bully))
 '(TeX-PDF-mode t)
 '(TeX-output-view-style
   (quote
     (("^dvi$"
       ("^landscape$" "^pstricks$\\|^pst-\\|^psfrag$")
       "%(o?)dvips -t landscape %d -o && gv %f")
      ("^dvi$" "^pstricks$\\|^pst-\\|^psfrag$" "%(o?)dvips %d -o && gv %f")
      ("^dvi$"
       ("^\\(?:a4\\(?:dutch\\|paper\\|wide\\)\\|sem-a4\\)$" "^landscape$")
       "%(o?)xdvi %dS -paper a4r -s 0 %d")
      ("^dvi$" "^\\(?:a4\\(?:dutch\\|paper\\|wide\\)\\|sem-a4\\)$" "%(o?)xdvi %dS -paper a4 %d")
      ("^dvi$"
       ("^\\(?:a5\\(?:comb\\|paper\\)\\)$" "^landscape$")
       "%(o?)xdvi %dS -paper a5r -s 0 %d")
      ("^dvi$" "^\\(?:a5\\(?:comb\\|paper\\)\\)$" "%(o?)xdvi %dS -paper a5 %d")
      ("^dvi$" "^b5paper$" "%(o?)xdvi %dS -paper b5 %d")
      ("^dvi$" "^letterpaper$" "%(o?)xdvi %dS -paper us %d")
      ("^dvi$" "^legalpaper$" "%(o?)xdvi %dS -paper legal %d")
      ("^dvi$" "^executivepaper$" "%(o?)xdvi %dS -paper 7.25x10.5in %d")
      ("^dvi$" "." "%(o?)xdvi %dS %d")
      ("^pdf$" "." "xpdf -fullscreen -remote %s -raise %o %(outpage)")
      ("^html?$" "." "netscape %o"))))
 '(ack-executable (executable-find "ack"))
 '(add-log-keep-changes-together t)
 '(ange-ftp-ftp-program-args (quote ("-i" "-n" "-g" "-v" "-u")))
 '(apropos-do-all t)
 '(apropos-sort-by-scores t)
 '(auto-revert-check-vc-info nil)
 '(auto-revert-interval 2)
 '(auto-save-interval 50)
 '(auto-save-timeout 5)
 '(avy-background t)
 '(avy-highlight-first t)
 '(avy-keys (quote (97 115 100 102 103 104 106 107 108 59)))
 '(avy-style (quote at-full))
 '(bbdb-complete-name-allow-cycling t)
 '(bbdb-dwim-net-address-allow-redundancy t)
 '(bbdb-send-mail-style (quote gnus))
 '(bbdb/pgp-method (quote mml-pgpmime))
 '(blink-cursor-mode nil nil (frame))
 '(bmkp-last-as-first-bookmark-file nil)
 '(bookmark-bmenu-file-column 50)
 '(bookmark-save-flag 0)
 '(browse-url-netscape-program "mozilla-firefox")
 '(c++-font-lock-extra-types (quote ("Q[a-zA-Z]*" "uint" "ulong" "string")))
 '(c-default-style (quote ((java-mode . "java") (other . "personal"))))
 '(calendar-mark-diary-entries-flag t)
 '(calendar-view-diary-initially-flag t)
 '(calendar-week-start-day 1)
 '(canlock-password "86b712369f839f776688a36513969db03cf50eb2" t)
 '(case-fold-search t)
 '(circe-format-self-say "<{nick}> {body}")
 '(column-number-mode t)
 '(comment-style (quote extra-line))
 '(company-idle-delay 0.3)
 '(company-show-numbers t)
 '(company-tooltip-align-annotations t)
 '(company-tooltip-limit 20)
 '(compilation-ask-about-save nil)
 '(compilation-read-command t)
 '(compilation-scroll-output t)
 '(compilation-window-height 10)
 '(compile-command "make")
 '(create-lockfiles nil)
 '(cscope-do-not-update-database t)
 '(custom-file "~/.emacs.d/cust.el")
 '(custom-safe-themes
   (quote
     ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(default-justification (quote left))
 '(delete-active-region nil)
 '(diary-file "~/.diary")
 '(diff-switches "-u")
 '(dired-omit-files "^\\.?#\\|^\\.$")
 '(dired-recursive-deletes (quote top))
 '(diredp-hide-details-initially-flag nil)
 '(display-time-24hr-format t)
 '(display-time-day-and-date t)
 '(display-time-mode t)
 '(doc-view-resolution 75)
 '(doxymacs-doxygen-style "JavaDoc")
 '(doxymacs-doxygen-tags "DOXYTAGS")
 '(ecb-auto-activate nil)
 '(ecb-auto-update-methods-after-save nil)
 '(ecb-compilation-buffer-names
   (quote
     (("*Calculator*")
      ("*vc*")
      ("*vc-diff*")
      ("*Backtrace*")
      ("*shell*")
      ("*compilation*")
      ("*bsh*")
      ("*grep*")
      ("*Compile-Log*")
      ("*slime-repl sbcl*"))))
 '(ecb-compile-window-height 10)
 '(ecb-compile-window-temporally-enlarge nil)
 '(ecb-directories-menu-user-extension-function (quote ignore))
 '(ecb-enlarged-compilation-window-max-height 10)
 '(ecb-eshell-auto-activate t)
 '(ecb-eshell-enlarge-when-eshell nil)
 '(ecb-eshell-enlarge-when-selecting nil)
 '(ecb-eshell-fit-window-to-command-output nil)
 '(ecb-eshell-synchronize nil)
 '(ecb-history-menu-user-extension-function (quote ignore))
 '(ecb-key-map
   (quote
     ("<f10>"
      (t "fh" ecb-history-filter)
      (t "fs" ecb-sources-filter)
      (t "fm" ecb-methods-filter)
      (t "fr" ecb-methods-filter-regexp)
      (t "ft" ecb-methods-filter-tagclass)
      (t "fc" ecb-methods-filter-current-type)
      (t "fp" ecb-methods-filter-protection)
      (t "fn" ecb-methods-filter-nofilter)
      (t "fl" ecb-methods-filter-delete-last)
      (t "ff" ecb-methods-filter-function)
      (t "p" ecb-nav-goto-previous)
      (t "n" ecb-nav-goto-next)
      (t "lc" ecb-change-layout)
      (t "lr" ecb-redraw-layout)
      (t "lw" ecb-toggle-ecb-windows)
      (t "lt" ecb-toggle-layout)
      (t "s" ecb-window-sync)
      (t "r" ecb-rebuild-methods-buffer)
      (t "a" ecb-toggle-auto-expand-tag-tree)
      (t "x" ecb-expand-methods-nodes)
      (t "h" ecb-show-help)
      (t "gl" ecb-goto-window-edit-last)
      (t "g1" ecb-goto-window-edit1)
      (t "g2" ecb-goto-window-edit2)
      (t "gc" ecb-goto-window-compilation)
      (t "gd" ecb-goto-window-directories)
      (t "gs" ecb-goto-window-sources)
      (t "gm" ecb-goto-window-methods)
      (t "gh" ecb-goto-window-history)
      (t "ga" ecb-goto-window-analyse)
      (t "gb" ecb-goto-window-speedbar)
      (t "md" ecb-maximize-window-directories)
      (t "ms" ecb-maximize-window-sources)
      (t "mm" ecb-maximize-window-methods)
      (t "mh" ecb-maximize-window-history)
      (t "ma" ecb-maximize-window-analyse)
      (t "mb" ecb-maximize-window-speedbar)
      (t "e" eshell)
      (t "o" ecb-toggle-scroll-other-window-scrolls-compile)
      (t "\\" ecb-toggle-compile-window)
      (t "/" ecb-toggle-compile-window-height)
      (t "," ecb-cycle-maximized-ecb-buffers)
      (t "." ecb-cycle-through-compilation-buffers))))
 '(ecb-layout-window-sizes
   (quote
     (("sigma"
       (ecb-methods-buffer-name 0.15384615384615385 . 0.9833333333333333)
       (ecb-sources-buffer-name 0.16346153846153846 . 0.48333333333333334)
       (ecb-directories-buffer-name 0.16346153846153846 . 0.5)))))
 '(ecb-major-modes-deactivate
   (quote
     (hide-all-except-activated . "\\(Info\\|custom\\)-mode")))
 '(ecb-methods-menu-user-extension-function (quote ignore))
 '(ecb-options-version "2.40")
 '(ecb-other-window-behavior (quote only-edit))
 '(ecb-run-ediff-in-ecb-frame t)
 '(ecb-sources-menu-user-extension-function (quote ignore))
 '(ecb-tip-of-the-day nil)
 '(ecb-vc-supported-backends
   (quote
     ((ecb-vc-dir-managed-by-CVS . vc-state)
      (ecb-vc-dir-managed-by-RCS . vc-state)
      (ecb-vc-dir-managed-by-SCCS . vc-state)
      (ecb-vc-dir-managed-by-SVN . vc-state))))
 '(edebug-trace t)
 '(ediff-custom-diff-options "-u")
 '(ediff-keep-variants t)
 '(elpy-rpc-backend nil)
 '(elpy-test-runner (quote elpy-test-nose-runner))
 '(enable-recursive-minibuffers t)
 '(epa-file-select-keys nil)
 '(erc-bbdb-auto-create-on-whois-p t)
 '(erc-echo-timestamps t)
 '(erc-input-line-position -2)
 '(erc-modules
   (quote
     (autojoin button completion dcc fill irccontrols keep-place match move-to-prompt netsplit networks noncommands readonly ring scrolltobottom services stamp track)))
 '(erc-track-exclude-types (quote ("JOIN" "KICK" "NICK" "PART" "QUIT" "MODE")))
 '(erc-track-shorten-start 2)
 '(erc-track-showcount t)
 '(eshell-ask-to-save-history (quote always))
 '(eshell-cmpl-cycle-completions nil)
 '(eshell-modules-list
   (quote
     (eshell-alias eshell-banner eshell-basic eshell-cmpl eshell-dirs eshell-glob eshell-hist eshell-ls eshell-pred eshell-prompt eshell-rebind eshell-script eshell-smart eshell-term eshell-unix eshell-xtra)))
 '(eshell-prefer-to-shell t nil (eshell))
 '(eshell-save-history-on-exit t)
 '(eshell-term-name "eterm-color")
 '(eshell-visual-commands
   (quote
     ("vi" "screen" "top" "less" "more" "lynx" "ncftp" "pine" "tin" "trn" "elm" "htop" "mocp" "tmux" "lftp")))
 '(fill-column 79)
 '(fill-nobreak-predicate (quote (fill-french-nobreak-p fill-single-word-nobreak-p)))
 '(find-file-visit-truename t)
 '(flyspell-default-dictionary "en")
 '(flyspell-issue-welcome-flag nil)
 '(font-latex-fontify-sectioning (quote color))
 '(gc-cons-threshold 20000000)
 '(gdb-all-registers t)
 '(gdb-many-windows t)
 '(gdb-same-frame nil)
 '(gdb-show-main t)
 '(gdb-use-separate-io-buffer t)
 '(generic-extras-enable-list
   (quote
     (alias-generic-mode apache-conf-generic-mode apache-log-generic-mode bat-generic-mode etc-fstab-generic-mode etc-modules-conf-generic-mode etc-passwd-generic-mode etc-services-generic-mode etc-sudoers-generic-mode fvwm-generic-mode hosts-generic-mode inetd-conf-generic-mode inf-generic-mode ini-generic-mode java-manifest-generic-mode java-properties-generic-mode mailagent-rules-generic-mode mailrc-generic-mode named-boot-generic-mode named-database-generic-mode prototype-generic-mode rc-generic-mode reg-generic-mode resolve-conf-generic-mode samba-generic-mode show-tabs-generic-mode vrml-generic-mode x-resource-generic-mode)))
 '(gist-multiple-files-mark "▶")
 '(glasses-face (quote bold))
 '(glasses-original-separator "")
 '(glasses-separate-parentheses-p nil)
 '(glasses-separator "")
 '(global-auto-revert-mode t)
 '(global-company-mode t)
 '(global-font-lock-mode t nil (font-lock))
 '(global-semantic-stickyfunc-mode t)
 '(global-undo-tree-mode t)
 '(global-visible-mark-mode-exclude-alist (quote ("^\\*")))
 '(global-whitespace-mode t)
 '(global-whitespace-newline-mode nil)
 '(gnorb-gnus-mail-search-backend (quote notmuch))
 '(gnus-activate-level 5)
 '(gnus-adaptive-pretty-print t)
 '(gnus-article-update-date-headers nil)
 '(gnus-asynchronous t)
 '(gnus-cacheable-groups ".*")
 '(gnus-completing-read-function (quote gnus-ido-completing-read))
 '(gnus-completion-styles
   (if
       (and
        (boundp
         (quote completion-styles-alist))
        (boundp
         (quote completion-styles)))
       (append
        (when
            (and
             (assq
              (quote substring)
              completion-styles-alist)
             (not
              (memq
               (quote substring)
               completion-styles)))
          (list
           (quote substring)))
        completion-styles)
     nil))
 '(gnus-decay-scores "\\.ADAPT\\'")
 '(gnus-fetch-old-headers nil)
 '(gnus-group-default-list-level 4)
 '(gnus-init-file "~/.emacs.d/gnus.el")
 '(gnus-novice-user nil)
 '(gnus-registry-install t)
 '(gnus-score-interactive-default-score 100)
 '(gnus-score-thread-simplify t)
 '(gnus-simplify-ignored-prefixes "RE :")
 '(gnus-summary-default-high-score 2000)
 '(gnus-summary-default-low-score -1)
 '(gnus-summary-default-score 1)
 '(gnus-summary-dummy-line-format
   "   %(:                                                   :%) %S
")
 '(gnus-summary-gather-subject-limit (quote fuzzy))
 '(gnus-summary-ignore-duplicates t)
 '(gnus-summary-make-false-root (quote dummy))
 '(gnus-summary-mark-below -10000)
 '(gnus-summary-thread-gathering-function (quote gnus-gather-threads-by-references))
 '(gnus-topic-display-empty-topics nil)
 '(gnus-uncacheable-groups nil)
 '(gnus-update-message-archive-method t)
 '(gnus-use-adaptive-scoring (quote (line)))
 '(gnus-use-correct-string-widths t)
 '(gnus-use-header-prefetch t)
 '(go-oracle-command "/Users/hodiquey/Projects/go/bin/oracle")
 '(graphviz-dot-preview-extension "ps")
 '(hl-line-face (quote highlight))
 '(hl-line-hack-exceptions (quote ("*eshell*" "*Calendar*")))
 '(hl-line-hack-face (quote highlight))
 '(htmlize-head-tags
   "<meta http-equiv=\"Content-Type\" content=\"text/html; charset=ISO-8859-1\">")
 '(htmlize-html-major-mode (quote html-mode))
 '(ibuffer-display-summary nil)
 '(ibuffer-expert t)
 '(ibuffer-movement-cycle nil)
 '(ibuffer-saved-filter-groups
   (quote
     (("default"
       ("sh"
        (mode . sh-mode))
       ("emacs"
        (or
         (name . "^\\*scratch\\*$")
         (name . "^\\*Messages\\*$")))
       ("org"
        (mode . org-mode))
       ("latex"
        (or
         (mode . LaTeX-mode)
         (mode . latex-mode)))
       ("C(++)"
        (or
         (mode . c-mode)
         (mode . c++-mode)))
       ("FDD"
        (mode . tl-fdd-mode))
       ("Java"
        (mode . java-mode))
       ("lisp"
        (or
         (mode . emacs-lisp-mode)
         (mode . lisp-interaction-mode)
         (mode . ielm-mode)
         (mode . lisp-mode)
         (mode . slime-repl-mode)))
       ("python"
        (mode . python-mode))
       ("go"
        (mode . go-mode))
       ("yaml"
        (mode . yaml-mode))
       ("dired"
        (mode . dired-mode))
       ("irc"
        (or
         (mode . circe-mode)
         (mode . circe-server-mode)
         (mode . circe-query-mode)
         (mode . circe-channel-mode)
         (mode . rcirc-mode)))
       ("gnus"
        (or
         (mode . message-mode)
         (mode . bbdb-mode)
         (mode . mail-mode)
         (mode . gnus-group-mode)
         (mode . gnus-summary-mode)
         (mode . gnus-article-mode)
         (name . "^\\.bbdb$")
         (name . "^\\.newsrc-dribble")))
       ("magit"
        (or
         (mode . magit-mode)
         (mode . magit-status-mode)
         (mode . magit-log-mode)
         (mode . magit-wazzup-mode)))))))
 '(ibuffer-saved-filters
   (quote
     (("gnus"
       ((or
         (mode . message-mode)
         (mode . mail-mode)
         (mode . gnus-group-mode)
         (mode . gnus-summary-mode)
         (mode . gnus-article-mode))))
      ("programming"
       ((or
         (mode . emacs-lisp-mode)
         (mode . cperl-mode)
         (mode . c-mode)
         (mode . java-mode)
         (mode . idl-mode)
         (mode . lisp-mode)))))))
 '(icicle-inhibit-reminder-prompt-flag t)
 '(ido-auto-merge-delay-time 1.5)
 '(ido-default-buffer-method (quote selected-window))
 '(ido-default-file-method (quote selected-window))
 '(ido-enable-flex-matching t)
 '(ido-everywhere t)
 '(ido-mode (quote both) nil (ido))
 '(ido-use-virtual-buffers t)
 '(imenu-auto-rescan t)
 '(indent-region-mode t)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(initial-scratch-message nil)
 '(js2-basic-offset 4)
 '(js2-bounce-indent-flag nil)
 '(js2-highlight-level 3)
 '(kill-read-only-ok t)
 '(kill-whole-line t)
 '(large-file-warning-threshold 1000000)
 '(line-move-visual nil)
 '(load-prefer-newer t)
 '(log-edit-common-indent -7)
 '(lui-fill-column 100)
 '(magit-completing-read-function (quote magit-ido-completing-read))
 '(magit-diff-refine-hunk nil)
 '(magit-pulls-executable "/var/lib/gems/1.8/bin/git-pulls")
 '(magit-remote-ref-format (quote remote-slash-branch))
 '(magit-status-buffer-switch-function (quote switch-to-buffer))
 '(magit-time-format-string "%Y-%m-%d %T")
 '(mail-source-delete-incoming t)
 '(mail-user-agent (quote gnus-user-agent))
 '(major-mode (quote indented-text-mode))
 '(max-lisp-eval-depth 500)
 '(max-specpdl-size 1000)
 '(menu-bar-mode nil)
 '(message-forward-as-mime t)
 '(message-forward-ignored-headers
   (quote
     ("^Content-Transfer-Encoding:" "^X-Gnus" "^Received:" "^Bcc:" "^X-MS" "^Delivered-To" "^X-Virus")))
 '(minibuffer-prompt-properties
   (quote
     (read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt)))
 '(mm-inline-text-html-with-images t)
 '(mm-text-html-renderer (quote shr))
 '(mml2015-display-key-image nil)
 '(mml2015-sign-with-sender t)
 '(mouse-1-click-follows-link nil)
 '(mouse-1-click-in-non-selected-windows nil)
 '(mouse-avoidance-threshold 15)
 '(mouse-drag-copy-region t)
 '(mouse-yank-at-point t)
 '(mumamo-global-mode t)
 '(next-line-add-newlines nil)
 '(nnimap-authinfo-file "~/.authinfo.gpg")
 '(nnir-namazu-index-directory "/home/yann/.mknmz")
 '(nnir-namazu-remove-prefix "/home/yann/Maildir/")
 '(nnir-search-engine (quote namazu) t)
 '(nnir-swish++-configuration-file "/home/yann/mail/mail/swish++.conf")
 '(nnir-swish++-program "search++")
 '(nnir-swish++-remove-prefix "/home/yann/Maildir/")
 '(nnir-swish-e-index-file "/home/yann/Maildir/index.swish")
 '(nnir-swish-e-remove-prefix "/home/yann/Maildir/")
 '(nnmail-split-fancy-match-partial-words t)
 '(nntp-authinfo-file "~/.authinfo.gpg")
 '(notmuch-fcc-dirs nil)
 '(ns-function-modifier (quote hyper))
 '(ns-right-alternate-modifier (quote alt))
 '(ns-right-control-modifier (quote left))
 '(ns-use-native-fullscreen nil)
 '(nxhtml-global-minor-mode t)
 '(nxhtml-global-validation-header-mode t)
 '(org-agenda-log-mode-items (quote (closed clock state)))
 '(org-agenda-remove-tags t)
 '(org-agenda-skip-archived-trees nil)
 '(org-agenda-start-with-clockreport-mode nil)
 '(org-agenda-start-with-log-mode t)
 '(org-catch-invisible-edits (quote smart))
 '(org-clock-into-drawer 2 t)
 '(org-crypt-disable-auto-save t)
 '(org-crypt-key "yann.hodique@gmail.com")
 '(org-crypt-tag-matcher "CRYPT")
 '(org-cycle-open-archived-trees t)
 '(org-disputed-keys
   (quote
     (([(shift up)]
       .
       [(meta p)])
      ([(shift down)]
       .
       [(meta n)])
      ([(shift left)]
       .
       [(meta -)])
      ([(shift right)]
       .
       [(meta +)])
      ([(control shift right)]
       .
       [(control meta +)])
      ([(control shift left)]
       .
       [(control meta -)]))))
 '(org-ellipsis (quote highlight))
 '(org-enforce-todo-checkbox-dependencies t)
 '(org-enforce-todo-dependencies t)
 '(org-export-allow-BIND t)
 '(org-export-backends (quote (ascii beamer html icalendar latex odt s5)))
 '(org-export-publishing-directory
   (quote
     ((:html . "./html")
      (:LaTeX . "./latex")
      (:ascii . "./txt")
      (:ical . "./ical")
      (:xoxo . "./xoxo"))) t)
 '(org-export-taskjuggler-keep-project-as-task nil)
 '(org-export-taskjuggler-target-version 3.3)
 '(org-file-apps
   (quote
     ((auto-mode . emacs)
      ("\\.mm\\'" . default)
      ("\\.x?html?\\'" . default)
      ("\\.pdf\\'" . default))))
 '(org-latex-classes
   (quote
     (("beamer" "\\documentclass[presentation]{beamer}"
                ("\\section{%s}" . "\\section*{%s}")
                ("\\subsection{%s}" . "\\subsection*{%s}")
                ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
      ("sigplanconf" "\\documentclass{sigplanconf}"
                     ("\\section{%s}" . "\\section*{%s}")
                     ("\\subsection{%s}" . "\\subsection*{%s}")
                     ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
      ("article" "\\documentclass[11pt]{article}"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
      ("report" "\\documentclass[11pt]{report}"
                ("\\part{%s}" . "\\part*{%s}")
                ("\\chapter{%s}" . "\\chapter*{%s}")
                ("\\section{%s}" . "\\section*{%s}")
                ("\\subsection{%s}" . "\\subsection*{%s}")
                ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
      ("book" "\\documentclass[11pt]{book}"
              ("\\part{%s}" . "\\part*{%s}")
              ("\\chapter{%s}" . "\\chapter*{%s}")
              ("\\section{%s}" . "\\section*{%s}")
              ("\\subsection{%s}" . "\\subsection*{%s}")
              ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))))
 '(org-list-empty-line-terminates-plain-lists t)
 '(org-modules
   (quote
     (org-bbdb org-bibtex org-docview org-gnus org-info org-jsinfo org-irc org-mew org-mhe org-rmail org-vm org-wl org-w3m org-s5)))
 '(org-outline-path-complete-in-steps nil)
 '(org-refile-allow-creating-parent-nodes (quote confirm))
 '(org-refile-targets (quote ((org-agenda-files :maxlevel . 3))))
 '(org-remember-interactive-interface (quote outline))
 '(org-remove-highlights-with-change nil)
 '(org-replace-disputed-keys t)
 '(org-special-ctrl-a/e t)
 '(org-stuck-projects
   (quote
     ("+LEVEL=2/-DONE"
      ("TODO" "NEXT" "NEXTACTION")
      ("quotes"))))
 '(org-todo-keywords
   (quote
     ((type "TODO(t)" "WAITING(w@/!)" "MAYBE(m)" "|" "DONE(d!)" "CANCELED(c@)"))))
 '(org-track-ordered-property-with-tag t)
 '(org-trello-current-prefix-keybinding "C-c o")
 '(org-use-fast-todo-selection t)
 '(org-use-speed-commands t)
 '(osd-args
   (quote
     ("--delay=3" "--age=3" "--pos=bottom" "--offset=70" "--outline=5" "--outlinecolour=grey" "--font=-microsoft-verdana-medium-r-normal--10-*-*-*-*-*-*")))
 '(package-selected-packages
   (quote
     (ido-completing-read+ magit-gh-pulls puppet-mode yaml-mode iedit graphviz-dot-mode pp-c-l cmake-mode ag helm-projectile helm magit avy org-trello markdown-mode gist notmuch php-mode crontab-mode elpy py-isort py-autopep8 cython-mode company-go go-eldoc elisp-slime-nav paredit cparen mic-paren info+ moccur-edit bbdb flx-ido icomplete+ dired+ smex exec-path-from-shell diminish)))
 '(paradox-automatically-star nil)
 '(parens-require-spaces t)
 '(partial-completion-mode t)
 '(pcomplete-autolist t)
 '(pcomplete-cycle-completions nil)
 '(pgg-query-keyserver t)
 '(planner-backend (quote muse))
 '(planner-default-page "TaskPool")
 '(planner-multi-copy-tasks-to-page "TaskPool")
 '(planner-tasks-file-behavior (quote (quote close)))
 '(planner-use-other-window nil)
 '(planner-xtla-log-edit-include-files-flag nil)
 '(planner-xtla-log-edit-notice-commit-function t)
 '(pop-up-windows t)
 '(pp^L-^L-string
   "          ------------------------------------------------------------          ")
 '(pp^L-^L-string-post "
")
 '(pp^L-^L-string-pre "
")
 '(pretty-control-l-mode t)
 '(preview-default-option-list
   (quote
     ("displaymath" "floats" "graphics" "textmath" "footnotes")))
 '(prolog-system (quote swi))
 '(proof-assistants (quote (isar coq acl2)))
 '(ps-lpr-command "kprinter")
 '(pyvenv-mode t)
 '(rcirc-buffer-maximum-lines 500)
 '(rcirc-fill-column 120)
 '(read-mail-command (quote gnus))
 '(read-quoted-char-radix 16)
 '(recentf-exclude (quote (":\\|#")))
 '(recentf-max-menu-items 30)
 '(recentf-max-saved-items 50)
 '(recentf-mode t)
 '(recentf-save-file "~/.emacs-recentf")
 '(reftex-auto-recenter-toc t)
 '(reftex-revisit-to-follow t)
 '(reftex-toc-split-windows-horizontally nil)
 '(safe-local-variable-values
   (quote
     ((indent-tabs-mode t)
      (eval set
            (make-local-variable
             (quote backup-directory-alist))
            (list
             (cons "."
                   (file-relative-name
                    (file-name-directory
                     (buffer-file-name))
                    (file-name-directory
                     (file-truename
                      (buffer-file-name)))))))
      (eval ignore-errors
            (whitespace-mode nil)
            (whitespace-mode t))
      (whitespace-style face trailing lines-tail)
      (test-case-name . twisted\.test\.test_internet)
      (org-export-html-table-tag . "<table border=\"1\" cellspacing=\"0\" cellpadding=\"6\" rules=\"groups\" frame=\"box\">")
      (org-export-html-style-include-default)
      (org-export-html-style . "   <style type=\"text/css\">
      p {font-weight: normal; color: gray; }
      h1 {color: black; }
  </style>")
      (folded-file . t)
      (TeX-master . main\.tex)
      (Package . Memoization)
      (Base . 10)
      (Syntax . Common-Lisp)
      (unibyte . t)
      (auto-recompile . t)
      (TeX-master . "main")
      (TeX-master . t)
      (auto-compile-lisp)
      (before-save-hook org-encrypt-entries))))
 '(save-abbrevs (quote silently))
 '(save-place t nil (saveplace))
 '(save-place-limit 100)
 '(sawfish-extra-keyword-list
   (quote
     ("add-frame-style" "call-after-load" "call-after-property-changed" "call-after-state-changed" "custom-set-property" "define" "define-structure" "export" "open")))
 '(sawfish-warning-keyword-list
   (quote
     ("fixme" "FIXME" "Fixme" "fix me" "Fix me" "!!!" "Grrr" "Bummer" "todo" "TODO" "Todo")))
 '(sc-auto-fill-region-p nil)
 '(sc-citation-leader "")
 '(sc-nested-citation-p t)
 '(scroll-bar-mode nil)
 '(scroll-conservatively 150)
 '(search-whitespace-regexp "[ 	
]+")
 '(select-active-regions (quote only))
 '(select-enable-clipboard nil)
 '(semantic-idle-scheduler-max-buffer-size 102400)
 '(server-use-tcp nil)
 '(set-mark-command-repeat-pop t)
 '(sh-shell-file "/bin/bash")
 '(size-indication-mode t)
 '(slime-complete-symbol*-fancy t)
 '(slime-display-compilation-output nil)
 '(slime-header-line-p nil)
 '(slime-kill-without-query-p t)
 '(smex-history-length 15)
 '(smex-prompt-string "M-x ")
 '(smex-save-file "~/.emacs.d/smex.save")
 '(smtpmail-debug-info t)
 '(smtpmail-debug-verb t)
 '(speedbar-frame-parameters
   (quote
     ((width . 20)
      (border-width . 0)
      (menu-bar-lines . 0)
      (tool-bar-lines . 0)
      (unsplittable . t))))
 '(speedbar-use-images nil)
 '(tabbar-inhibit-functions
   (quote
     (tabbar-default-inhibit-function yh/tabbar-inhibit-function)))
 '(tags-revert-without-query t)
 '(tc-make-attribution (quote kai-tc-simple-attribution))
 '(tc-mouse-overlays t)
 '(temp-buffer-resize-mode t)
 '(tempo-insert-region nil)
 '(tempo-interactive t)
 '(test-case-python-arguments "-W ignore::DeprecationWarning")
 '(test-case-python-executable "python")
 '(tla-arch-branch (quote baz))
 '(tls-program
   (quote
     ("openssl s_client -connect %h:%p -no_ssl2 -ign_eof" "gnutls-cli --insecure -p %p %h" "gnutls-cli --insecure -p %p %h --protocols ssl3" "openssl s_client -connect %h:%p -no_ssl2 -ign_eof")))
 '(tool-bar-mode nil nil (tool-bar))
 '(tooltip-frame-parameters
   (quote
     ((name . "tooltip")
      (internal-border-width . 1)
      (border-width . 0))))
 '(tooltip-gud-tips-p t)
 '(tracking-shorten-buffer-names-p nil)
 '(tramp-default-method-alist
   (quote
     (("%" "" "smb")
      ("" "\\`\\(anonymous\\|ftp\\)\\'" "ftp")
      ("\\`ftp" "" "ftp")
      ("\\`localhost\\'" "\\`root\\'" "su"))))
 '(tramp-gvfs-methods nil)
 '(tramp-syntax (quote ftp))
 '(transient-mark-mode nil)
 '(truncate-partial-width-windows nil)
 '(type-break-good-rest-interval 300)
 '(type-break-interval 5400)
 '(undo-tree-visualizer-spacing 2 t)
 '(uniquify-buffer-name-style (quote post-forward-angle-brackets) nil (uniquify))
 '(uniquify-strip-common-suffix t)
 '(vc-follow-symlinks t)
 '(vc-handled-backends nil)
 '(vc-make-backup-files t)
 '(vc-stay-local t)
 '(visible-bell nil)
 '(wdired-allow-to-change-permissions (quote advanced))
 '(whitespace-style
   (quote
     (face tabs trailing lines space-before-tab newline indentation empty space-after-tab space-mark tab-mark newline-mark)))
 '(winring-prompt-on-create (quote usually))
 '(winring-show-names t)
 '(yas/prompt-functions (quote (yas/dropdown-prompt yas/ido-prompt yas/no-prompt)))
 '(yas/triggers-in-field t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "black" :foreground "wheat" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 130 :width normal :foundry "apple" :family "Source Code Pro"))))
 '(bold ((t (:foreground "lightcoral" :weight bold))))
 '(bold-italic ((t (:foreground "orange" :slant italic :weight bold))))
 '(circe-my-message-face ((t (:background "slateblue"))))
 '(circe-originator-face ((t (:weight bold))))
 '(cparen-around-andor-face ((t (:foreground "red" :weight bold))))
 '(cparen-around-begin-face ((t (:foreground "red"))))
 '(cparen-around-define-face ((t (:foreground "lightblue" :weight bold))))
 '(cparen-around-quote-face ((t (:foreground "brown"))))
 '(cursor ((t (:background "yellow"))))
 '(dircolors-face-objet ((t (:foreground "Gray"))) t)
 '(diredp-date-time ((t (:foreground "DarkGoldenrod3"))))
 '(diredp-dir-priv ((t (:box (:line-width 1 :color "grey75" :style released-button)))))
 '(diredp-file-name ((t (:foreground "LightBlue"))))
 '(diredp-file-suffix ((t (:foreground "yellow"))))
 '(ecb-default-highlight-face ((((class color) (background dark)) (:inherit ecb-default-general-face :background "slateblue"))))
 '(ecb-directory-face ((((class color) (background dark)) (:inherit ecb-default-highlight-face))))
 '(ecb-history-face ((((class color) (background dark)) (:inherit ecb-default-highlight-face))))
 '(ecb-method-face ((((class color) (background dark)) (:inherit ecb-default-highlight-face))))
 '(ecb-source-face ((((class color) (background dark)) (:inherit ecb-default-highlight-face))))
 '(ecb-token-header-face ((((class color) (background dark)) (:background "SeaGreen4"))))
 '(erc-direct-msg-face ((t (:foreground "cyan"))))
 '(erc-input-face ((t (:foreground "red" :weight bold))))
 '(erc-keyword-face ((t (:foreground "green" :weight bold))))
 '(font-latex-bold-face ((t (:inherit bold))))
 '(font-latex-italic-face ((t (:inherit italic))))
 '(font-latex-math-face ((((class color) (background dark)) (:inherit font-lock-number-face))))
 '(font-latex-slide-title-face ((t (:inherit font-lock-type-face :weight bold))))
 '(font-latex-string-face ((((class color) (background dark)) (:inherit font-lock-string-face))))
 '(font-latex-verbatim-face ((((class color) (background dark)) (:foreground "burlywood"))))
 '(font-lock-builtin-face ((((class color) (background dark)) (:foreground "lightsteelblue"))))
 '(font-lock-comment-delimiter-face ((default (:inherit font-lock-comment-face)) (((class color) (min-colors 16)) (:foreground "red"))))
 '(font-lock-comment-face ((t (:foreground "gray" :slant italic))))
 '(font-lock-doc-face ((t (:inherit font-lock-comment-face))))
 '(font-lock-floatnumber-face ((((class color) (background dark)) (:foreground "yellow4"))))
 '(font-lock-function-name-face ((((class color) (background dark)) (:foreground "lightblue2" :slant italic))))
 '(font-lock-hexnumber-face ((((class color) (background dark)) (:foreground "cyan"))))
 '(font-lock-keys-face ((((class color) (background dark)) (:foreground "yellow" :weight bold))))
 '(font-lock-number-face ((t (:foreground "violet" :weight bold))))
 '(font-lock-preprocessor-face ((t (:inherit font-lock-builtin-face :weight bold))))
 '(font-lock-pvs-function-type-face ((t (:foreground "blue"))) t)
 '(font-lock-pvs-parens-face ((t (:foreground "yellow"))) t)
 '(font-lock-pvs-record-parens-face ((t (:foreground "red"))) t)
 '(font-lock-pvs-set-brace-face ((t (:foreground "darkred"))) t)
 '(font-lock-pvs-table-face ((t (:foreground "black"))) t)
 '(font-lock-qt-face ((((class color) (background dark)) (:foreground "green2" :weight bold))))
 '(font-lock-string-face ((((class color) (background dark)) (:foreground "green3"))))
 '(font-lock-type-face ((((class color) (min-colors 88) (background dark)) (:foreground "PaleGreen" :weight bold))))
 '(font-lock-variable-name-face ((((class color) (background dark)) (:foreground "LightSkyBlue"))))
 '(gnus-group-mail-low ((t (:foreground "aquamarine3" :weight bold))))
 '(gnus-group-mail-low-empty ((((class color) (background dark)) (:foreground "aquamarine3"))))
 '(gnus-group-mail-low-empty-face ((((class color) (background dark)) (:foreground "aquamarine3"))) t)
 '(gnus-group-mail-low-face ((t (:foreground "aquamarine3" :weight bold))) t)
 '(gnus-header-content ((t (:foreground "green" :slant italic))))
 '(gnus-header-content-face ((t (:foreground "green" :slant italic))) t)
 '(gnus-header-name ((((class color) (background dark)) (:inherit message-header-name))))
 '(gnus-header-name-face ((((class color) (background dark)) (:foreground "LightGreen"))) t)
 '(highlight ((t (:background "#222222"))))
 '(hl-sexp-face ((((class color) (background dark)) (:inherit highlight))))
 '(hs-face ((t (:background "#656565" :box 1))))
 '(icicle-Completions-instruction-1 ((t (:foreground "lightblue"))))
 '(icicle-prompt-suffix ((((type x w32 mac) (class color)) (:foreground "dark blue"))))
 '(info-elisp-reference-item ((t (:background "DarkGrey"))))
 '(info-elisp-special-form-ref-item ((t (:background "DarkGray" :foreground "DarkMagenta"))))
 '(info-reference-item ((t (:background "LightGray" :foreground "gray35"))))
 '(info-string ((t (:inherit font-lock-string-face))))
 '(italic ((t (:foreground "Orchid" :slant italic))))
 '(lui-irc-colors-fg-1-face ((t (:foreground "darkgray"))))
 '(lui-irc-colors-fg-10-face ((((class color)) (:foreground "cyan3"))))
 '(lui-irc-colors-fg-12-face ((((class color)) (:foreground "lightblue"))))
 '(lui-irc-colors-fg-14-face ((((class color)) (:foreground "gray"))))
 '(lui-irc-colors-fg-2-face ((((class color)) (:foreground "cyan3"))))
 '(lui-irc-colors-fg-3-face ((t (:foreground "green2"))))
 '(lui-irc-colors-fg-4-face ((((class color)) (:foreground "orange"))))
 '(lui-irc-colors-fg-6-face ((((class color)) (:foreground "magenta"))))
 '(magit-branch ((t (:inherit magit-header :foreground "red" :weight bold))))
 '(magit-diff-file-header ((t (:inherit magit-header :foreground "grey70"))))
 '(magit-diff-hunk-header ((t (:inherit magit-header :foreground "lightblue"))))
 '(magit-item-mark ((((class color) (background dark)) (:foreground "lightgreen"))))
 '(magit-log-graph ((t (:foreground "grey50"))))
 '(magit-section-title ((t (:inherit magit-header :foreground "orange" :weight bold))))
 '(menu ((((type x-toolkit)) (:background "lightgrey" :foreground "black" :box (:line-width 1 :style released-button)))))
 '(message-header-name ((((class color) (background dark)) (:inherit highlight :foreground "LightGreen"))))
 '(message-header-other ((((class color) (background dark)) (:foreground "#ffaf00"))))
 '(message-separator ((((class color) (background dark)) (:inherit highlight :foreground "lightblue"))))
 '(mmm-code-submode-face ((t (:background "darkgreen"))))
 '(mmm-default-submode-face ((t (:background "black"))))
 '(mode-line ((t (:background "orange" :foreground "black" :box (:line-width 1 :style released-button)))))
 '(mode-line-inactive ((t (:inherit mode-line :background "grey30" :foreground "grey80" :box (:line-width -1 :color "grey40") :weight light))))
 '(mumamo-background-chunk-major ((((class color) (min-colors 88) (background dark)) nil)))
 '(mumamo-background-chunk-submode ((((class color) (min-colors 88) (background dark)) (:background "gray28"))))
 '(mumamo-background-chunk-submode1 ((((class color) (min-colors 88) (background dark)) (:background "grey30"))))
 '(mumamo-background-chunk-submode2 ((((class color) (min-colors 88) (background dark)) (:background "grey40"))))
 '(mumamo-background-chunk-submode3 ((((class color) (min-colors 88) (background dark)) (:background "grey50"))))
 '(mumamo-background-chunk-submode4 ((((class color) (min-colors 88) (background dark)) (:background "grey60"))))
 '(muse-header-1 ((t (:foreground "green" :weight bold))))
 '(muse-header-2 ((t (:foreground "lightblue" :weight bold))))
 '(muse-header-3 ((t (:foreground "grey" :weight bold))))
 '(muse-header-4 ((t (:weight bold))))
 '(org-agenda-date ((t (:foreground "orange"))) t)
 '(org-agenda-date-weekend ((t (:inherit org-agenda-date :strike-through t :weight bold))) t)
 '(org-column ((t (:inherit default :background "skyblue4"))))
 '(org-done ((t (:background "green4" :foreground "yellow" :weight bold))))
 '(org-hide ((((background dark)) (:foreground "gray23"))))
 '(org-mode-line-clock ((t (:inherit modeline :background "lightblue"))) t)
 '(org-todo ((t (:background "red" :foreground "yellow" :weight bold))))
 '(planner-note-headline-face ((t (:foreground "turquoise" :weight bold))))
 '(pp^L-highlight ((((type x w32 mac graphic) (class color)) (:foreground "red" :box (:line-width 3 :style pressed-button) :weight bold))))
 '(region ((t (:background "#3b6363"))))
 '(rst-level-1-face ((t (:background "grey50"))) t)
 '(rst-level-2-face ((t (:background "grey45"))) t)
 '(rst-level-3-face ((t (:background "grey40"))) t)
 '(rst-level-4-face ((t (:background "grey35"))) t)
 '(rst-level-5-face ((t (:background "grey30"))) t)
 '(rst-level-6-face ((t (:background "grey25"))) t)
 '(show-paren-match ((t (:background "black" :foreground "red" :weight bold))))
 '(show-paren-mismatch ((t (:background "magenta" :foreground "white" :weight bold))))
 '(tabbar-selected-face ((t (:inherit tabbar-default-face :foreground "blue" :box (:line-width 2 :color "white" :style pressed-button)))))
 '(tabbar-unselected-face ((t (:inherit tabbar-default-face :box (:line-width 2 :color "white" :style released-button)))))
 '(underline ((t (:foreground "seagreen3" :underline t))))
 '(variable-pitch ((t (:family "nimbus"))))
 '(w3-style-face-00001 ((t (:underline nil :weight normal :height 150))))
 '(w3-style-face-00002 ((t (:underline nil))))
 '(w3-style-face-00003 ((t (:underline nil :weight normal :height 150))))
 '(w3-style-face-00004 ((t (:underline nil :weight normal :height 150))))
 '(w3-style-face-00005 ((t (:underline nil :height 150))))
 '(w3-style-face-00007 ((t (:underline nil :height 150))))
 '(w3-style-face-00008 ((t (:underline nil :weight normal :height 150)))))
