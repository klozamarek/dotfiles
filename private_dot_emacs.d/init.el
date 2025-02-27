
;; ---------------------------
;; Package Management Setup
;; ---------------------------
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

;; ---------------------------
;; Global Settings
;; ---------------------------
(setq apropos-sort-by-scores t)
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq isearch-lazy-count t) ; enable count of matches in Isearch
(setq lazy-count-prefix-format "(%s/%s) ") ; format count of matches in Isearch
(setq lazy-count-suffix-format nil) ; no suffix 
(setq search-whitespace-regexp ".*?") ; whitespace matches all string between two strings in Isearch 
(setq make-backup-files nil) ; do not make backup files
(setq backup-inhibited nil) ; Not sure if needed, given `make-backup-files'
(setq create-lockfiles nil)
(setq custom-file (make-temp-file "emacs-custom-")) ; Disable the damn thing by making it disposable.
(icomplete-mode -1)  ;; Ensure icomplete-mode is disabled
(global-set-key (kbd "C-r") 'redraw-display)
(add-hook 'window-configuration-change-hook 'redraw-display)

;; ---------------------------
;; Helm Configuration
;; ---------------------------
(use-package helm
  :ensure t
  :bind (("M-x" . helm-M-x))
  :config
    (require 'helm) ;; Ensure Helm is loaded before accessing helm-map
    (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)  ;; Rebind tab to do persistent action
    (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)    ;; Make TAB work in terminal
    (define-key helm-map (kbd "C-z") 'helm-select-action))               ;; List actions using C-z
    (helm-mode 1)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(setq helm-buffers-fuzzy-matching t
      helm-recentf-fuzzy-match t
      helm-M-x-fuzzy-match t)
;; ---------------------------
;; Wttr Configuration
;; ---------------------------
(use-package wttrin
  :ensure t
  :commands (wttrin)
  :bind ("C-c w" . wttrin)
  :custom
  (wttrin-default-locations '("Wieliczka" "Brusy" "Gdańsk" "Warszawa"))
  (wttrin-default-languages '("Accept-Language" . "pl-PL")))

;; ---------------------------
;; Auto-Complete Configuration
;; ---------------------------
(use-package auto-complete
  :ensure t
  :config
  (require 'auto-complete-config)
  (ac-config-default)
  (global-auto-complete-mode t))

;; ---------------------------
;; fzf-find-file
;; ---------------------------
(use-package fzf
  :bind ("C-x f" . fzf-find-file)
  :config
  (setq fzf/args "-x --color bw --print-query --margin=1,0 --no-hscroll"
        fzf/executable "fzf"
        fzf/git-grep-args "-i --line-number %s"
        fzf/grep-command "grep -nrH"
        fzf/position-bottom t
        fzf/window-height 15))

;; ---------------------------
;; ripgrep-all: Search for content snippets within files
;; ---------------------------
(defun my-rga (query &optional dir)
  "Search for QUERY in DIR (default is current directory) using the external rga binary.
Uses the --vimgrep flag so that results are compatible with grep-mode."
  (interactive
   (list (read-string "Search query: ")
         (read-directory-name "Directory: " default-directory)))
  (let ((default-directory dir))
    (compilation-start
     (format "/usr/bin/rga --vimgrep %s" (shell-quote-argument query))
     'grep-mode)))
(global-set-key (kbd "C-c g") 'my-rga)

;; ---------------------------
;; CSV Mode for .csv Files
;; ---------------------------
(use-package csv-mode
  :mode ("\\.csv\\'" . csv-mode))

;; ---------------------------
;; Gruvbox Theme
;; ---------------------------
(use-package gruvbox-theme
  :config
  (setq custom-safe-themes t)
  (load-theme 'gruvbox-dark-hard t))

;; ---------------------------
;; Email (Message) Configuration
;; ---------------------------
(use-package message
  :ensure nil  ;; built-in, so do not attempt to install
  :config
  (setq mailcap-prefer-mailcap-viewers t)
  (setq mail-user-agent 'message-user-agent)
  (setq sendmail-program "/usr/bin/msmtp")
  (setq message-sendmail-f-is-evil t)
  (setq message-sendmail-extra-arguments '("--read-envelope-from"))
  (setq message-send-mail-function 'message-send-mail-with-sendmail)
  (setq user-full-name "Marek Kloza")
  (setq user-mail-address "ssserpent@gmail.com")
  (setq message-auto-save-directory "/home/ssserpent/.local/share/mail/ssserpent/drafts")
  (setq message-kill-buffer-on-exit t))

;; ---------------------------
;; Notmuch Configuration
;; ---------------------------
(use-package notmuch
  :load-path "/usr/share/emacs/site-lisp/"
  :defer t
  :commands (notmuch notmuch-mua-new-mail))

;;; Account settings
(use-package notmuch
  :defer t
  :config
  (setq notmuch-identities '("Marek Kloza <ssserpent@gmail.com>" "Marek Kloza <klozamarek@gmail.com>"))
  (setq notmuch-fcc-dirs '(("ssserpent@gmail.com" . "ssserpent/sent")
                           ("kloza.marek@gmail.com" . "klozamarek/sent")))
  (setq notmuch-message-reject-fcc t)
  (setq notmuch-trash-folder "/home/ssserpent/.local/share/mail/ssserpent/trash"))
  (setq message-signature "Pozdrowienia,\nMarek Kloza")
;  (add-hook 'message-setup-hook 'message-insert-signature)

  ;;;; General UI
  (use-package notmuch
  :defer t
  :config
  (setq notmuch-show-logo nil
        notmuch-column-control 1.0
        notmuch-hello-auto-refresh t
        notmuch-hello-recent-searches-max 20
        notmuch-hello-thousands-separator ""
        notmuch-hello-sections '(notmuch-hello-insert-saved-searches)
        notmuch-show-all-tags-list t))

  ;;;; Search
(use-package notmuch
  :defer t
  :config
  (setq notmuch-search-oldest-first nil)
  (setq notmuch-search-result-format
        '(("date" . "%12s  ")
          ("count" . "%-7s  ")
          ("authors" . "%-20s  ")
          ("subject" . "%-80s  ")
          ("tags" . "(%s)")))
  (setq notmuch-tree-result-format
        '(("date" . "%12s  ")
          ("authors" . "%-20s  ")
          ((("tree" . "%s")
            ("subject" . "%s"))
           . " %-80s  ")
          ("tags" . "(%s)")))
  (setq notmuch-search-line-faces
        '(("unread" . notmuch-search-unread-face)
          ;; ;; NOTE 2022-09-19: I disable this because I add a cosmeic
          ;; ;; emoji via `notmuch-tag-formats'.  This way I do not get
          ;; ;; an intense style which is very distracting when I filter
          ;; ;; my mail to include this tag.
          ;;
          ;; ("flag" . notmuch-search-flagged-face)
          ;;
          ;; Using `italic' instead is just fine.  Though I also tried
          ;; it without any face and I was okay with it.  The upside of
          ;; having a face is that you can identify the message even
          ;; when the window is split and you don't see the tags.
          ("flag" . italic)))
  (setq notmuch-show-empty-saved-searches t)
  (setq notmuch-saved-searches
        `(( :name "📥 inbox: ssserpent"
            :query "tag:inbox and folder:ssserpent/inbox/"
            :sort-order newest-first
            :key ,(kbd "s"))
          ( :name "📚 inbox: klozamarek"
            :query "tag:inbox and folder:klozamarek/inbox/"
            :sort-order newest-first
            :key ,(kbd "k"))
          ( :name "📮 all mail: ssserpent"
            :query "folder:ssserpent/archives"
            :sort-order newest-first
            :key ,(kbd "S"))
          ( :name "📦 all mail: klozamarek"
            :query "folder:klozamarek/archives"
            :sort-order newest-first
            :key ,(kbd "K"))
          ( :name "💡 unread"
            :query "tag:unread"
            :sort-order newest-first
            :key ,(kbd "u"))
          ( :name "📆 this week"
            :query "date:this_week"
            :sort-order newest-first
            :key ,(kbd "w"))
          ( :name "📨 all sent"
            :query "tag:sent"
            :sort-order newest-first
            :key ,(kbd "t"))
          ( :name "📎 has: pdf"
            :query "tag:pdf"
            :sort-order newest-first
            :key ,(kbd "p"))
          ( :name "📣 lists"
            :query "tag:list"
            :sort-order newest-first
            :key ,(kbd "l"))
          ( :name "🧹 thrash"
            :query "tag:deleted"
            :sort-order newest-first
            :key ,(kbd "d"))
          ( :name "💼 from: daxa"
            :query "tag:daxa"
            :sort-order newest-first
            :key ,(kbd "x")))))
;;;; Tags
(use-package notmuch
  :defer t
  :config
  (setq notmuch-archive-tags '("+archived")
        notmuch-message-replied-tags '("+replied")
        notmuch-message-forwarded-tags '("+forwarded")
        notmuch-show-mark-read-tags '("-unread")
        notmuch-draft-tags '("+draft")
        ; notmuch-draft-folder "drafts"
        notmuch-draft-save-plaintext 'ask)
  ;; All emoji are cosmetic.  The tags are just the text.
  (setq notmuch-tag-formats
        '(("unread" (propertize tag 'face 'notmuch-tag-unread))
          ("flag" (propertize tag 'face 'notmuch-tag-flagged)
           (concat tag "🚩")))
        notmuch-tag-deleted-formats
        '(("unread" (notmuch-apply-face bare-tag 'notmuch-tag-deleted)
           (concat "👁️" tag))
          (".*" (notmuch-apply-face tag 'notmuch-tag-deleted)
           (concat "🚫" tag)))
        notmuch-tag-added-formats
        '(("deleted" (notmuch-apply-face tag 'notmuch-tag-added)
           (concat "💥" tag))
          (".*" (notmuch-apply-face tag 'notmuch-tag-added)
           (concat "🏷️" tag)))))

;;;; Reading messages
(use-package notmuch
  :defer t
  :config
  (setq notmuch-show-relative-dates t)
  (setq notmuch-show-all-multipart/alternative-parts nil)
  (setq notmuch-show-indent-messages-width 0)
  (setq notmuch-show-indent-multipart nil)
  (setq notmuch-show-part-button-default-action 'notmuch-show-view-part)
  (setq notmuch-show-text/html-blocked-images ".") ; block everything
  (setq notmuch-wash-wrap-lines-length 120)
  (setq notmuch-unthreaded-show-out nil)
  (setq notmuch-message-headers '("To" "Cc" "Subject" "Date"))
  (setq notmuch-message-headers-visible t)

  (let ((count most-positive-fixnum)) ; I don't like the buttonisation of long quotes
    (setq notmuch-wash-citation-lines-prefix count
          notmuch-wash-citation-lines-suffix count)))

;;;; Email composition
(use-package notmuch
  :defer t
  :config
  (setq notmuch-mua-compose-in 'current-window)
  (setq notmuch-mua-hidden-headers nil)
  (setq notmuch-address-command 'internal)
  (setq notmuch-address-use-company nil)
  (setq notmuch-always-prompt-for-sender t)
  (setq notmuch-mua-cite-function 'message-cite-original-without-signature)
  (setq notmuch-mua-reply-insert-header-p-function 'notmuch-show-reply-insert-header-p-never)
  (setq notmuch-mua-user-agent-function nil)
  (setq notmuch-maildir-use-notmuch-insert t)
  (setq notmuch-crypto-process-mime t)
  (setq notmuch-crypto-get-keys-asynchronously t)
  (setq notmuch-mua-attachment-regexp   ; see `notmuch-mua-send-hook'
        (concat "\\b\\(attache\\?ment\\|attached\\|attach\\|załącz\\w*\\)\\b"))
  
  (defun prot-notmuch-message-tab ()
    "Override for `message-tab' to enforce header line check.
More specifically, perform address completion when on a relevant header
line, because `message-tab' sometimes (not sure when/how) fails to do
that and instead tries to complete against dictionary entries."
    (interactive nil message-mode)
    (cond
     ((save-excursion
        (goto-char (line-beginning-position))
        (looking-at notmuch-address-completion-headers-regexp))
      (notmuch-address-expand-name)
      ;; Completion was performed; nothing else to do.
      nil)
     (message-tab-body-function (funcall message-tab-body-function))
     (t (funcall (or (lookup-key text-mode-map "\t")
                     (lookup-key global-map "\t")
                     'indent-relative)))))

  (advice-add #'message-tab :override #'prot-notmuch-message-tab))

;;;; Hooks and key bindings
(use-package notmuch
  :hook
  (notmuch-mua-send . notmuch-mua-attachment-check) ; also see `notmuch-mua-attachment-regexp'
  (notmuch-show . (lambda () (setq-local header-line-format nil)))
  :config
  ; (remove-hook 'notmuch-show-hook #'notmuch-show-turn-on-visual-line-mode)
  ; (remove-hook 'notmuch-search-hook #'notmuch-hl-line-mode) ; Check my `lin' package
  :bind
  ( :map global-map
    ("C-c n" . notmuch)
    ("C-x n" . notmuch-mua-new-mail) ; override `compose-mail'
    :map notmuch-search-mode-map ; I normally don't use the tree view, otherwise check `notmuch-tree-mode-map'
    ("a" . notmuch-search-add-tag) ; the default is too easy to hit accidentally and I do not archive stuff
    ("A" . nil)
    ("/" . notmuch-search-filter) ; alias for l
    ("r" . notmuch-search-reply-to-thread) ; easier to reply to all by default
    ("R" . notmuch-search-reply-to-thread-sender)
    :map notmuch-show-mode-map
    ("a" . notmuch-show-advance-and-archive) ; the default is too easy to hit accidentally and I do not archive stuff
    ("A" . nil)
    ("r" . notmuch-show-reply) ; easier to reply to all by default
    ("R" . notmuch-show-reply-sender)
    :map notmuch-hello-mode-map
    ("C-<tab>" . nil)))

;;; My own tweaks for notmuch (prot-notmuch.el)
(add-to-list 'load-path "~/.emacs.d/lisp/")
(use-package prot-notmuch
  :ensure nil
  :after notmuch
  :bind
  ( :map notmuch-search-mode-map
    ("D" . prot-notmuch-search-delete-thread)
    ("S" . prot-notmuch-search-spam-thread)
    ("g" . prot-notmuch-refresh-buffer)
    :map notmuch-show-mode-map
    ("D" . prot-notmuch-show-delete-message)
    ("S" . prot-notmuch-show-spam-message)
    :map notmuch-show-stash-map
    ("S" . prot-notmuch-stash-sourcehut-link))
  :config
  ;; Those are for the actions that are available after pressing 'k'
  ;; (`notmuch-tag-jump').  For direct actions, refer to the key
  ;; bindings below.
  (setq notmuch-tagging-keys
        `((,(kbd "d") prot-notmuch-mark-delete-tags "💥 Mark for deletion")
          (,(kbd "f") prot-notmuch-mark-flag-tags "🚩 Flag as important")
          (,(kbd "s") prot-notmuch-mark-spam-tags "🔥 Mark as spam")
          (,(kbd "r") ("-unread") "👁️ Mark as read")
          (,(kbd "u") ("+unread") "🗨️ Mark as unread")))

  ;; These emoji are purely cosmetic.  The tag remains the same: I
  ;; would not like to input emoji for searching.
  (add-to-list 'notmuch-tag-formats '("encrypted" (concat tag "🔒")))
  (add-to-list 'notmuch-tag-formats '("attachment" (concat tag "📎")))
  (add-to-list 'notmuch-tag-formats '("coach" (concat tag "🏆")))
  (add-to-list 'notmuch-tag-formats '("package" (concat tag "🗂️"))))

;;; notmuch-indicator (another package of mine)
(use-package notmuch-indicator
  :ensure t
  :after notmuch
  :config
  (setq notmuch-indicator-args
        '(( :terms "tag:unread and tag:inbox"
            ;; :label "[A] "
            :label "📥 "
            :label-face prot-modeline-indicator-green
            :counter-face prot-modeline-indicator-green)
          ( :terms "tag:unread and tag:inbox and folder:ssserpent/inbox"
            ;; :label "[U] "
            :label "💬 "
            :label-face prot-modeline-indicator-cyan
            :counter-face prot-modeline-indicator-cyan)
          ( :terms "tag:unread and tag:inbox and folder:klozamarek/inbox"
            ;; :label "[P] "
            :label "📦 "
            :label-face prot-modeline-indicator-magenta
            :counter-face prot-modeline-indicator-magenta)
          ( :terms "tag:unread"
            ;; :label "[C] "
            :label "🏆 "
            :label-face prot-modeline-indicator-red
            :counter-face prot-modeline-indicator-red))

        notmuch-indicator-refresh-count (* 60 3)
        notmuch-indicator-hide-empty-counters t
        notmuch-indicator-force-refresh-commands '(notmuch-refresh-this-buffer))

  ;; I control its placement myself.  See prot-emacs-modeline.el where
  ;; I set the `mode-line-format'.
  (setq notmuch-indicator-add-to-mode-line-misc-info nil)

  (notmuch-indicator-mode 1))

(provide 'prot-emacs-notmuch)

;; ---------------------------
;; mu4e Configuration
;; ---------------------------
(use-package mu4e
  :ensure nil  ;; Do not install; already installed externally
  :commands mu4e
  :bind (("C-c u" . mu4e))
  :config
  (setq mu4e-maildir "~/.local/share/mail")
  (setq mu4e-attachment-dir (expand-file-name "~/Downloads/mail-attachments/"))
  (setq mu4e-maildir-shortcuts
        '(("/ssserpent/inbox" . ?s)
          ("/ssserpent/archives" . ?a)
          ("/klozamarek/inbox" . ?k)
          ("/klozamarek/archives" . ?r)))
  (setq mu4e-view-show-images t)
  (setq mu4e-sent-messages-behavior 'delete)
  (setq mu4e-view-attachment-default-handler "xdg-open")
  (setq mu4e-html2text-command "w3m -T text/html")
  ; (setq mu4e-headers-fields '((:human-date . 20)
  ;                             (:flags . 6)
  ;                             (:mailing-list . 10)
  ;                             (:from . 22)
  ;                             (:subject)))
  (setq mu4e-use-fancy-chars t ; Cool idea, but they create misalignments
        mu4e-headers-draft-mark     '("D" . "🛠️")
        mu4e-headers-flagged-mark   '("F" . "🚩")
        mu4e-headers-new-mark       '("N" . "🔥")
        mu4e-headers-passed-mark    '("P" . "📨")
        mu4e-headers-replied-mark   '("R" . "🖊️")
        mu4e-headers-seen-mark      '("S" . "👁️")
        mu4e-headers-trashed-mark   '("T" . "🚫")
        mu4e-headers-attach-mark    '("a" . "📎")
        mu4e-headers-encrypted-mark '("x" . "🔒")
        mu4e-headers-signed-mark    '("s" . "🔑")
        mu4e-headers-unread-mark    '("u" . "💬")
        mu4e-headers-list-mark      '("l" . "📬")
        mu4e-headers-personal-mark  '("p" . "👴")
        mu4e-headers-calendar-mark  '("c" . "📅"))
  (setq mu4e-headers-auto-update t)
  (setq mu4e-get-mail-command "true" ; I auto-fetch with a systemd timer
        mu4e-update-interval nil)
  (setq mu4e-hide-index-messages t)
  (setq mu4e-change-filenames-when-moving t)
  (setq mu4e-compose-dont-reply-to-self t))
  (setq mu4e-headers-date-format "%d/%m/%Y %H:%M")  ;; Set mu4e date format
  (setq mu4e-marks
        '((refile
           :char ("r" . "▶")
           :prompt "refile"
           :dyn-target (lambda (target msg) (mu4e-get-refile-folder msg))
           :action (lambda (docid msg target)
                     (mu4e--server-move docid (mu4e--mark-check-target target) "-N")))
          (delete
           :char ("D" . "🚫")
           :prompt "Delete"
           :show-target (lambda (target) "delete")
           :action (lambda (docid msg target) (mu4e--server-remove docid)))
          (flag
           :char ("+" . "🚩")
           :prompt "+flag"
           :show-target (lambda (target) "flag")
           :action (lambda (docid msg target)
                     (mu4e--server-move docid nil "+F-u-N")))
          (move
           :char ("m" . "▷")
           :prompt "move"
           :ask-target  mu4e--mark-get-move-target
           :action (lambda (docid msg target)
                     (mu4e--server-move docid (mu4e--mark-check-target target) "-N")))
          (read
           :char    ("!" . "👁️")
           :prompt "!read"
           :show-target (lambda (target) "read")
           :action (lambda (docid msg target) (mu4e--server-move docid nil "+S-u-N")))
          (trash
           :char ("d" . "🚫")
           :prompt "dtrash"
           :dyn-target (lambda (target msg) (mu4e-get-trash-folder msg))
           :action (lambda (docid msg target)
                     (mu4e--server-move docid
                                        (mu4e--mark-check-target target) "+T-N")))
          (unflag
           :char    ("-" . "➖")
           :prompt "-unflag"
           :show-target (lambda (target) "unflag")
           :action (lambda (docid msg target) (mu4e--server-move docid nil "-F-N")))
          (untrash
           :char   ("=" . "▲")
           :prompt "=untrash"
           :show-target (lambda (target) "untrash")
           :action (lambda (docid msg target) (mu4e--server-move docid nil "-T")))
          (unread
           :char    ("?" . "💬")
           :prompt "?unread"
           :show-target (lambda (target) "unread")
           :action (lambda (docid msg target) (mu4e--server-move docid nil "-S+u-N")))
          (unmark
           :char  " "
           :prompt "unmark"
           :action (mu4e-error "No action for unmarking"))
          (action
           :char ( "a" . "◯")
           :prompt "action"
           :ask-target  (lambda () (mu4e-read-option "Action: " mu4e-headers-actions))
           :action  (lambda (docid msg actionfunc)
                      (save-excursion
                        (when (mu4e~headers-goto-docid docid)
                          (mu4e-headers-action actionfunc)))))
          (something
           :char  ("*" . "✱")
           :prompt "*something"
           :action (mu4e-error "No action for deferred mark"))))

  (setq mu4e-modeline-support t
        mu4e-modeline-unread-items '("U:" . "[U]")
        mu4e-modeline-all-read '("R:" . "[R]")
        mu4e-modeline-all-clear '("C:" . "[C]")
        mu4e-modeline-max-width 42)

  (setq mu4e-notification-support t
        ;; TODO 2024-02-26: Write custom mu4e notification function.
        mu4e-notification-filter #'mu4e--default-notification-filter)

  (setq mu4e-headers-advance-after-mark nil)
  (setq mu4e-headers-auto-update t)
  (setq mu4e-headers-date-format "%F %a, %T")
  (setq mu4e-headers-time-format "%R")
  (setq mu4e-headers-long-date-format "%F, %R")
  (setq mu4e-headers-leave-behavior 'apply)

  (setq mu4e-headers-fields
        '((:date . 26)
          (:flags . 8)
          (:from . 20)
          (:subject)))

  (setq mu4e-get-mail-command "true" ; I auto-fetch with a systemd timer
        mu4e-update-interval nil)
  (setq mu4e-hide-index-messages t)

  (setq mu4e-read-option-use-builtin nil
        mu4e-completing-read-function 'completing-read)

  (setq mu4e-search-results-limit -1
        mu4e-search-sort-field :date
        mu4e-search-sort-direction 'descending)

  (setq mu4e-view-show-addresses t)
  (setq mu4e-split-view 'horizontal)

  (setq mu4e-index-lazy-check t)
  (setq mu4e-change-filenames-when-moving t) ; better for `mbsync'?
  (setq mu4e-display-update-status-in-modeline nil)
  (setq mu4e-headers-include-related nil)
  (setq mu4e-view-auto-mark-as-read t)

  (setq mu4e-compose-complete-addresses nil
        mu4e-compose-complete-only-personal t)

  (setq mu4e-context-policy 'pick-first
        mu4e-compose-context-policy nil)

(require 'mu4e-context)
(setq mu4e-contexts
        (list
         (make-mu4e-context
          :name "ssserpent"
          :match-func (lambda (msg)
                        (when msg
                          (string-match-p "^/ssserpent" (mu4e-message-field msg :maildir))))
          :vars '((user-mail-address     . "ssserpent@gmail.com")
                  (user-full-name        . "Marek Kloza")
                  (mu4e-drafts-folder    . "/ssserpent/drafts")
                  (mu4e-sent-folder      . "/ssserpent/sent")
                  (mu4e-trash-folder     . "/ssserpent/trash")
                  (mu4e-refile-folder    . "/ssserpent/archives")
                  (mu4e-get-mail-command . "mbsync -a")))
         (make-mu4e-context
          :name "klozamarek"
          :match-func (lambda (msg)
                        (when msg
                          (string-match-p "^/klozamarek" (mu4e-message-field msg :maildir))))
          :vars '((user-mail-address     . "klozamarek@gmail.com")
                  (user-full-name        . "Marek KLoza")
                  (mu4e-drafts-folder    . "/klozamarek/drafts")
                  (mu4e-sent-folder      . "/klozamarek/sent")
                  (mu4e-trash-folder     . "/klozamarek/trash")
                  (mu4e-refile-folder    . "/klozamarek/archives")
                  (mu4e-get-mail-command . "mbsync -a")))))

;; ---------------------------
;; mu4e-alert Configuration
;; ---------------------------
(use-package mu4e-alert
  :after mu4e
  :config
  (mu4e-alert-set-default-style 'libnotify)
  (add-hook 'after-init-hook #'mu4e-alert-enable-notifications)
  (add-hook 'after-init-hook #'mu4e-alert-enable-mode-line-display)
  (setq mu4e-alert-interesting-mail-query "flag:unread AND NOT flag:trashed"))

;; ---------------------------
;; mu4e-column-faces Configuration
;; ---------------------------
(use-package mu4e-column-faces
  :after mu4e
  :config
  (mu4e-column-faces-mode))

;; ---------------------------
;; Chezmoi and Magit Integration
;; ---------------------------
(use-package chezmoi
  :config
  (global-set-key (kbd "C-c C-f") #'chezmoi-find)
  (global-set-key (kbd "C-c C-w") #'chezmoi-write)
  (setq-default chezmoi-template-display-p t))

(use-package magit
  :config
  (defun chezmoi-magit-status ()
    "Open magit-status for the chezmoi repository."
    (interactive)
    (magit-status (expand-file-name "~/.local/share/chezmoi")))
  (global-set-key (kbd "C-c m") #'chezmoi-magit-status))

; ;; ---------------------------
; ;; Custom Email Signature Setup
; ;; ---------------------------
; (defun my-setup-email ()
;   "Prompt user to select a From address and set the correct signature."
;   (interactive)
;   (let* ((chosen-from (completing-read "Choose From: "
;                                         '("ssserpent@gmail.com" "kloza.marek@gmail.com") nil t)))
;     (setq user-mail-address chosen-from)
;     (message-remove-header "From")
;     (message-add-header (format "From: %s" user-mail-address))
;     (setq message-signature
;           (cond
;            ((string= chosen-from "ssserpent@gmail.com")
;             "Pozdrowienia,\nMarek Kloza\nssserpent@gmail.com\n")
;            ((string= chosen-from "kloza.marek@gmail.com")
;             "Pozdrawiam,\nMarek Kloza\nkloza.marek@gmail.com\n")
;            (t "Best,\nMarek Kloza")))
;     (save-excursion
;       (goto-char (point-min))
;       (while (re-search-forward "^-- $" nil t)
;         (delete-region (line-beginning-position) (line-end-position)))
;       (goto-char (point-min))
;       (when (re-search-forward "^-- $" nil t)
;         (delete-region (point) (point-max)))
;       (goto-char (point-max))
;       (insert "\n-- \n" message-signature))
;     (message "Signature applied: %s" message-signature)))

; (add-hook 'message-setup-hook #'my-setup-email)

;; ---------------------------
;; Gruvbox theme
;; ---------------------------
(use-package gruvbox-theme
  :ensure t
  :config
  (load-theme 'gruvbox-dark-hard t))

;; ---------------------------
;; Custom Set Variables and Faces
;; ---------------------------
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(helm auto-complete)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
