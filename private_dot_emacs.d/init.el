
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

;; ---------------------------
;; Helm Configuration
;; ---------------------------
(use-package helm
  :ensure t
  :bind (("M-x"     . helm-M-x)
         ("C-x C-f" . helm-find-files)
         ("C-x b"   . helm-buffers-list))
  :config
  (helm-mode 1))

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
(add-to-list 'load-path "/usr/share/emacs/site-lisp/notmuch")
(setq notmuch-command "/usr/bin/notmuch")
(setq-default notmuch-search-oldest-first nil)

(use-package notmuch
  :ensure nil  ;; Do not install; already installed externally
  :commands notmuch
  :bind (("C-c n" . notmuch))
  :config
  (setq notmuch-search-oldest-first nil)
  (setq notmuch-fcc-dirs '(("ssserpent@gmail.com" . "ssserpent/sent")
                           ("kloza.marek@gmail.com" . "klozamarek/sent")))
  (setq notmuch-message-reject-fcc t)
  (setq notmuch-trash-folder "/home/ssserpent/.local/share/mail/ssserpent/trash"))

;; ---------------------------
;; mu4e Configuration
;; ---------------------------
(use-package mu4e
  :ensure nil  ;; Do not install; already installed externally
  :commands mu4e
  :bind (("C-c u" . mu4e))
  :config
  (setq mu4e-maildir "~/.local/share/mail")
  (setq mu4e-maildir-shortcuts
        '(("/ssserpent/inbox" . ?s)
          ("/ssserpent/archives" . ?a)
          ("/klozamarek/inbox" . ?k)
          ("/klozamarek/archives" . ?r)))
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
  (setq mu4e-context-policy 'pick-first)
  (setq mu4e-use-fancy-chars t)
  (setq mu4e-attachment-dir "~/Downloads")
  (setq mu4e-view-show-images t)
  (setq mu4e-sent-messages-behavior 'delete)
  (setq mu4e-view-attachment-default-handler "xdg-open")
  (setq mu4e-html2text-command "w3m -T text/html")
  (setq mu4e-headers-fields '((:human-date . 20)
                              (:flags . 6)
                              (:mailing-list . 10)
                              (:from . 22)
                              (:subject)))
  (setq mu4e-headers-draft-mark     '("D" . "💈")
        mu4e-headers-flagged-mark   '("F" . "📍")
        mu4e-headers-new-mark       '("N" . "🔥")
        mu4e-headers-passed-mark    '("P" . "❯")
        mu4e-headers-replied-mark   '("R" . "❮")
        mu4e-headers-seen-mark      '("S" . "✔ ")
        mu4e-headers-trashed-mark   '("T" . "💀")
        mu4e-headers-attach-mark    '("a" . "📎")
        mu4e-headers-encrypted-mark '("x" . "🔒")
        mu4e-headers-signed-mark    '("s" . "🔑")
        mu4e-headers-unread-mark    '("u" . "📨")
        mu4e-headers-list-mark      '("l" . "🔈")
        mu4e-headers-personal-mark  '("p" . "👨")
        mu4e-headers-calendar-mark  '("c" . "📅"))
  (setq headers-auto-update t)
  (setq update-interval (* 10 60))
  (setq mu4e-change-filenames-when-moving t)
  (setq mu4e-compose-dont-reply-to-self t))

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

;; ---------------------------
;; Custom Email Signature Setup
;; ---------------------------
(defun my-setup-email ()
  "Prompt user to select a From address and set the correct signature."
  (interactive)
  (let* ((chosen-from (completing-read "Choose From: "
                                        '("ssserpent@gmail.com" "kloza.marek@gmail.com") nil t)))
    (setq user-mail-address chosen-from)
    (message-remove-header "From")
    (message-add-header (format "From: %s" user-mail-address))
    (setq message-signature
          (cond
           ((string= chosen-from "ssserpent@gmail.com")
            "Pozdrowienia,\nMarek Kloza\nssserpent@gmail.com\n")
           ((string= chosen-from "kloza.marek@gmail.com")
            "Pozdrawiam,\nMarek Kloza\nkloza.marek@gmail.com\n")
           (t "Best,\nMarek Kloza")))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^-- $" nil t)
        (delete-region (line-beginning-position) (line-end-position)))
      (goto-char (point-min))
      (when (re-search-forward "^-- $" nil t)
        (delete-region (point) (point-max)))
      (goto-char (point-max))
      (insert "\n-- \n" message-signature))
    (message "Signature applied: %s" message-signature)))

(add-hook 'message-setup-hook #'my-setup-email)

;; ---------------------------
;; Custom Set Variables and Faces
;; ---------------------------
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(gruvbox-dark-hard))
 '(fido-vertical-mode nil)
 '(icomplete-mode nil)
 '(mu4e-headers-date-format "%d/%m/%Y %H:%M")
 '(package-install-selected-packages '(undo-tree csv-mode magit-delta gruvbox-theme))
 '(package-selected-packages
   '(wttrin helm fzf gruvbox-theme auto-complete csv-mode magit chezmoi mu4e-alert mu4e-column-faces)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
