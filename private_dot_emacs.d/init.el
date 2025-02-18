(autoload 'notmuch "notmuch" "notmuch mail" t)
(global-set-key (kbd "C-c n") #'notmuch)
(autoload 'mu4e "mu4e" "Launch mu4e email client." t)
(global-set-key (kbd "C-c u") #'mu4e)

(when (equal window-system 'x)
      (set-fontset-font "fontset-default" 'unicode "Dejavu Sans Mono")
      (set-face-font 'default "Inconsolata-10"))

(setq mailcap-prefer-mailcap-viewers t)

;; Mail User Agent (MUA) configuration
(setq mail-user-agent 'message-user-agent)

;; Configure sending emails via msmtp
(setq sendmail-program "/usr/bin/msmtp")
(setq message-sendmail-f-is-evil t)
(setq message-sendmail-extra-arguments '("--read-envelope-from"))
(setq message-send-mail-function 'message-send-mail-with-sendmail)

;; Configure user details
(setq user-full-name "Marek Kloza")
(setq user-mail-address "ssserpent@gmail.com")

;; Set up directory for drafts
(setq message-auto-save-directory "/home/ssserpent/.local/share/mail/ssserpent/drafts")
(setq message-kill-buffer-on-exit t)

;; Optional: Set up Notmuch maildir location
(setq notmuch-search-oldest-first nil)
(setq notmuch-fcc-dirs
      '(("ssserpent@gmail.com" . "ssserpent/sent")
        ("kloza.marek@gmail.com" . "klozamarek/sent")))

;; Set up Trash folder
(setq notmuch-message-reject-fcc nil) ;; Ensure Sent messages are saved
(setq notmuch-trash-folder "/home/ssserpent/.local/share/mail/ssserpent/trash")

;; Load gruvbox theme
(setq custom-safe-themes t) ;; Trust all themes
(load-theme 'wombat t)

;; Enable MELPA
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Ensure `use-package` is installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)  ;; Automatically install missing packages

;; Apropos can sort results by relevancy
(setq apropos-sort-by-scores t)

(add-to-list 'auto-mode-alist '("\\.csv\\'" . csv-mode))  ;; Open .csv in csv-mode

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
   '(marginalia vertico consult chezmoi undo-tree magit-delta gruvbox-theme goto-chg csv-mode annalist)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(defun my-setup-email ()
  "Prompt user to select a From address and set the correct signature."
  (interactive)
  (let* ((chosen-from (completing-read "Choose From: "
                                       '("ssserpent@gmail.com" "kloza.marek@gmail.com") nil t)))
    ;; Set the user-mail-address dynamically
    (setq user-mail-address chosen-from)

    ;; Remove old From field and set the new one
    (message-remove-header "From")
    (message-add-header (format "From: %s" user-mail-address))

    ;; Set the correct signature
    (setq message-signature
          (cond
           ((string= chosen-from "ssserpent@gmail.com")
            "Pozdrowienia,\nMarek Kloza\nssserpent@gmail.com\n")
           ((string= chosen-from "kloza.marek@gmail.com")
            "Pozdrawiam,\nMarek Kloza\nkloza.marek@gmail.com\n")
           (t "Best,\nMarek Kloza")))

    ;; Remove any existing signature and extra "--"
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^-- $" nil t)
        (delete-region (line-beginning-position) (line-end-position)))
      (goto-char (point-min))
      (when (re-search-forward "^-- $" nil t)
        (delete-region (point) (point-max)))
      (goto-char (point-max))
      (insert "\n-- \n" message-signature)))

  (message "Signature applied: %s" message-signature))

;; Ensure the function runs on message composition
(add-hook 'message-setup-hook #'my-setup-email)

;; Configure chezmoi.el with magit support
(use-package chezmoi
  :ensure t
  :config
  (global-set-key (kbd "C-c C-f") #'chezmoi-find)
  (global-set-key (kbd "C-c C-w") #'chezmoi-write)
  (setq-default chezmoi-template-display-p t))
  
;; Ensure magit is installed and setup chezmoi-magit-status
(use-package magit
  :ensure t
  :config
  (defun chezmoi-magit-status ()
    "Open magit-status for the chezmoi repository."
    (interactive)
    (magit-status (expand-file-name "~/.local/share/chezmoi")))
  (global-set-key (kbd "C-c m") #'chezmoi-magit-status))

;; Install and configure consult
(use-package consult
  :ensure t
  :bind
  (("C-x b" . consult-buffer)           ;; Better buffer switching
   ("M-y" . consult-yank-pop)           ;; Improved kill-ring history
   ("C-x r b" . consult-bookmark)       ;; Quickly jump to bookmarks
   ("M-g g" . consult-goto-line)        ;; Go to a specific line
   ("M-g o" . consult-outline)          ;; Navigate document headings
   ("M-g i" . consult-imenu)            ;; Jump to function or class in code
   ("M-g m" . consult-mark)             ;; Jump to marks
   ("M-s d" . consult-find)             ;; Search for files using `find`
   ("M-s g" . consult-grep)             ;; Search files using `grep`
   ("M-s r" . consult-ripgrep)          ;; Use `ripgrep` for searching
   ("M-s l" . consult-line)             ;; Search for lines in buffer
   ("M-s k" . consult-keep-lines)       ;; Keep only matching lines
   ("M-s u" . consult-focus-lines)))    ;; Focus on certain lines

;; Enable preview feature
(setq consult-preview-key 'any)

;; Use vertico for completion
(use-package vertico
  :ensure t
  :init
  (vertico-mode))

;; Optional: Enable marginalia for richer descriptions
(use-package marginalia
  :ensure t
  :init
  (marginalia-mode))

;; Set the base maildir path (adjust if your mail directory is elsewhere)
(setq mu4e-maildir "~/.local/share/mail")

;; Define maildir shortcuts (for fast access in the mu4e view)
(setq mu4e-maildir-shortcuts
      '( ("/ssserpent/inbox" . ?s)
         ("/ssserpent/archives" . ?a)
         ("/klozamarek/inbox" . ?k)
         ("/klozamarek/archives" . ?r) ))

;; Define contexts for each account using mu4easy/mu4e context support:
(require 'mu4e)         ;; Ensure mu4e is loaded
(require 'mu4e-context) ;; If using context support

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

;; Additonal settings for mu4e
(setq mu4e-context-policy 'pick-first) ; Use the first matching context automatically
(setq mu4e-use-fancy-chars t) ; use fancy non-ascii characters for mail therads
(setq mu4e-attachment-dir "~/Downloads") ; save attachment to my desktop (this can also be a function)
(setq mu4e-view-show-images t) ; attempt to show images when viewing messages
(setq mu4e-sent-messages-behavior 'delete) ; don't save messages to Sent Messages, Gmail/IMAP takes care of this
(setq mu4e-view-attachment-default-handler "xdg-open")
(setq mu4e-html2text-command "w3m -T text/html") ; how to hanfle html-formatted emails
(setq mu4e-headers-fields '((:human-date . 20)
                            (:flags . 6)
                            (:mailing-list . 10)
                            (:from . 22)
                            (:subject)))
(setq
   mu4e-headers-draft-mark     '("D" . "💈")
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
(setq message-kill-buffer-on-exit t)
(setq headers-auto-update t)
(setq update-interval (* 10 60))
(setq mu4e-change-filenames-when-moving t)
(setq mu4e-compose-dont-reply-to-self t)

;; Require and configure mu4e-alert
(require 'mu4e-alert)
(mu4e-alert-set-default-style 'libnotify)
(add-hook 'after-init-hook #'mu4e-alert-enable-notifications)
(add-hook 'after-init-hook #'mu4e-alert-enable-mode-line-display)

;; Optionally customize which mails trigger alerts
(setq mu4e-alert-interesting-mail-query "flag:unread AND NOT flag:trashed")

(use-package mu4e-column-faces
  :after mu4e
  :config (mu4e-column-faces-mode))

