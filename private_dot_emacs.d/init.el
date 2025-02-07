(autoload 'notmuch "notmuch" "notmuch mail" t)
(global-set-key (kbd "C-c n") #'notmuch)

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
(setq message-auto-save-directory "ssserpent/drafts/cur")
(setq message-kill-buffer-on-exit t)

;; Optional: Set up Notmuch maildir location
(setq notmuch-search-oldest-first nil)
(setq notmuch-fcc-dirs "ssserpent/sent/cur")

;; Set up Trash folder
(setq notmuch-message-reject-fcc nil) ;; Ensure Sent messages are saved
(setq notmuch-trash-folder "ssserpent/trash/cur")

;; Load gruvbox theme
(setq custom-safe-themes t) ;; Trust all themes
(load-theme 'gruvbox-dark-medium t)

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
 '(fido-vertical-mode t)
 '(package-install-selected-packages '(undo-tree csv-mode magit-delta gruvbox-theme)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
