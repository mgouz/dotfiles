;; setq warning-minimum-level :error  ; don't show warning buffer unless error
					; (setq org-directory "~/org/")
(setq org-clock-sound t)

;; My Init file

;;; THIS NEEDS TO BE AT THE TOP
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(add-to-list 'load-path "./modules")

;; bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

					; (add-to-list 'load-path "/opt/homebrew/Cellar/mu/1.12.8/share/emacs/site-lisp/mu/mu4e")


(when (file-exists-p "/opt/homebrew/Cellar/mu/1.12.8/share/emacs/site-lisp/mu/mu4e")
  (require 'mu4e)
  ;; use mu4e for e-mail in emacs
  (setq mail-user-agent 'mu4e-user-agent)
  (setq mu4e-drafts-folder "/[Gmail].Drafts")
  (setq mu4e-sent-folder   "/[Gmail].Sent Mail")
  (setq mu4e-trash-folder  "/[Gmail].Trash")


  ;; don't save message to Sent Messages, Gmail/IMAP takes care of this
  (setq mu4e-sent-messages-behavior 'delete)

  ;; (See the documentation for `mu4e-sent-messages-behavrio` if you have
  ;; additional non-Gmail addresses and want to assign them different behavior
  ;; )

  (setq mu4e-maildir-shortcuts
	'( (:maildir "/INBOX"              :key ?i)
	   (:maildir "/[Gmail].Sent Mail"  :key ?s)
	   (:maildir "/[Gmail].Trash"      :key ?t)
	   (:maildir "/[Gmail].All Mail"   :key ?a)))

  (add-to-list 'mu4e-bookmarks
	       ;; ':favorite t' i.e. use this one for the modeline
	       '(:query "maildir:/inbox" :name "Inbox" :key ?i :favorite t))

  ;; allow for updating mail using U in the main view:
  (setq mu4e-get-mail-command "offlineimap")

  ;; something about ourselves
  (setq user-mail-address "mattgouzoulis@gmail.com"
	user-full-name "Matthew Gouozulis"
	message-signature
	(concat "Best regards,\n"
		"Matthew Gouzoulis"))

  ;; sending mail -- replace USERNAME with your gmail username
  ;; also, make sure the gnutls command line utils are installed
  (require 'smtpmail)
  (setq message-send-mail-function 'smtpmail-send-it
	starttls-use-gnutls t
	smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
	smtpmail-auth-credentials
	'(("smtp.gmail.com" 587 "mattgouzoulis@gmail.com" nil))
	smtpmail-default-smtp-server "smtp.gmail.com"
	smtpmail-smtp-server "smtp.gmail.com"
	smtpmail-smtp-service 587)

  (setq message-kill-buffer-on-exit t)

  ;; use 'fancy' non-ascii characters in various places in mu4e
  (setq mu4e-use-fancy-chars t)

  ;; save attachment to my desktop (this can also be a function)
  (setq mu4e-attachment-dir "~/Desktop")

  ;; attempt to show images when viewing messages
  (setq mu4e-view-show-images t)
  )

(add-to-list 'load-path (concat user-emacs-directory "/modules"))
(require 'mg-ui)
(require 'mg-evil)
(require 'mg-wip)
(require 'mg-git)
(require 'mg-completion-minibuffer)
(require 'mg-completion-point)
(require 'mg-org)
(require 'mg-debugging)

(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1))


(use-package yasnippet-snippets
  :ensure t
  :after yasnippet
  )

(use-package emacs
  :custom
  ;; Support opening new minibuffers from inside existing minibuffers.
  (enable-recursive-minibuffers t)
  ;; Hide commands in M-x which do not apply to the current mode.  Corfu
  ;; commands are hidden, since they are not used via M-x. This setting is
  ;; useful beyond Corfu.
  (read-extended-command-predicate #'command-completion-default-include-p)
  :config
  ;; Mimicing a Browser's keymaps
  (keymap-global-set (kbd "s-}") 'tab-bar-switch-to-next-tab 1)
  (keymap-global-set (kbd "s-{") 'tab-bar-switch-to-prev-tab 1)
  (keymap-global-set (kbd "s-T") 'tab-undo 1)
  (keymap-global-set (kbd "s-t") (lambda () (interactive) (tab-new) (switch-to-buffer "*scratch*")) 1)
  (keymap-global-set (kbd "s-w") 'tab-close 1)
  (keymap-global-set (kbd "s-[") 'previous-buffer 1)
  (keymap-global-set (kbd "s-]") 'next-buffer 1)

  ;; Misc key maps
  (global-set-key (kbd "s-P") 'execute-extended-command) ; Mimic VSCode's command pallete
  (global-set-key (kbd "S-s-<return>") 'maximize-window) ; C-w = to balance windows again (a-la iterm2)

  ;; TODO swap this out for the appropriate formatting function based on mode of the buffer
  (global-set-key (kbd "M-s-l") 'eglot-format) ; C-w = to balance windows again (a-la iterm2)

  ;; When I press this, I want to follow to a new window
  ;; (global-set-key (kbd "s-<return>") 'ret-new-buffer)
  ;; (global-set-key (kbd "s-<mouse-1>") 'ret-new-buffer)

  ;; Eglot settings for run
  ;; (add-hook 'c- 'eglot-ensure)

  ;; UI
  (tool-bar-mode 0) ;; Remove tool bar
  (scroll-bar-mode 0) ; ;remove scroll bar
  (global-set-key (kbd "s-b") 'treemacs) ; Mimic VSCode's file explorer

  (setq inhibit-startup-message t)
  (set-fringe-mode 10)
  (setq visible-bell nil)
  (add-to-list 'default-frame-alist '(fullscreen . maximized))


  (electric-pair-mode t) ;; Matching parentheses
  ;; (global-display-line-numbers-mode t)
  (setq ispell-program-name "aspell")

  ;; Make Flymake run in all programming buffers
  (add-hook 'prog-mode-hook 'flymake-mode)  
  (add-hook 'org-mode-hook 'flyspell-mode)  

  ;; Disbale auto save to stop spamming my machine with auto-save files everywhere 
  (setq auto-save-default nil)
  
  (add-to-list 'auto-mode-alist '("\\.yml$'" . yaml-ts-mode))

  ;; PDF View does not play nice with display-line-numbers-mode
  (dolist (mode '(text-mode-hook
                  prog-mode-hook
                  conf-mode-hook))
    (add-hook mode (lambda () (display-line-numbers-mode 1))))

  ;; (add-hook 'pdf-view-mode-hook 'display-line-numbers-mode)


  ;; Used for getting rid of the top window bar
;; Don't get rid of it for Gnu/Linux 
  (cond 
	((string-equal  system-type  "darwin") (add-to-list 'default-frame-alist '(undecorated . t)) )
	((string-equal system-type  "gnu/linux") ())
  )

  ;; Set font to be bigger in Graphical Emacs on WSL/Linux
  ;; (if (equal system-type  "gnu/linux")
  ;;     (set-face-attribute 'default nil  :height 1000)
  ;;    (set-default-font "Monaco 14")
  ;; )
  
  
  (which-key-mode)


  ;; Org-mode Setup
  (setq org-startup-indented t))
;; END EMACS Config


;; (add-hook 'prog-mode-hook (lambda () (eglot)))  

;; Start custom function definitions
(defun open-emacs-directory ()
  "Open the config directory"
  (interactive)
  (find-file user-emacs-directory))

(use-package quickrun
  :ensure t
  )

(use-package perspective
  :ensure t
  :bind
  ("C-x C-b" . persp-list-buffers)         ; or use a nicer switcher, see below
  :custom
  (persp-mode-prefix-key (kbd "C-c M-p"))  ; pick your own prefix key here
  :init
  (persp-mode))

(setq treesit-language-source-alist
      '((bash "https://github.com/tree-sitter/tree-sitter-bash")
	(cmake "https://github.com/uyha/tree-sitter-cmake")
	(css "https://github.com/tree-sitter/tree-sitter-css")
	(elisp "https://github.com/Wilfred/tree-sitter-elisp")
	(go "https://github.com/tree-sitter/tree-sitter-go")
	(html "https://github.com/tree-sitter/tree-sitter-html")
	(javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
	(json "https://github.com/tree-sitter/tree-sitter-json")
	(make "https://github.com/alemuller/tree-sitter-make")
	(markdown "https://github.com/ikatyang/tree-sitter-markdown")
	(python "https://github.com/tree-sitter/tree-sitter-python")
	(toml "https://github.com/tree-sitter/tree-sitter-toml")
	(tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
	(typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
	(yaml "https://github.com/ikatyang/tree-sitter-yaml")))

(use-package vterm
  :ensure t
  :load-path   "~/.emacs.d/emacs-libvterm")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("4594d6b9753691142f02e67b8eb0fda7d12f6cc9f1299a49b819312d6addad1d" "e8bd9bbf6506afca133125b0be48b1f033b1c8647c628652ab7a2fe065c10ef0" default))
 '(elfeed-feeds '("https://osblog.stephenmarz.com/feed.rss"))
 '(org-babel-load-languages
   '((emacs-lisp . t)
     (awk . t)
     (python . t)
     (js . t)
     (java . t)
     (C . t)
     (sqlite . t)
     (css . t)
     (lua . t)))
 '(package-selected-packages
   '(all-the-icons cape corfu diff-hl doom-modeline doom-themes elfeed embark-consult evil-collection evil-nerd-commenter evil-snipe evil-surround general magit marginalia orderless org-roam pdf-tools quickrun smartparens treemacs-evil vertico yasnippet-snippets)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;; Setting pkgconf variable for pdf-tools install 
(setenv "PKG_CONFIG_PATH" "/opt/homebrew/Cellar/poppler/25.01.0/lib/pkgconfig/:/usr/X11/lib:/pkgconfig/usr/local/Cellar/libffi/3.2.1/lib/pkgconfig:/opt/homebrew/Cellar/glib/2.82.4/lib/pkgconfig/:/opt/homebrew/Cellar/cairo/1.18.2/lib/pkgconfig/:/opt/homebrew/Cellar/libpng/1.6.46/lib/pkgconfig/:/opt/homebrew/Cellar/zlib/1.3.1/lib/pkgconfig/zlib.pc")
(put 'downcase-region 'disabled nil)
