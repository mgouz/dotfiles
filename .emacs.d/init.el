; setq warning-minimum-level :error  ; don't show warning buffer unless error
					; (setq org-directory "~/org/")
(setq org-clock-sound t)

;; My Init file

;;; THIS NEEDS TO BE AT THE TOP
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(add-to-list 'load-path "./modules")

(let ((backup-dir "~/tmp/emacs/backups")
      (auto-saves-dir "~/tmp/emacs/auto-saves/"))
  (dolist (dir (list backup-dir auto-saves-dir))
    (when (not (file-directory-p dir))
      (make-directory dir t)))
  (setq backup-directory-alist `(("." . ,backup-dir))
        auto-save-file-name-transforms `((".*" ,auto-saves-dir t))
        auto-save-list-file-prefix (concat auto-saves-dir ".saves-")
        tramp-backup-directory-alist `((".*" . ,backup-dir))
        tramp-auto-save-directory auto-saves-dir))

(setq backup-by-copying t    ; Don't delink hardlinks
      delete-old-versions t  ; Clean up the backups
      version-control t      ; Use version numbers on backups,
      kept-new-versions 5    ; keep some new versions
      kept-old-versions 2)   ; and some old ones, too


(defun edit-init-file ()
  "Edit the `user-init-file', in other words, this file."
  (interactive)
  (find-file user-init-file))

(defun zc ()
  "Edit the `user-init-file', in other words, this file."
  (interactive)
  (find-file "~/.zshrc"))

(defun restclient ()
  "Edit the `user-init-file', in other words, this file."
  (interactive)
  (find-file "~/.zshrc"))

;; (defvar bootstrap-version)
;; (let ((bootstrap-file
;;        (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
;;       (bootstrap-version 6))
;;   (unless (file-exists-p bootstrap-file)
;;     (with-current-buffer
;;         (url-retrieve-synchronously
;; 	 "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
;; 	 'silent 'inhibit-cookies)
;;       (goto-char (point-max))
;;       (eval-print-last-sexp)))
;;   (load bootstrap-file nil 'nomessage))

;; (straight-use-package 'use-package)  ;; optional but recommended
;; (straight-pull-recipe-repositories)
;; (setq straight-allow-recipe-inheritance nil)


;; bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package)
  (setq use-package-always-ensure t))

(require 'use-package)

(setq lsp-use-plists t)

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
(require 'mg-lsp)
(require 'mg-ai)

(use-package yasnippet
  :ensure t)


(use-package yasnippet-snippets
  :ensure t
  :after yasnippet
  )

(use-package emacs
  :custom
  ;; Support opening new minibuffers from inside existing minibuffers.

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
  (global-set-key (kbd "s-/") 'evilnc-comment-or-uncomment-lines)

  ;; Stop killing wrods when I want to DELETE them

  ;; (global-set-key (kbd "C-w") 'delete-region-no-kill)
  ;; (global-set-key (kbd "M-<backspace>") 'backward-delete-word)
  ;; (global-set-key (kbd "M-DEL") 'backward-delete-word)


  ;; (global-set-key (kbd "M-n") 'forward-paragraph)
  ;; (global-set-key (kbd "M-p") 'backward-paragraph)
  (global-set-key (kbd "C-x P") 'project-switch-project)
  (global-set-key (kbd "M-m") 'compile)
  (global-set-key (kbd "C-x C-b")
		  (lambda ()
		    (interactive)
		    (if (bound-and-true-p persp-mode)
			(ibuffer) (persp-ibuffer)))
		  )
  ;; Misc key maps
  (global-set-key (kbd "s-P") 'execute-extended-command) ; Mimic VSCode's command pallete
  (global-set-key (kbd "S-s-<return>") 'maximize-window) ; C-w = to balance windows again (a-la iterm2)

  ;; TODO swap this out for the appropriate formatting function based on mode of the buffer
  (global-set-key (kbd "M-s-l") 'eglot-format) ; C-w = to balance windows again (a-la iterm2)


  (global-set-key (kbd "C-x .") 'find-file)
  (global-set-key (kbd "C-x C-.") 'find-file-other-window)
  (global-set-key (kbd "C-s-h") 'windmove-left)
  (global-set-key (kbd "C-s-j") 'windmove-down)
  (global-set-key (kbd "C-s-k") 'windmove-up)
  (global-set-key (kbd "C-s-l") 'windmove-right)
  
  (global-set-key (kbd "M-#") 'dictionary-lookup-definition)
  
  
  
  ;; When I press this, I want to follow to a new window
  ;; (global-set-key (kbd "s-<return>") 'ret-new-buffer)
  ;; (global-set-key (kbd "s-<mouse-1>") 'ret-new-buffer)

  ;; Eglot settings for run
  ;; (add-hook 'c- 'eglot-ensure)
  ;; (add-hook 'c-mode-hook 'eglot-ensure)
  ;; (add-hook 'c++-mode-hook 'eglot-ensure)
  ;; (add-hook 'dockerfile-mode-hook 'eglot-ensure)
  ;; (add-hook 'dockerfile-mode-hook 'eglot-ensure)
  ;; (add-hook 'rust-mode 'eglot-ensure)
  ;; (add-hook 'go-mode 'eglot-ensure)
  ;; (add-hook 'python-mode 'eglot-ensure)
  ;; (add-hook 'typescript-mode
  ;;    'eglot-ensure)
  


  ;; UI
  (tool-bar-mode 0) ;; Remove tool bar
  (scroll-bar-mode 0) ; ;remove scroll bar
  (global-set-key (kbd "s-b") 'treemacs) ; Mimic VSCode's file explorer
  (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
  (global-set-key (kbd "C-S-c C-e") 'mc/edit-ends-of-lines)
  (global-set-key (kbd "C-S-c C-a") 'mc/edit-beginnings-of-lines)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
  (repeat-mode)

  (setq inhibit-startup-message t)
  (set-fringe-mode 10)
  (setq visible-bell nil)
  ;; (add-to-list 'default-frame-alist '(fullscreen . maximized))

  
  ;; (global-display-line-numbers-mode t)
  (setq ispell-program-name "aspell")

  ;; Make Flymake run in all programming buffers
  (add-hook 'prog-mode-hook 'flymake-mode)
  (add-hook 'org-mode-hook 'flyspell-mode)
  (add-hook 'mhtml-mode-hook 'emmet-mode)

  (use-package flymake
    :config
    (setq flymake-show-diagnostics-at-end-of-line nil)
    (setq flymake-no-changes-timeout 0.5)
    :bind (:map flymake-mode-map
                ("M-n" . flymake-goto-next-error) ; optional but recommended error navigation
                ("M-p" . flymake-goto-prev-error)))
  

  ;; Disbale auto save to stop spamming my machine with auto-save files everywhere
  (setq auto-save-default nil)
  
  (setq auto-mode-alist (append '(("\\.ts$" . typescript-ts-mode)
				  ("\\.yml$'" . yaml-ts-mode)) auto-mode-alist))

  ;; PDF View does not play nice with display-line-numbers-mode
  (dolist (mode '(text-mode-hook
                  prog-mode-hook
                  conf-mode-hook
		  emacs-lisp-mode))
    (add-hook mode (lambda () (display-line-numbers-mode 1))))

  ;; (add-hook 'pdf-view-mode-hook 'display-line-numbers-mode)


  ;; Used for getting rid of the top window bar
  ;; Don't get rid of it for Gnu/Linux 
  (cond 
   ;; ((string-equal  system-type  "darwin") (add-to-list 'default-frame-alist '(undecorated . t)) )
   ((string-equal system-type  "gnu/linux") ())
   )

  ;; Set font to be bigger in Graphical Emacs on WSL/Linux
  ;; (if (equal system-type  "gnu/linux")
  ;;     (set-face-attribute 'default nil  :height 1000)
  ;;    (set-default-font "Monaco 14")
  ;; )
  
  
  (which-key-mode)
  (editorconfig-mode 1)


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
  ;; ("C-x C-b" . persp-list-buffers)         ; or use a nicer switcher, see below
  :custom
  (persp-mode-prefix-key (kbd "C-c M-p"))  ; pick your own prefix key here
  :init
  (persp-mode))

(setq treesit-language-source-alist
      '((bash "https://github.com/tree-sitter/tree-sitter-bash")
	(cmake "https://github.com/uyha/tree-sitter-cmake")
	(css "https://github.com/tree-sitter/tree-sitter-css")
	(elisp "https://github.com/Wilfred/tree-sitter-elisp")
	(go . "https://github.com/tree-sitter/tree-sitter-go")
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

;; (add-to-list 'eglot-server-programs '((js-mode typescript-mode) . (eglot-deno "deno" "lsp")))
;; (defclass eglot-deno (eglot-lsp-server) ()
;;   :documentation "A custom class for deno lsp.")

;; (cl-defmethod eglot-initialization-options ((server eglot-deno))
;;   "Passes through required deno initialization options"
;;   (list :enable t
;; 	:lint t))

;; (push auto-mode-alist
;;       ("\\.ts" . typescript-ts-mode))

(require 'dired-x)
(require 'json)

(use-package vterm
  :ensure t
  )

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("4594d6b9753691142f02e67b8eb0fda7d12f6cc9f1299a49b819312d6addad1d"
     "e8bd9bbf6506afca133125b0be48b1f033b1c8647c628652ab7a2fe065c10ef0"
     default))
 '(elfeed-feeds '("https://osblog.stephenmarz.com/feed.rss"))
 '(org-babel-load-languages
   '((emacs-lisp . t) (awk . t) (python . t) (js . t) (java . t) (C . t)
     (sqlite . t) (css . t) (lua . t)))
 '(package-selected-packages
   '(aider aidermacs all-the-icons cape catppuccin-theme copilot corfu
	   dap-mode dape diff-hl disaster dockerfile-mode
	   doom-modeline doom-themes ein embark-consult emmet-mode
	   evil-collection evil-nerd-commenter evil-snipe
	   evil-surround forge go-mode gptel leetcode lsp-tailwindcss
	   lsp-ui marginalia mcp meson-mode multiple-cursors nix-mode
	   orderless org-roam perspective projectile quickrun
	   restclient rust-mode smartparens treemacs-evil vertico
	   vterm yasnippet-snippets)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(use-package forge
  :after magit)


;; Setting pkgconf variable for pdf-tools install 
(setenv "PKG_CONFIG_PATH" "/opt/homebrew/Cellar/poppler/25.01.0/lib/pkgconfig/:/usr/X11/lib:/pkgconfig/usr/local/Cellar/libffi/3.2.1/lib/pkgconfig:/opt/homebrew/Cellar/glib/2.82.4/lib/pkgconfig/:/opt/homebrew/Cellar/cairo/1.18.2/lib/pkgconfig/:/opt/homebrew/Cellar/libpng/1.6.46/lib/pkgconfig/:/opt/homebrew/Cellar/zlib/1.3.1/lib/pkgconfig/zlib.pc")
(put 'downcase-region 'disabled nil)




;; (use-package aider
;;   :ensure t
;;   :config
;;   ;; For latest claude sonnet model
;;   (setq aider-args '("--model" "github_copilot/gpt-4.1" "--no-auto-accept-architect")) ;; add --no-auto-commits if you don't want it
;;   (setenv "ANTHROPIC_API_KEY" "")
;;   ;; Or chatgpt model
;;   ;; (setq aider-args '("--model" "o4-mini"))
;;   ;; (setenv "OPENAI_API_KEY" <your-openai-api-key>)
;;   ;; Or use your personal config file
;;   ;; (setq aider-args `("--config" ,(expand-file-name "~/.aider.conf.yml")))
;;   ;; ;;
;;   ;; Optional: Set a key binding for the transient menu
;;   (global-set-key (kbd "C-c a") 'aider-transient-menu) ;; for wider screen
;;   ;; or use aider-transient-menu-2cols / aider-transient-menu-1col, for narrow screen
;;   (aider-magit-setup-transients) ;; add aider magit function to magit menu
;;   ;; auto revert buffer
;;   (global-auto-revert-mode 1)
;;   (auto-revert-mode 1))

(ignore-errors
  (require 'ansi-color)
  (defun my-colorize-compilation-buffer ()
    (when (eq major-mode 'compilation-mode)
      (ansi-color-apply-on-region compilation-filter-start (point-max))))
  (add-hook 'compilation-filter-hook 'my-colorize-compilation-buffer))


(use-package smartparens
  :ensure smartparens  ;; install the package
  :hook (prog-mode text-mode markdown-mode) ;; add `smartparens-mode` to these hooks
  :config
  ;; load default config
  
  (require 'smartparens-config))


;; This is honestly killing me a bit -- 
;; but recursive editing seems pretty powerful so i'll keep it for now
(setq enable-recursive-minibuffers t)

;;
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(setq x-select-enable-clipboard t)

;; Make it so that that you delete a region instead of kill it
(delete-selection-mode)

;; Make it so that you highlight a line 
(hl-line-mode)

;; Go back and forward different layouts using C-c <arrows>
(winner-mode)

;; ;; Try to get Poke working for better binary data mangling 
;; (add-to-list 'load-path
;; 	     (concat user-emacs-directory "/elpa/poke-3.2"))


;; Add window movement (shift + arrow keys)
;; (windmove-default-keybindings)


(setq enable-recursive-minibuffers nil)


(use-package disaster
  :commands (disaster)
  :ensure t
  :init
  ;; If you prefer viewing assembly code in `nasm-mode` instead of `asm-mode`
  ;; (setq disaster-assembly-mode #'nasm-mode)
  ;; :bind (:map (c++-mode-map c-mode-map disaster-mode-map)
  ;;             ("C-c d" . #'disaster))
  )
(use-package forge
  :ensure t
  :after magit)

;; :hook ((html-mode css-mode web-mode tsx-ts-mode typescript-ts-mode js-mode svelte-mode) . lsp))


(use-package lsp-tailwindcss
  ;; :straight '(lsp-tailwindcss :type git :host github :repo "merrickluo/lsp-tailwindcss")
  :init (setq lsp-tailwindcss-add-on-mode t)
  :ensure t
  :after lsp-mode
  :config
  (dolist (tw-major-mode
           '(css-mode
             css-ts-mode
	     web-mode
	     html-mode
             typescript-mode
             typescript-ts-mode
             tsx-ts-mode
             js2-mode
             js-ts-mode
             clojure-mode))
    (add-to-list 'lsp-tailwindcss-major-modes tw-major-mode)))

(use-package typescript-mode
  :mode "\\.ts"
  :hook (typescipt-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2)
  (require 'dap-node)
  (dap-node-setup))

;; (dap-register-debug-template
;;   "Debug Electron"
;;   (list :type "node"
;;         :request "launch"
;;         :program "${workspaceFolder}/main.ts"
;;         :outFiles ["${workspaceFolder}/dist/**/*.js"]
;;         :name "Debug Server"))
(add-hook 'tsx-ts-mode-hook #'lsp-deferred)

(require 'multiple-cursors)

;; (use-package combobulate
;;   :ensure t
;;   :preface
;;   ;; You can customize Combobulate's key prefix here.
;;   ;; Note that you may have to restart Emacs for this to take effect!
;;   (setq combobulate-key-prefix "C-c o")

;;   ;; Optional, but recommended.
;;   ;;
;;   ;; You can manually enable Combobulate with `M-x
;;   ;; combobulate-mode'.
;;   :hook
;;   ((python-ts-mode . combobulate-mode)
;;    (js-ts-mode . combobulate-mode)
;;    (go-mode . go-ts-mode)
;;    (html-ts-mode . combobulate-mode)
;;    (css-ts-mode . combobulate-mode)
;;    (yaml-ts-mode . combobulate-mode)
;;    (typescript-ts-mode . combobulate-mode)
;;    (json-ts-mode . combobulate-mode)
;;    (tsx-ts-mode . combobulate-mode))

  ;; (use-package tsx-mode
  ;;   :straight '(tsx-mode :type git :host github :repo "orzechowskid/tsx-mode.el" :branch "emacs30")) 
(use-package treemacs
  :config
  (setq treemacs-position 'right))

(use-package projectile
  :ensure t
  :init
  (projectile-mode +1)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :config
  (setq projectile-completion-system 'default)
  (setq projectile-enable-caching t)
  (setq projectile-globally-ignored-directories
	(append '(".idea" ".vscode" "node_modules" ".git" ".hg" ".svn" ".tox" "__pycache__")
		projectile-globally-ignored-directories))
  (setq projectile-globally-ignored-files
	(append '("TAGS" "*.pyc" "*.o" "*~" "#*#" ".DS_Store")
		projectile-globally-ignored-files))
  (setq projectile-indexing-method 'alien)
  ;; (setq projectile-project-search-path '("~/Projects/" "~/Work/"))
  )
