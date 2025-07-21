;; setq warning-minimum-level :error  ; don't show warning buffer unless error
					; (setq org-directory "~/org/")
(setq org-clock-sound t)

;; My Init file

;;; THIS NEEDS TO BE AT THE TOP
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(add-to-list 'load-path "./modules")

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
  (global-set-key (kbd "C-x C-b") 'ibuffer)
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
  :load-path   "~/.emacs.d/emacs-libvterm")

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
     (sqlite . t) (css . t) (lua . t) (go . t) (rust . t)))
 '(package-selected-packages
   '(all-the-icons calfw cape catppuccin-theme corfu dap-mode dape
		   diff-hl dockerfile-mode doom-modeline doom-themes
		   elfeed ellama embark-consult ement emmet-mode
		   evil-collection evil-nerd-commenter evil-snipe
		   evil-surround forge go-mode gptel lsp-tailwindcss
		   lsp-ui marginalia multiple-cursors nix-mode ob-go
		   orderless org-roam pdf-tools perspective poke
		   poke-mode quickrun restclient rust-mode
		   simple-httpd smartparens treemacs-evil
		   treemacs-projectile tuareg typescript-mode vertico
		   vterm web-mode yasnippet-snippets zig-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;; Setting pkgconf variable for pdf-tools install 
(setenv "PKG_CONFIG_PATH" "/opt/homebrew/Cellar/poppler/25.01.0/lib/pkgconfig/:/usr/X11/lib:/pkgconfig/usr/local/Cellar/libffi/3.2.1/lib/pkgconfig:/opt/homebrew/Cellar/glib/2.82.4/lib/pkgconfig/:/opt/homebrew/Cellar/cairo/1.18.2/lib/pkgconfig/:/opt/homebrew/Cellar/libpng/1.6.46/lib/pkgconfig/:/opt/homebrew/Cellar/zlib/1.3.1/lib/pkgconfig/zlib.pc")
(put 'downcase-region 'disabled nil)



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
(require 'poke)

;; Add window movement (shift + arrow keys)
;; (windmove-default-keybindings)


(setq enable-recursive-minibuffers nil)
(require 'multiple-cursors)

(use-package ellama
  :ensure t
  :bind ("C-c e" . ellama)
  ;; send last message in chat buffer with C-c C-c
  :hook (org-ctrl-c-ctrl-c-final . ellama-chat-send-last-message)
  :init
  ;; setup key bindings
  ;; (setopt ellama-keymap-prefix "C-c e")
  ;; language you want ellama to translate to
  (setopt ellama-language "German")
  ;; could be llm-openai for example
  (require 'llm-ollama)
  (setopt ellama-provider
  	  (make-llm-ollama
  	   ;; this model should be pulled to use it
  	   ;; value should be the same as you print in terminal during pull
  	   :chat-model "llama3:8b-instruct-q8_0"
  	   :embedding-model "nomic-embed-text"
  	   :default-chat-non-standard-params '(("num_ctx" . 8192))))
  (setopt ellama-summarization-provider
  	  (make-llm-ollama
  	   :chat-model "qwen2.5:3b"
  	   :embedding-model "nomic-embed-text"
  	   :default-chat-non-standard-params '(("num_ctx" . 32768))))
  (setopt ellama-coding-provider
  	  (make-llm-ollama
  	   :chat-model "qwen2.5-coder:3b"
  	   :embedding-model "nomic-embed-text"
  	   :default-chat-non-standard-params '(("num_ctx" . 32768))))
  ;; Predefined llm providers for interactive switching.
  ;; You shouldn't add ollama providers here - it can be selected interactively
  ;; without it. It is just example.
  (setopt ellama-providers
  	  '(("zephyr" . (make-llm-ollama
  			 :chat-model "zephyr:7b-beta-q6_K"
  			 :embedding-model "zephyr:7b-beta-q6_K"))
  	    ("mistral" . (make-llm-ollama
  			  :chat-model "mistral:7b-instruct-v0.2-q6_K"
  			  :embedding-model "mistral:7b-instruct-v0.2-q6_K"))
  	    ("mixtral" . (make-llm-ollama
  			  :chat-model "mixtral:8x7b-instruct-v0.1-q3_K_M-4k"
  			  :embedding-model "mixtral:8x7b-instruct-v0.1-q3_K_M-4k"))))
  ;; Naming new sessions with llm
  (setopt ellama-naming-provider
  	  (make-llm-ollama
  	   :chat-model "llama3:8b-instruct-q8_0"
  	   :embedding-model "nomic-embed-text"
  	   :default-chat-non-standard-params '(("stop" . ("\n")))))
  (setopt ellama-naming-scheme 'ellama-generate-name-by-llm)
  ;; Translation llm provider
  (setopt ellama-translation-provider
  	  (make-llm-ollama
  	   :chat-model "qwen2.5:3b"
  	   :embedding-model "nomic-embed-text"
  	   :default-chat-non-standard-params
  	   '(("num_ctx" . 32768))))
  (setopt ellama-extraction-provider (make-llm-ollama
  				      :chat-model "qwen2.5-coder:7b-instruct-q8_0"
  				      :embedding-model "nomic-embed-text"
  				      :default-chat-non-standard-params
  				      '(("num_ctx" . 32768))))
  ;; customize display buffer behaviour
  ;; see ~(info "(elisp) Buffer Display Action Functions")~
  (setopt ellama-chat-display-action-function #'display-buffer-full-frame)
  (setopt ellama-instant-display-action-function #'display-buffer-at-bottom)
  :config
  ;; show ellama context in header line in all buffers
  (ellama-context-header-line-global-mode +1)
  ;; show ellama session id in header line in all buffers
  (ellama-session-header-line-global-mode +1)
  ;; handle scrolling events
  (advice-add 'pixel-scroll-precision :before #'ellama-disable-scroll)
  (advice-add 'end-of-buffer :after #'ellama-enable-scroll))

(use-package lsp-mode
  :diminish "LSP"
  :ensure t
  :hook ((lsp-mode . lsp-diagnostics-mode)
         (lsp-mode . lsp-enable-which-key-integration)
         ((tsx-ts-mode
           typescript-ts-mode
           js-ts-mode) . lsp-deferred))
  :custom
  (lsp-keymap-prefix "C-c l")           ; Prefix for LSP actions
  (lsp-completion-provider :none)       ; Using Corfu as the provider
  (lsp-diagnostics-provider :flymake)
  (lsp-session-file (locate-user-emacs-file ".lsp-session"))
  (lsp-log-io nil)                      ; IMPORTANT! Use only for debugging! Drastically affects performance
  (lsp-keep-workspace-alive nil)        ; Close LSP server if all project buffers are closed
  (lsp-idle-delay 0.5)                  ; Debounce timer for `after-change-function'
  ;; core
  (lsp-enable-xref t)                   ; Use xref to find references
  (lsp-auto-configure t)                ; Used to decide between current active servers
  (lsp-eldoc-enable-hover t)            ; Display signature information in the echo area
  (lsp-enable-dap-auto-configure t)     ; Debug support
  (lsp-enable-file-watchers nil)
  (lsp-enable-folding nil)              ; I disable folding since I use origami
  (lsp-enable-imenu t)
  (lsp-enable-indentation nil)          ; I use prettier
  (lsp-enable-links nil)                ; No need since we have `browse-url'
  (lsp-enable-on-type-formatting nil)   ; Prettier handles this
  (lsp-enable-suggest-server-download t) ; Useful prompt to download LSP providers
  (lsp-enable-symbol-highlighting t)     ; Shows usages of symbol at point in the current buffer
  (lsp-enable-text-document-color nil)   ; This is Treesitter's job

  (lsp-ui-sideline-show-hover nil)      ; Sideline used only for diagnostics
  (lsp-ui-sideline-diagnostic-max-lines 20) ; 20 lines since typescript errors can be quite big
  ;; completion
  (lsp-completion-enable t)
  (lsp-completion-enable-additional-text-edit t) ; Ex: auto-insert an import for a completion candidate
  (lsp-enable-snippet t)                         ; Important to provide full JSX completion
  (lsp-completion-show-kind t)                   ; Optional
  ;; headerline
  (lsp-headerline-breadcrumb-enable t)  ; Optional, I like the breadcrumbs
  (lsp-headerline-breadcrumb-enable-diagnostics nil) ; Don't make them red, too noisy
  (lsp-headerline-breadcrumb-enable-symbol-numbers nil)
  (lsp-headerline-breadcrumb-icons-enable nil)
  ;; modeline
  (lsp-modeline-code-actions-enable nil) ; Modeline should be relatively clean
  (lsp-modeline-diagnostics-enable nil)  ; Already supported through `flycheck'
  (lsp-modeline-workspace-status-enable nil) ; Modeline displays "LSP" when lsp-mode is enabled
  (lsp-signature-doc-lines 1)                ; Don't raise the echo area. It's distracting
  (lsp-ui-doc-use-childframe t)              ; Show docs for symbol at point
  (lsp-eldoc-render-all nil)            ; This would be very useful if it would respect `lsp-signature-doc-lines', currently it's distracting
  ;; lens
  (lsp-lens-enable nil)                 ; Optional, I don't need it
  ;; semantic
  (lsp-semantic-tokens-enable nil)      ; Related to highlighting, and we defer to treesitter

  :init
  (setq lsp-use-plists t))

(use-package lsp-completion
  :no-require
  :hook ((lsp-mode . lsp-completion-mode)))

(use-package lsp-ui
  :ensure t
  :commands
  (lsp-ui-doc-show
   lsp-ui-doc-glance)
  :bind (:map lsp-mode-map
              ("C-c C-d" . 'lsp-ui-doc-glance))
  :after (lsp-mode evil)
  :config (setq lsp-ui-doc-enable t
                evil-lookup-func #'lsp-ui-doc-glance ; Makes K in evil-mode toggle the doc for symbol at point
                lsp-ui-doc-show-with-cursor nil      ; Don't show doc when cursor is over symbol - too distracting
                lsp-ui-doc-include-signature t       ; Show signature
                lsp-ui-doc-position 'at-point))


;; :hook ((html-mode css-mode web-mode tsx-ts-mode typescript-ts-mode js-mode svelte-mode) . lsp))


(use-package lsp-tailwindcss
  :straight '(lsp-tailwindcss :type git :host github :repo "merrickluo/lsp-tailwindcss")
  :init (setq lsp-tailwindcss-add-on-mode t)
  :config
  (dolist (tw-major-mode
           '(css-mode
             css-ts-mode
             typescript-mode
             typescript-ts-mode
             tsx-ts-mode
             js2-mode
             js-ts-mode
             clojure-mode))
    (add-to-list 'lsp-tailwindcss-major-modes tw-major-mode)))

(use-package lsp-ui
  :ensure t
  :commands
  (lsp-ui-doc-show
   lsp-ui-doc-glance)
  :bind (:map lsp-mode-map
              ("C-c C-d" . 'lsp-ui-doc-glance))
  :after (lsp-mode evil)
  :config (setq lsp-ui-doc-enable t
                evil-lookup-func #'lsp-ui-doc-glance ; Makes K in evil-mode toggle the doc for symbol at point
                lsp-ui-doc-show-with-cursor nil      ; Don't show doc when cursor is over symbol - too distracting
                lsp-ui-doc-include-signature t       ; Show signature
                lsp-ui-doc-position 'at-point))

(use-package lsp-eslint
  :demand t
  :after lsp-mode)


(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-language-id-configuration '(tsx-ts-mode . "typescriptreact"))

  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection '("tailwindcss-language-server" "--stdio"))
    :activation-fn (lsp-activate-on "typescriptreact")
    :server-id 'tailwindcss
    :add-on? t
    :priority -1)))
(add-hook 'tsx-ts-mode-hook #'lsp-deferred)



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
