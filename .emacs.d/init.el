;; -*- lexical-binding: t; -*-
;; setq warning-minimum-level :error  ; don't show warning buffer unless error
					; (setq org-directory "~/org/")
;; (setq org-clock-sound t)

;; My Init file


;;; THIS NEEDS TO BE AT THE TOP
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(add-to-list 'load-path "./modules")

(setq vc-follow-symlinks t) ; Follow symlinks automatically

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
  "Edit my `zshrc'"
  (interactive)
  (find-file "~/.zshrc"))

(defun e-restclient ()
  "Edit my `restclient' config"
  (interactive)
  (find-file "~/.emacs.d/rest/restclient.http"))

;; bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package)
  (setq use-package-always-ensure t))

(require 'use-package)

(setq lsp-use-plists t)

(add-to-list 'load-path (concat user-emacs-directory "/modules"))
(require 'mg-ui)
(require 'mg-evil)
(require 'mg-wip)
(require 'mg-git)
(require 'mg-completion-minibuffer)
(require 'mg-completion-point)
(require 'mg-org)
;; (require 'mg-debugging)
(require 'mg-lsp)
(require 'mg-ai)
(require 'mg-wm)

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

  (global-origami-mode)
  (hl-line-mode 1)
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
  (hl-line-mode 1)

  (setq inhibit-startup-message t)
  (set-fringe-mode 10)
  (setq visible-bell nil)
  ;; (add-to-list 'default-frame-alist '(fullscreen . maximized))

  
  ;; (global-display-line-numbers-mode t)
  (setq ispell-program-name "aspell")

  ;; Make Flymake run in all programming buffers
  (add-hook 'org-mode-hook 'flyspell-mode)
  (add-hook 'mhtml-mode-hook 'emmet-mode)
  (add-hook 'prog-mode-hook 'flymake-mode)

;; (use-package flycheck
;;   :ensure t
;;   :bind (:map flycheck-mode-map
;; 			  ("M-n" . flycheck-next-error) ; optional but recommended error navigation
;; 			  ("M-p" . flycheck-previous-error))
;;   :init (global-flycheck-mode))


;; (with-eval-after-load 'flycheck
;;   (use-package consult-flycheck
;; 	:ensure t
;;   :bind (:map flycheck-mode-map
;; 			  ("gh" . consult-flycheck))))


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



;; (use-package tree-sitter
;;   :config
;;   (require 'tree-sitter-langs)
  ;; (global-tree-sitter-mode)
  ;; (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package treesit
  :mode (
	 ("\\.tsx\\'" . tsx-ts-mode)
  ;; 	 ("\\.ts\\'" . typescript-ts-mode-hook)
  ;; 	 ("\\.js\\'" . js-ts-mode)
  ;; 	 ("\\.html\\'" . html-ts-mode)
  ;; 	 ("\\.css\\'" . css-ts-mode)
  ;; 	 ("\\.json\\'" . json-ts-mode)
  ;; 	 ("\\.py\\'" . python-ts-mode)
  ;; 	 ("\\.go\\'" . go-ts-mode)
  ;; 	 ("\\.rs\\'" . rust-ts-mode)
  ;; 	 ("\\.toml\\'" . toml-ts-mode)
  ;; 	 ("\\.yaml\\'" . yaml-ts-mode)
  ;; 	 ("\\.md\\'" . markdown-ts-mode)
	 ("\\.cpp\\'" . c++-ts-mode)
	 ("\\.cc\\'" . c++-ts-mode)
  ;; 	 ("\\.hpp\\'" . c++-ts-mode)
  ;; 	 ("\\.hh\\'" . c++-ts-mode)
  ;; 	 ("\\.c\\'" . c-ts-mode)
	 ("\\.h\\'" . c++-ts-mode)
	 )
	 
	 
  :preface
  (defun mp-setup-install-grammars ()
    "Install Tree-sitter grammars if they are absent."
    (interactive)
    (dolist (grammar
             ;; Note the version numbers. These are the versions that
             ;; are known to work with Combobulate *and* Emacs.
             '((css . ("https://github.com/tree-sitter/tree-sitter-css" "v0.20.0"))
               (go . ("https://github.com/tree-sitter/tree-sitter-go" "v0.20.0"))
               (html . ("https://github.com/tree-sitter/tree-sitter-html" "v0.20.1"))
               (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "v0.20.1" "src"))
               (json . ("https://github.com/tree-sitter/tree-sitter-json" "v0.20.2"))
               (markdown . ("https://github.com/ikatyang/tree-sitter-markdown" "v0.7.1"))
               (python . ("https://github.com/tree-sitter/tree-sitter-python" "v0.20.4"))
               (rust . ("https://github.com/tree-sitter/tree-sitter-rust" "v0.21.2"))
               (toml . ("https://github.com/tree-sitter/tree-sitter-toml" "v0.5.1"))
               (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "tsx/src"))
               (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "typescript/src"))
               (yaml . ("https://github.com/ikatyang/tree-sitter-yaml" "v0.5.0"))
	       (make "https://github.com/alemuller/tree-sitter-make")
	       (markdown . ("https://github.com/ikatyang/tree-sitter-markdown"))
	       (cpp . ("https://github.com/tree-sitter/tree-sitter-cpp"))
	       (c . ("https://github.com/tree-sitter/tree-sitter-c"))
               (toml . ("https://github.com/tree-sitter/tree-sitter-toml"))
	       (bash . ("https://github.com/tree-sitter/tree-sitter-bash"))
	       (cmake . ("https://github.com/uyha/tree-sitter-cmake"))))
      (add-to-list 'treesit-language-source-alist grammar)
      ;; Only install `grammar' if we don't already have it
      ;; installed. However, if you want to *update* a grammar then
      ;; this obviously prevents that from happening.
      (unless (treesit-language-available-p (car grammar))
        (treesit-install-language-grammar (car grammar)))))

  ;; Optional. Combobulate works in both xxxx-ts-modes and
  ;; non-ts-modes.

  ;; You can remap major modes with `major-mode-remap-alist'. Note
  ;; that this does *not* extend to hooks! Make sure you migrate them
  ;; also
  ;; (dolist (mapping
  ;;          '((python-mode . python-ts-mode)
  ;;            (css-mode . css-ts-mode)
  ;;            (typescript-mode . typescript-ts-mode)
  ;;            (js2-mode . js-ts-mode)
  ;;            (bash-mode . bash-ts-mode)
  ;;            (conf-toml-mode . toml-ts-mode)
  ;;            (go-mode . go-ts-mode)
  ;;            (css-mode . css-ts-mode)
  ;;            (json-mode . json-ts-mode)
  ;;            (js-json-mode . json-ts-mode)))
  ;;   (add-to-list 'major-mode-remap-alist mapping))
  :config
  (mp-setup-install-grammars))
  ;; Do not forget to customize Combobulate to your liking:
  ;;
  ;;  M-x customize-group RET combobulate RET
  ;;
  ;; (use-package combobulate
  ;;   :custom
  ;;   ;; You can customize Combobulate's key prefix here.
  ;;   ;; Note that you may have to restart Emacs for this to take effect!
  ;;   (combobulate-key-prefix "C-c o")
  ;;   :hook ((prog-mode . combobulate-mode))
  ;;   ;; Amend this to the directory where you keep Combobulate's source
  ;;   ;; code.
  ;;   :load-path ("path-to-git-checkout-of-combobulate")))


;; (add-to-list 'eglot-server-programs '((js-mode typescript-mode) . (eglot-deno "deno" "lsp")))
;; (defclass eglot-deno (eglot-lsp-server) ()
;;   :documentation "A custom class for deno lsp.")

;; (cl-defmethod eglot-initialization-options ((server eglot-deno))
;;   "Passes through required deno initialization options"
;;   (list :enable t
;; 	:lint t))

;; (push auto-mode-alist
;;       ("\\.ts" . typescript-ts-mode))

;; (require 'dired-x)
(require 'json)

(use-package vterm
  :ensure t
  :hook (vterm-mode . (lambda ()
			(setq-local global-hl-line-mode nil)))
  (add-hook 'vterm-mode-hook evil-emacs-state))

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
   '(aider aidermacs all-the-icons cape catppuccin-theme claude-code-ide
	   copilot corfu dap-mode dape diff-hl disaster docker
	   dockerfile-mode doom-modeline doom-themes
	   dtrace-script-mode ein embark-consult emmet-mode
	   evil-collection evil-nerd-commenter evil-snipe
	   evil-surround flycheck-dtrace forge glsl-mode go-mode gptel
	   leetcode lsp-tailwindcss lsp-ui lua-mode magit-todos
	   marginalia mcp meson-mode multiple-cursors ninja-mode
	   nix-mode orderless org-roam origami perspective quickrun
	   restclient rust-mode smartparens treemacs-evil
	   treemacs-projectile vertico vertico-posframe vterm
	   yasnippet-snippets))
 '(package-vc-selected-packages
   '((claude-code-ide :url
		      "https://github.com/manzaltu/claude-code-ide.el")))
 '(safe-local-variable-values
   '((projectile-project-compilation-cmd
      . "cmake --build build && ./build/chapterX")
     (projectile-run-project . "./build/chapterX"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(use-package forge
  :after magit)


(use-package hl-todo
    :ensure t
    :config
    (global-hl-todo-mode t))

(setq hl-todo-keyword-faces
        '(("TODO" . "green")
          ("FIXME" . "pink")
          ("BUG" . "red")
	  ("NOTE" . "yellow")
	  ("HACK" . "orange")
	  ("REVIEW" . "blue")
	  ("DEPRECATED" . "purple")))

(use-package magit-todos
  :ensure t
  :after magit
  :config (magit-todos-mode 1))



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
;; this is murdering me
(setq enable-recursive-minibuffers nil)

;;
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(delete-selection-mode 1)

;; Allow emacs to use the system clipboard
;; (setq select-enable-clipboard t)
(setq x-select-enable-clipboard t)

;; Save clipboard strings into kill ring before replacing them.
(setq save-interprogram-paste-before-kill t)

;; (setq mouse-yank-at-point t)
(setq apropos-do-all t)

;; Stop saving text that is kill into the kill ring
;; I only want stuff I purposely copy to be saved
;; (setq kill-transform-function (lambda (string) nil))


;; --------------------------------

;; Make it so that you highlight a line 
(hl-line-mode t)

;; Go back and forward different layouts using C-c <arrows>
(winner-mode t)
;; This is just winner-mode but for tabs
(tab-bar-history-mode t) 

(smartparens-global-mode t)

(yas-global-mode t)




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

(use-package docker
  :ensure t)

(use-package dockerfile-mode
  :ensure t
  :mode ("Dockerfile\\'" . dockerfile-mode))

(use-package glsl-mode
  :ensure t
  :mode ("\\.glsl\\'" . glsl-mode))

(use-package ninja-mode
  :ensure t
  :mode ("\\.ninja\\'" . ninja-mode))

(use-package meson-mode
  :ensure t
  :mode ("meson\\.build\\'" . meson-mode))

(use-package cmake-mode
  :ensure t
  :mode ("CMakeLists\\.txt\\'" . cmake-mode)
  :mode ("\\.cmake\\'" . cmake-mode))

(use-package dtrace-script-mode
  :ensure t
  :mode ("\\.d\\'" . dtrace-script-mode))

(use-package rust-mode
  :init
  (setq rust-mode-treesitter-derive nil))


(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns x))
	(exec-path-from-shell-initialize)))

;; (use-package clang-format
;;   :commands (clang-format-buffer clang-format-on-save-mode))

;; (add-hook 'c-mode-hook 'clang-format-on-save-mode)
;; (add-hook 'c++-mode-hook 'clang-format-on-save-mode)
;; (add-hook 'glsl-mode-hook 'clang-format-on-save-mode)

(add-hook
  'rst-mode-hook
  (lambda ()
    (setq-local fill-column 120)
    (setq-local indent-tabs-mode nil)
    (setq-local tab-width 3)
    (setq-local evil-shift-width 3)

    (add-to-list 'write-file-functions 'delete-trailing-whitespace)

    ;; package: find-file-in-project
    (setq-local ffip-patterns '("*.rst" "*.py"))))
(setq  warning-minimum-level :error)  ; don't show warning buffer unless error
(setq  display-warning-minimum-level :error)  ; don't show warning buffer unless error
(setq switch-to-buffer-obey-display-actions t) ;; Emacs should obey display actions when switching buffers

;; stop the warning buffer from stealing focus
(setq warnings-buffer-display-style nil)

