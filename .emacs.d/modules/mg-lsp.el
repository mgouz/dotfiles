(use-package lsp-mode
  :diminish "LSP"
  :ensure t
  :hook ((lsp-mode . lsp-diagnostics-mode)
         (lsp-mode . lsp-enable-which-key-integration)
         (tsx-ts-mode . lsp-deferred)
	 (rust-mode . lsp-deferred)
	 (c-mode . lsp-deferred)
	 (c++-mode . lsp-deferred)
	 (python-mode . lsp-deferred)
	 (go-mode . lsp-deferred)
	 (typescript-mode . lsp-deferred)
	 (typescript-ts-mode . lsp-deferred))
  :custom
  (lsp-keymap-prefix "C-c l")           ; Prefix for LSP actions
  (lsp-completion-provider :none)       ; Using Corfu as the provider
  (lsp-diagnostics-provider :flymake)
  ;; (lsp-session-file (locate-user-emacs-file ".lsp-session"))
  (lsp-log-io nil)                      ; IMPORTANT! Use only for debugging! Drastically affects performance
  (lsp-keep-workspace-alive nil)        ; Close LSP server if all project buffers are closed
  (lsp-idle-delay 0.5)                  ; Debounce timer for `after-change-function'
  ;; ;; core
  (lsp-enable-xref t)                   ; Use xref to find references
  (lsp-auto-configure t)                ; Used to decide between current active servers
  (lsp-eldoc-enable-hover t)            ; Display signature information in the echo area
  (lsp-eldoc-render-all nil)
  (lsp-enable-dap-auto-configure t)     ; Debug support
  ;; (lsp-enable-file-watchers nil)
  ;; (lsp-enable-folding nil)              ; I disable folding since I use origami
  (lsp-enable-imenu t)
  ;; (lsp-enable-indentation nil)          ; I use prettier
  ;; (lsp-enable-links nil)                ; No need since we have `browse-url'
  ;; (lsp-enable-on-type-formatting nil)   ; Prettier handles this
  (lsp-enable-suggest-server-download t) ; Useful prompt to download LSP providers
  (lsp-enable-symbol-highlighting t)     ; Shows usages of symbol at point in the current buffer
  (lsp-enable-text-document-color nil)   ; This is Treesitter's job

  ;; (lsp-ui-sideline-show-hover nil)      ; Sideline used only for diagnostics
  ;; (lsp-ui-sideline-diagnostic-max-lines 20) ; 20 lines since typescript errors can be quite big
  ;; ;; completion
  (lsp-completion-enable t)
  ;; (lsp-completion-enable-additional-text-edit t) ; Ex: auto-insert an import for a completion candidate
  (lsp-enable-snippet t)                         ; Important to provide full JSX completion
  (lsp-completion-show-kind t)                   ; Optional
  ;; ;; headerline
  (lsp-headerline-breadcrumb-enable nil)  ; Optional, I don't need it
  (lsp-headerline-breadcrumb-enable-diagnostics nil) ; Don't make them red, too noisy
  ;; (lsp-headerline-breadcrumb-enable-symbol-numbers nil)
  ;; (lsp-headerline-breadcrumb-icons-enable nil)
  ;; ;; modeline
  ;; (lsp-modeline-code-actions-enable nil) ; Modeline should be relatively clean
  ;; ;; (lsp-modeline-diagnostics-enable nil)  ; Already supported through `flycheck'
  ;; (lsp-modeline-workspace-status-enable nil) ; Modeline displays "LSP" when lsp-mode is enabled
  ;; (lsp-signature-doc-lines 1)                ; Don't raise the echo area. It's distracting
  (lsp-ui-doc-use-childframe t)              ; Show docs for symbol at point
  ;; ;; (lsp-eldoc-render-all nil)            ; This would be very useful if it would respect `lsp-signature-doc-lines', currently it's distracting
  ;; ;; lens
  ;; (lsp-lens-enable nil)                 ; Optional, I don't need it
  ;; ;; semantic
  (lsp-semantic-tokens-enable nil)      ; Related to highlighting, and we defer to treesitter
  :config
  (setq lsp-auto-guess-root t) ; allow single scripts


  :init
  (setq lsp-use-plists t)) 

;; (use-package dap-mode)

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
                ;; lsp-ui-doc-show-with-cursor nil      ; Don't show doc when cursor is over symbol - too distracting
                lsp-ui-doc-include-signature t       ; Show signature
                lsp-ui-doc-position 'at-point))
(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown")
  :bind (:map markdown-mode-map
         ("C-c C-e" . markdown-do)))

;; (setq dap-lldb-debug-program '("/path/to/lldb-dap"))

;; (require 'dap-lldb)
;; (dap-register-debug-template
;;   "LLDB::Run"
;;   (list :type "lldb-vscode"
;;         :cwd nil
;;         :request "launch"
;;         :program "${workspaceFolder}/build/main"
;;         :name "LLDB::Run"))

(use-package dap-mode
  :ensure t
  :after lsp-mode
  :config
  (dap-auto-configure-mode)
  (setq dap-auto-configure-features '(sessions locals breakpoints expressions controls tooltip))
  ;; (require 'dap-lldb)
  ;; (require 'dap-gdb-lldb)
  ;; (dap-gdb-lldb-setup)
  (require 'dap-python)
  (require 'dap-go)
  ;; (require 'dap-node)
  (require 'dap-chrome)
  ;; (require 'dap-firefox)
  (require 'dap-cpptools)
  (dap-cpptools-setup)
  ;; (require 'dap-java)
  ;; (require 'dap-netcore)
  (require 'dap-dlv-go)
  )
(with-eval-after-load 'dap-mode
  (dap-ui-mode 1)
  (dap-tooltip-mode 1)
  (tooltip-mode 1)
  (dap-ui-controls-mode 1)
  ;; (dap-ui-breakpoints)	
  (dap-register-debug-template "Go :: Launch File"
	(list :type "go"
		  :request "launch"
		  :name "Launch File"
		  :mode "auto"
		  :program "${file}"
		  :buildFlags ""
		  :args nil
		  :env nil
		  :envFile nil))
(setq dap-ui-marker-face '((t (:inherit error :inverse-video t))))

;;   (require 'dap-gdb)
;;   (require 'dap-lldb)

;; (dap-register-debug-template
;;   "C++ :: Debug Emacs"
;;   (list :name "C++ :: Debug Emacs"
;;         :type "gdb"
;;         :request "launch"
;;         :args ""
;;         :program "./~/projects/opengl/build/learnopengl"
;;         :cwd ""
;;         :stopAtEntry t))
  (define-key dap-mode-map (kbd "<f5>") 'dap-debug)
  (define-key dap-mode-map (kbd "<f6>") 'dap-continue)
  (define-key dap-mode-map (kbd "<f7>") 'dap-next)
  (define-key dap-mode-map (kbd "<f8>") 'dap-step-in)
  (define-key dap-mode-map (kbd "<f9>") 'dap-step-out)
  (define-key dap-mode-map (kbd "<f10>") 'dap-breakpoint-toggle)
  )

(with-eval-after-load 'lsp-mode
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
  (require 'dap-cpptools)
  (yas-global-mode)
  (dap-register-debug-template
  "cpptools::Run Configuration"
  (list :type "cppdbg"
        :request "launch"
        :name "cpptools::Run Configuration"
        :MIMode "lldb"
        :program "${workspaceFolder}/build/main"
        :cwd "${workspaceFolder}")))


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
  (add-to-list 'lsp-language-id-configuration '(jsx-ts-mode . "typescriptreact"))

  ;; ------------------ Register additional lsp clients -------------------- 
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection '("tailwindcss-language-server" "--stdio"))
    :activation-fn (lsp-activate-on "typescriptreact")
    :server-id 'tailwindcss
    :add-on? t
    :priority -1))
  ;; GLSL Analyzer LSP: Doesn't seem to have diagnostics, but at least it works
  ;; You do have to build from source but it's zig and not C++ so the build is easy
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection '("glsl_analyzer" "--stdio"))
    :activation-fn (lsp-activate-on "glsl")
    :server-id 'glsl-analyzer
    :add-on? t
    :priority -1))

  ;; For whatever reason, this doesn't work properly when I try to configure it for opengl
  ;; it also doesn't recognize glsl file extensions so I have to set the file as .frag or .vert (I believe)
  ;; (lsp-register-client
  ;;  (make-lsp-client
  ;;   :new-connection (lsp-stdio-connection '("glslls" "--target-env opengl --stdin"))
  ;;   :activation-fn (lsp-activate-on "glsl")
  ;;   :server-id 'glsl-language-server
  ;;   :add-on? t
  ;;   :priority -1))
  )


(provide 'mg-lsp)
