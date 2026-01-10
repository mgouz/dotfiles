;;; -*- lexical-binding: t -*-


; (keymap-unset 'Info-mode (kbd "SPC"))
; (keymap-unset 'dired-mode (kbd "SPC"))
; (evil-define-keymap)

; Download Evil
;; (defun my-org-insert-item
;;     "If we're on a heading, then insert a heading and go into insert mode"
;;     (org-insert-item) 
;;   )

(use-package evil
  :ensure t ;; install the evil package if not installed
  :config 
  (global-evil-mc-mode 1)
  ;; Don't set emacs mode because it can affect
  (evil-set-leader '(normal motion) (kbd "SPC")) ;; Has issues in Info mode, finder, ibuffer, and dired
  (evil-define-key 'normal  'global  (kbd "<leader>.")  'find-file)
  (evil-define-key 'normal  'global (kbd "<leader>;")  'eval-expression)
  (evil-define-key 'normal  'global (kbd "<leader>:")  'execute-extended-command)
  (evil-define-key 'normal  'global (kbd "<leader>/")  'consult-line) ;; was evil-search-forward
  (evil-define-key 'normal  'global (kbd "C-z")  'suspend-frame)
  (evil-define-key 'normal  'global (kbd "C-x C-z")  'evil-emacs-state)
  (evil-define-key 'normal  'global (kbd "C-w g C-]")  (lambda ()
							 "Split window and go to definition"
							 (interactive)
							 (split-window-right)
							 (windmove-right)
							 (evil-goto-definition)))
  (evil-define-key 'normal 'global (kbd "s-z")  'evil-undo)
  (evil-define-key 'normal 'global (kbd "s-Z")  'evil-redo)
  (evil-define-key 'insert  'global (kbd "C-d")  'delete-char) ;; was evil-search-forward
  ;; evil-multiedit
(evil-define-key 'normal 'global
  (kbd "s-d")   #'evil-multiedit-match-symbol-and-next
  (kbd "s-D")   #'evil-multiedit-match-symbol-and-prev)
(evil-define-key 'visual 'global
  "R"           #'evil-multiedit-match-all
  (kbd "s-d")   #'evil-multiedit-match-and-next
  (kbd "s-D")   #'evil-multiedit-match-and-prev)
(evil-define-key '(visual normal) 'global
  (kbd "C-M-d") #'evil-multiedit-restore)

(with-eval-after-load 'evil-mutliedit
  (evil-define-key 'multiedit 'global
    (kbd "s-d")   #'evil-multiedit-match-and-next
    (kbd "s-D") #'evil-multiedit-match-and-prev
    (kbd "RET")   #'evil-multiedit-toggle-or-restrict-region)
  (evil-define-key '(multiedit multiedit-insert) 'global
    (kbd "C-n")   #'evil-multiedit-next
    (kbd "C-p")   #'evil-multiedit-prev))

  (evil-define-key '(normal visual) 'global
  "gzm" #'evil-mc-make-all-cursors
  "gzu" #'evil-mc-undo-all-cursors
  "gzz" #'+evil/mc-toggle-cursors
  "gzc" #'+evil/mc-make-cursor-here
  "gzn" #'evil-mc-make-and-goto-next-cursor
  "gzp" #'evil-mc-make-and-goto-prev-cursor
  "gzN" #'evil-mc-make-and-goto-last-cursor
  "gzP" #'evil-mc-make-and-goto-first-cursor)

  (with-eval-after-load 'evil-mc
  (evil-define-key '(normal visual) evil-mc-key-map
    (kbd "C-n") #'evil-mc-make-and-goto-next-cursor
    (kbd "C-N") #'evil-mc-make-and-goto-last-cursor
    (kbd "C-p") #'evil-mc-make-and-goto-prev-cursor
    (kbd "C-P") #'evil-mc-make-and-goto-first-cursor))

  ;; Folding 
  ;; (evil-define-key 'normal 'global (kbd "zM")  'origami-close-all-nodes)
  ;; (evil-define-key 'normal 'global (kbd "zR")  'origami-open-all-nodes)
  ;; (evil-define-key 'normal 'global (kbd "za")  'origami-toggle-node)
  ;; ;; (evil-define-key 'normal 'global (kbd "<TAB>")  'origami-toggle-node)
  ;; (evil-define-key 'normal 'global (kbd "zc")  'origami-close-node)
  ;; (evil-define-key 'normal 'global (kbd "zo")  'origami-open-node)


  ;; Buffer setting
  (evil-define-key 'normal 'global (kbd "<leader>bi")  'ibuffer)
  (evil-define-key 'normal 'global (kbd "<leader>bk")  'kill-buffer)
  (evil-define-key 'normal 'global (kbd "<leader>k")  'kill-buffer)
  (evil-define-key 'normal 'global (kbd "<leader>B")  'switch-to-buffer)
  (evil-define-key 'normal 'global (kbd "<leader>bxu")  'rename-uniquely)
  (evil-define-key 'normal 'global (kbd "<leader>bxr")  'rename-buffer)
  (evil-define-key 'normal 'global (kbd "<leader>s")  'scratch-buffer)
  (evil-define-key 'normal 'global (kbd "<leader>bb")  'switch-to-buffer)
  (evil-define-key 'normal 'global (kbd "<leader>d")  'dired-jump)

  ;; Project settings
  (evil-define-key 'normal 'global (kbd "<leader>pi")  'projectile-ibuffer)
  (evil-define-key 'normal 'global (kbd "<leader>pb")  'consult-project-buffer)
  (evil-define-key 'normal 'global (kbd "<leader>pB")  'projectile-switch-to-buffer-other-window)
  (evil-define-key 'normal 'global (kbd "<leader>pf")  'projectile-find-file)
  (evil-define-key 'normal 'global (kbd "<leader>pF")  'projectile-find-file-other-window)
  (evil-define-key 'normal 'global (kbd "s-p")  'projectile-find-file)
  (evil-define-key 'normal 'global (kbd "<leader>pp")  'projectile-switch-project)
  (evil-define-key 'normal 'global (kbd "<leader>pk")  'projectile-kill-buffers)
  (evil-define-key 'normal 'global (kbd "<leader>pt")  'projectile-test-project)
  (evil-define-key 'normal 'global (kbd "<leader>pI")  'projectile-ibuffer)
  (evil-define-key 'normal 'global (kbd "<leader>pc")  'projectile-compile-project)
  (evil-define-key 'normal 'global (kbd "<leader>pX")  'projectile-run-vterm-other-window)
  (evil-define-key 'normal 'global (kbd "s-j")  '(lambda ()
						   (interactive)
						   (let ((root-window (frame-root-window)))
						     ;; Split the root window
						     (select-window root-window)
						     (split-window-below)
						     ;; Move to the window below
						     (other-window 1))))
  (evil-define-key 'normal 'global (kbd "<leader>pd")  'project-find-dir)
  (evil-define-key 'normal 'global (kbd "<leader>pD")  'projectile-dired)
  (evil-define-key 'normal 'global (kbd "<leader>pr")  'project-query-replace-regexp)
  (evil-define-key 'normal 'global (kbd "<leader>pg")  'consult-ripgrep)
  (evil-define-key 'normal 'global (kbd "<leader>pv")  'magit)
  (evil-define-key 'normal 'global (kbd "<leader>p!")  'projectile-run-shell-command-in-root)
  (evil-define-key 'normal 'global (kbd "<leader>p&")  'projectile-run-async-shell-command-in-root)

  ;; Porjectile LifeCycle commands (use projectile-comint-mode to make compilation buffers editable)
  (evil-define-key 'normal 'global (kbd "<leader>pxp")  'projectile-switch-project)
  (evil-define-key 'normal 'global (kbd "<leader>pxk")  'projectile-kill-buffers)
  (evil-define-key 'normal 'global (kbd "<leader>pxt")  'projectile-test-project)
  (evil-define-key 'normal 'global (kbd "<leader>pxi")  'projectile-install-project)
  (evil-define-key 'normal 'global (kbd "<leader>pxc")  'projectile-compile-project)
  (evil-define-key 'normal 'global (kbd "<leader>pxr")  'projectile-run-project)
  (evil-define-key 'normal 'global (kbd "<leader>pxo")  'projectile-configure-project)
  (evil-define-key 'normal 'global (kbd "<leader>po")  'projectile-configure-project)
  ;; (evil-define-key 'normal 'global (kbd "<leader>pe")  'projectile-run-vterm-other-window)
  ;; (setq projectile-comint-mode t)


  ;; Projectile settings
  (evil-define-key 'normal 'global (kbd "<leader>C-.")  'persp-switch)
  (evil-define-key 'normal 'global (kbd "<leader><TAB><TAB>")  'persp-switch)
  (evil-define-key 'normal 'global (kbd "<leader><TAB>.")  'persp-switch)
  (evil-define-key 'normal 'global (kbd "<leader>C-r")  'persp-rename)
  (evil-define-key 'normal 'global (kbd "<leader>C-[")  'persp-prev)
  (evil-define-key 'normal 'global (kbd "<leader><escape>")  'persp-prev)
  (evil-define-key 'normal 'global (kbd "<leader>C-]")  'persp-next)
  (evil-define-key 'normal 'global (kbd "<leader>C-k")  'persp-kill)

  (evil-define-key 'normal 'global (kbd "<leader><TAB>r")  'persp-rename)
  (evil-define-key 'normal 'global (kbd "<leader><TAB>[")  'persp-prev)
  (evil-define-key 'normal 'global (kbd "<leader><TAB>-]")  'persp-next)
  (evil-define-key 'normal 'global (kbd "<leader><TAB>k")  'persp-kill)

					;(add-hook 'vterm-mode-hook 'turn-off-evil-mode) 
  ;; (evil-define-key 'normal 'global (kbd "<leader>pe")  (lambda () (interactive)
  ;; 							 (let ((current (current-buffer)))
  ;; 							   (evil-window-split 40)
  ;; 							   (windmove-down)
  ;; 							   (projectile-run-vterm)
  ;; 							   (windmove-up)
  ;; 							   (switch-to-buffer current))))
  (evil-define-key 'normal 'global (kbd "<leader>pj")  'project-find-tag)

  ;; AI bindings
  (evil-define-key 'normal 'global (kbd "<leader>aa")  'aidermacs-transient-menu)

  ;; Shell functions
  ;; (evil-define-key 'normal 'global (kbd "<leader>xm")  'insert-file) ;; Mimic mkdir functionality

  ;; (evil-define-key 'normal 'global (kbd "<leader>xa")  'org-agenda)
  ;; (evil-define-key 'normal 'global (kbd "<leader>xc")  'org-goto-calendar)

  ;; Org Bindings
  (evil-define-key 'normal 'global (kbd "<leader>X")   'org-capture)
  (evil-define-key 'normal 'global (kbd "<leader>on")  'org-capture)
  (evil-define-key 'normal 'global (kbd "<leader>oa")  'org-agenda)
  (evil-define-key 'normal 'global (kbd "<leader>oc")  'org-goto-calenldar)
  (evil-define-key 'normal 'global (kbd "<leader>om")  'org-tags-view)
  ;; (evil-define-key 'normal 'global (kbd "<leader>n*")  'search-org-project-for-symbol) ;; TODO

  ;; Help bindings
  (evil-define-key 'normal 'global (kbd "<leader>ho")  'describe-symbol)
  (evil-define-key 'normal 'global (kbd "<leader>hf")  'describe-function)
  (evil-define-key 'normal 'global (kbd "<leader>hF")  'Info-goto-emacs-command-node)
  (evil-define-key 'normal 'global (kbd "<leader>hv")  'describe-variable)
  (evil-define-key 'normal 'global (kbd "<leader>hk")  'describe-key)
  (evil-define-key 'normal 'global (kbd "<leader>hc")  'describe-key-briefly)
  (evil-define-key 'normal 'global (kbd "<leader>hR")  'info-display-manual)
  (evil-define-key 'normal 'global (kbd "<leader>hi")  'info)
  (evil-define-key 'normal 'global (kbd "<leader>hm")  'describe-mode)
  (evil-define-key 'normal 'global (kbd "<leader>hP")  'describe-package)
  (evil-define-key 'normal 'global (kbd "<leader>hp")  'finder-by-keyword)
  (evil-define-key 'normal 'global (kbd "<leader>ha")  'apropos)
  (evil-define-key 'normal 'global (kbd "<leader>h'")  'describe-char)

  ;; TODO add more which-key bindings to keep track of keymaps for local and major+minor modes
  (evil-define-key 'normal 'global (kbd "<leader>hb")  'embark-bindings)
  (evil-define-key 'normal 'global (kbd "<leader>hB")  'describe-bindings)

  ;; Code actions 
  ;; (evil-define-key 'normal 'global (kbd "<leader>ce")  'eglot)
  ;; (evil-define-key 'normal 'global (kbd "<leader>cc")  'compile)
  ;; (evil-define-key 'normal 'global (kbd "<leader>cC")  'recompile)
  ;; (evil-define-key 'normal 'global (kbd "<leader>k")  'describe-key)
  (evil-define-key 'normal 'global (kbd "<leader>cr")  'xref-find-references)

  ;; LSP bindings
  (evil-define-key 'normal 'global (kbd "<leader>L")  'lsp)
  (evil-define-key 'normal 'global (kbd "<leader>la")  'lsp-execute-code-action)
  (evil-define-key 'normal 'global (kbd "<leader>ls")  'lsp-describe-session)
  (evil-define-key 'normal 'global (kbd "<leader>lrr")  'lsp-rename)
  (evil-define-key 'normal 'global (kbd "<leader>lrf")  'lsp-rename-file)
  (evil-define-key 'normal 'global (kbd "<leader>lgd")  'lsp-find-definition)
  (evil-define-key 'normal 'global (kbd "<leader>lgr")  'lsp-find-references)
  (evil-define-key 'normal 'global (kbd "<leader>lf")  'lsp-format-buffer)
  (evil-define-key 'normal 'global (kbd "<leader>lf")  'lsp-format-buffer)
  (evil-define-key 'normal 'global (kbd "s-O")  'lsp-ui-imenu)
  (evil-define-key 'normal 'global (kbd "s-D")  'dap-debug)
  ;; (evil-define-key 'normal 'global (kbd "<leadlr>k")  'describe-key)
  ;; (evil-define-key 'normal 'global (kbd "<leader>lr")  'xref-find-references)

  ;; Insert and Snippet bindings
  (evil-define-key 'normal 'global (kbd "<leader>is")  'yas-insert-snippet)
  (evil-define-key 'normal 'global (kbd "<leader>yi") 'yas-insert-snippet)


  ;; Misc bindings
  (evil-define-key 'normal 'global (kbd "<leader>xe")  'eshell)
  (evil-define-key 'normal 'global (kbd "<leader>xd")  'xref-find-definitions)
  (evil-define-key 'normal 'global (kbd "<leader>xr")  'xref-find-references)
  (evil-define-key 'normal 'global (kbd "<leader>fs") 'save-buffer)


  ;; (evil-define-key 'normal 'global (kbd "<leader>cf")  ')
  ;; (evil-define-key 'normal 'global (kbd "<leader>cv")  ')
  ;; (evil-define-key 'normal 'global (kbd "<leader>ck")  ')
  ;; (evil-define-key 'normal 'global (kbd "<leader>cc")  ')
  ;; (evil-define-key 'normal 'global (kbd "<leader>ci")  ')
  ;; (evil-define-key 'normal 'global (kbd "<leader>cm")  ')
  ;; (evil-define-key 'normal 'global (kbd "<leader>cP")  ')

  
  (evil-define-key 'motion 'global (kbd "gh")  'consult-flymake)
  ;; (evil-define-key '(normal insert) 'global (kbd "C-p") 'previous-line)
  ;; (evil-define-key '(normal insert) 'global (kbd "C-n") 'next-line)
  (evil-define-key '(insert motion ) 'global (kbd "C-a") 'move-beginning-of-line)
  (evil-define-key '(insert motion ) 'global (kbd "C-e") 'move-end-of-line)
  (evil-define-key '(insert motion ) 'global (kbd "C-f") 'forward-char)
  (evil-define-key '(insert motion ) 'global (kbd "C-b") 'backward-char)
  (evil-define-key '(normal insert motion) 'global (kbd "C-u") 'evil-scroll-up)
  (evil-define-key '(normal insert motion) 'global (kbd "C-k") 'previous-line)
  (evil-define-key '(normal insert motion) 'global (kbd "C-j") 'next-line)
  ;; (evil-define-key '(insert) 'global (kbd "C-d") 'delete-char)


  (evil-set-initial-state 'eshell-mode 'emacs)
  (evil-set-initial-state 'vterm-mode 'emacs)
  (evil-set-undo-system 'undo-redo)
  (setq evil-want-C-u-scroll t)

  :init ;; tweak evil's configuration before loading it
  (setq evil-want-keybinding nil)
  (evil-mode 1))

;; (add-hook 'Info-mode-hook (evil-normalize-keymaps))
;; (add-hook 'dired-mode-hook 'evil-normalize-keymaps) ;; evil-want-keybinding should be null
;; Useful for fixing a bunch of messed up Evil keybindings that Evil doesn't do by default
(use-package evil-collection
  :after evil
  :ensure t)
;; (with-eval-after-load evil-mode
;;   ;; (setq docker-map-list '(docker-container-mode-map
;;   ;; 			  docker-context-mode-map
;;   ;;                         docker-image-mode-map
;;   ;;                         docker-machine-mode-map
;;   ;;                         docker-network-mode-map
;;   ;;                         docker-volume-mode-map))
;;   ;; (add-to-list 'evil-collection-mode-list (append evil-collection-mode-list docker-map-list))
;;   (evil-collection-init))
(evil-collection-init)

;; (with-eval-after-load 'docker (evil-collection-docker-setup))
;; (with-eval-after-load 'info (evil-collection-info-setup))
;; (with-eval-after-load 'calendar (evil-collection-calendar-setup))
;; (with-eval-after-load 'dired (evil-collection-dired-setup))



;; TODO Create keymaps for better which-key +prefix
;; 
;;(which-key-add-key-based-replacements
;; "SPC b" "buffer/bookmark"
;; "C-SPC b" "buffer/bookmark"
;; .
;; .
;; .)


;; (use-package general
;;    :after evil
;;    :ensure t
;;    :init
;;  (setq general-override-states '(insert emacs hybrid normal visual motion operator replace))
;;    :config

;; (general-create-definer my-leader-def
;;   ;; :prefix my-leader
;;   :prefix "SPC")

;; (my-leader-def
;;   :keymaps 'normal
;;   ;; bind "SPC a"
;;   "." 'org-agenda
;;   "bb" 'switch-to-buffer
;;   "c" 'org-capture)

;; )

;; (keymap-unset 'Info-mode-map (kbd "SPC")
;; Plugin for surround texting (ala vim-surround)
(use-package evil-surround
  :after evil
  :ensure t
  :config
  (global-evil-surround-mode 1))

;; Plugin to mimic vim-sneak + vim-seek
(use-package evil-snipe
  :after evil
  :ensure t
  :config
  (evil-snipe-mode 1)
  (evil-snipe-override-mode 1))

(use-package evil-nerd-commenter
  :after evil
  :ensure t
  :config 
  (global-set-key (kbd "s-/") 'evilnc-comment-or-uncomment-lines))

;; (evil-set-initial-state 'dired-mode 'emacs)
;; (evil-set-initial-state 'magit-status-mode 'evil)


(use-package vimish-fold
  :ensure
  :after evil)

(use-package evil-vimish-fold
  :ensure
  :after vimish-fold
  :hook ((prog-mode conf-mode text-mode) . evil-vimish-fold-mode))

(provide 'mg-evil)

;;; evil.el ends here
