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
  ;; Don't set emacs mode because it can affect
  (evil-set-leader '(normal motion) (kbd "SPC")) ;; Has issues in Info mode, finder, and ibuffer/dired
  (evil-define-key 'normal  'global  (kbd "<leader>.")  'find-file)
  (evil-define-key 'normal  'global (kbd "<leader>;")  'eval-expression)
  (evil-define-key 'normal  'global (kbd "<leader>:")  'execute-extended-command)
  (evil-define-key 'normal  'global (kbd "<leader>/")  'consult-line) ;; was evil-search-forward
  (evil-define-key 'normal  'global (kbd "C-z")  'suspend-frame)
  (evil-define-key 'normal  'global (kbd "C-x C-z")  'evil-emacs-state)




  ;; (define-prefix-command 'buffer)
  (evil-define-key 'normal 'global (kbd "<leader>bi")  'ibuffer)
  (evil-define-key 'normal 'global (kbd "<leader>bk")  'kill-buffer)
  (evil-define-key 'normal 'global (kbd "<leader>B")  'switch-to-buffer)
  (evil-define-key 'normal 'global (kbd "<leader>bb")  'switch-to-buffer)

  ;; Projectile settings
  (evil-define-key 'normal 'global (kbd "<leader>pi")  'consult-project-buffer)
  (evil-define-key 'normal 'global (kbd "<leader>pf")  'project-find-file)
  (evil-define-key 'normal 'global (kbd "<leader>pp")  'project-switch-project)
  (evil-define-key 'normal 'global (kbd "<leader>pt")  'treemacs)
  (evil-define-key 'normal 'global (kbd "<leader>pd")  'project-find-dir)
  ;; (evil-define-key 'normal 'global (kbd "<leader>pe")  'projectile-run-vterm-other-window)

  ;; Projectile settings
  (evil-define-key 'normal 'global (kbd "<leader>C-.")  'persp-switch)
  (evil-define-key 'normal 'global (kbd "<leader><TAB><TAB>")  'persp-switch)
  (evil-define-key 'normal 'global (kbd "<leader><TAB>.")  'persp-switch)
  (evil-define-key 'normal 'global (kbd "<leader>C-r")  'persp-rename)
  (evil-define-key 'normal 'global (kbd "<leader>C-[")  'persp-prev)
  (evil-define-key 'normal 'global (kbd "<leader><escape>")  'persp-prev)
  (evil-define-key 'normal 'global (kbd "<leader>C-]")  'persp-next)
  (evil-define-key 'normal 'global (kbd "<leader>C-d")  'persp-kill)

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
  (evil-define-key 'normal 'global (kbd "<leader>nn")  'org-capture)
  (evil-define-key 'normal 'global (kbd "<leader>na")  'org-agenda)
  (evil-define-key 'normal 'global (kbd "<leader>nc")  'org-goto-calenldar)
  (evil-define-key 'normal 'global (kbd "<leader>nm")  'org-tags-view)
  ;; (evil-define-key 'normal 'global (kbd "<leader>n*")  'search-org-project-for-symbol) ;; TODO
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
  (evil-define-key 'normal 'global (kbd "<leader>ha")  'apropos)
  (evil-define-key 'normal 'global (kbd "<leader>h'")  'describe-char)

  ;; TODO add more which-key bindings to keep track of keymaps for local and major+minor modes
  (evil-define-key 'normal 'global (kbd "<leader>hb")  'embark-bindings)
  (evil-define-key 'normal 'global (kbd "<leader>hB")  'describe-bindings)

  ;; Code actions 
  (evil-define-key 'normal 'global (kbd "<leader>ce")  'eglot)
  (evil-define-key 'normal 'global (kbd "<leader>cc")  'compile)
  (evil-define-key 'normal 'global (kbd "<leader>cC")  'recompile)
  ;; (evil-define-key 'normal 'global (kbd "<leader>k")  'describe-key)
  (evil-define-key 'normal 'global (kbd "<leader>cr")  'xref-find-references)

  ;; Insert
  (evil-define-key 'normal 'global (kbd "<leader>is")  'yas-insert-snippet)

  (evil-define-key 'normal 'global (kbd "<leader>xe")  'eshell)
  (evil-define-key 'normal 'global (kbd "<leader>fs") 'save-buffer)
  (evil-define-key 'normal 'global (kbd "<leader>yi") 'yas-insert-snippet)


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
  (evil-define-key '(normal insert motion) 'global (kbd "C-u") 'evil-scroll-up)


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
  :ensure t
  :config
  (evil-collection-init))



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

(provide 'mg-evil)

;;; evil.el ends here
