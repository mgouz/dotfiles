;; -*- lexical-binding: t -*-
;; Configuration for org and stuff
(require 'org)

(use-package writeroom-mode
  :ensure t
  :commands (writeroom-mode)
  :init
  (setq writeroom-width 100))

;; Org-mode setup
(use-package org-roam
  :ensure t
  ;; :custom
  :bind
  (("C-c n l" . org-roam-buffer-toggle)
   ("C-c n f" . org-roam-node-find)
   ("C-c n g" . org-roam-graph)
   ("C-c n i" . org-roam-node-insert)
   ("C-c n c" . org-roam-capture)

   ;; Dailies
   ("C-c n j" . org-roam-dailies-capture-today))
  :config
  ;; If you're using a vertical completion framework, you might want a more informative completion interface
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)

  (setq org-roam-capture-templates
   '(("d" "default" plain
      "%?"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)))
  ;; If using org-roam-protocol
  ;; (require 'org-roam-protocol)
  )  


(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/org/gtd.org" "Tasks")
         "* TODO %?\n  %i\n  %a")
        ("j" "Journal" entry (file+datetree "~/org/journal.org")
         "* %?\nEntered on %U\n  %i\n  %a")
        ("b" "Blog" entry (file+datetree "~/org/blog.org")
         "* #TITLE: %?\n  %i\n  %a")
	))


;;Org-babel changes
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("4594d6b9753691142f02e67b8eb0fda7d12f6cc9f1299a49b819312d6addad1d"
     "e8bd9bbf6506afca133125b0be48b1f033b1c8647c628652ab7a2fe065c10ef0"
     default))



 '(package-selected-packages
   '(all-the-icons cape corfu diff-hl doom-modeline doom-themes
		   embark-consult evil-collection evil-nerd-commenter
		   evil-snipe evil-surround general magit marginalia
		   orderless org-roam pdf-tools quickrun treemacs-evil
		   vertico yasnippet-snippets)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Set up agenda to look at my actual org files 
(setq org-agenda-files '("~/org" "~/org/roam/"))


(global-set-key (kbd "C-c x") 'org-capture)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-goto-calendar)

;; Trying to add more keywords to the do list... not working right now
;; (setq 'org-todo-keywords (append '((sequence "[ ]" "[X]")) org-todo-keywords))

(add-to-list 'Info-directory-list (concat org-directory "/to-read"))


;; (define-key org-mode-map (kbd "C-l") 'org-shiftright)
;; (define-key org-mode-map (kbd "C-h") 'org-shiftleft)
;; (define-key org-mode-map (kbd "C-<return>") )

(org-babel-do-load-languages 
 'org-babel-load-languages
 '((emacs-lisp . t)
   (awk . t)
   (python . t)
   (js . t)
   (java . t)
   (C . t)
   (sqlite . t)
   (css . t)
   (lua . t)
   (shell . t)))

(setq org-roam-directory "~/org/roam")

(provide 'mg-org)
