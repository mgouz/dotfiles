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
   (C . t) ; Enables C, C++, and D
   (sqlite . t)
   (css . t)
   (lua . t)
   ;; (ruby . t)
   ;; (go . t)
   ;; (mermaid . t)
   (shell . t)))

(setq org-roam-directory "~/org/roam")

(use-package org-modern
  :ensure t
  :hook (org-mode . global-org-modern-mode)
  :config
  ;; Optional customizations
  (setq org-indented nil)
  )

;; Minimal UI

  (load-theme 'modus-operandi)
;; Choose some fonts
;; (set-face-attribute 'default nil :family "Iosevka")
;; (set-face-attribute 'variable-pitch nil :family "Iosevka Aile")
;; (set-face-attribute 'org-modern-symbol nil :family "Iosevka")

;; Add frame borders and window dividers

;; Add frame borders and window dividers
;; (modify-all-frames-parameters
;;  '((right-divider-width . 40)
;;    (internal-border-width . 40)))
(dolist (face '(window-divider
                window-divider-first-pixel
                window-divider-last-pixel))
  (face-spec-reset-face face)
  (set-face-foreground face (face-attribute 'default :background)))
(set-face-background 'fringe (face-attribute 'default :background))

(setq
 ;; Edit settings
 org-auto-align-tags nil
 org-tags-column 0
 org-catch-invisible-edits 'show-and-error
 org-special-ctrl-a/e t
 org-insert-heading-respect-content t

 ;; Org styling, hide markup etc.
 org-hide-emphasis-markers t
 org-pretty-entities t
 org-agenda-tags-column 0
 org-ellipsis "…")

(with-eval-after-load 'org (global-org-modern-mode))

(use-package org-super-agenda
  :ensure t
  :after org-agenda
  :config
  (setq org-super-agenda-header-map (make-sparse-keymap)) ; Disable org-super-agenda header keybindings
  )
;; (let ((org-super-agenda-groups
;;        '(;; Each group has an implicit boolean OR operator between its selectors.
;;          (:name "Today"  ; Optionally specify section name
;;                 :time-grid t  ; Items that appear on the time grid
;;                 :todo "TODAY")  ; Items that have this TODO keyword
;;          (:name "Important"
;;                 ;; Single arguments given alone
;;                 :tag "bills"
;;                 :priority "A")
;;          ;; Set order of multiple groups at once
;;          (:order-multi (2 (:name "Shopping in town"
;;                                  ;; Boolean AND group matches items that match all subgroups
;;                                  :and (:tag "shopping" :tag "@town"))
;;                           (:name "Food-related"
;;                                  ;; Multiple args given in list with implicit OR
;;                                  :tag ("food" "dinner"))
;;                           (:name "Personal"
;;                                  :habit t
;;                                  :tag "personal")
;;                           (:name "Space-related (non-moon-or-planet-related)"
;;                                  ;; Regexps match case-insensitively on the entire entry
;;                                  :and (:regexp ("space" "NASA")
;;                                                ;; Boolean NOT also has implicit OR between selectors
;;                                                :not (:regexp "moon" :tag "planet")))))
;;          ;; Groups supply their own section names when none are given
;;          (:todo "WAITING" :order 8)  ; Set order of this section
;;          (:todo ("SOMEDAY" "TO-READ" "CHECK" "TO-WATCH" "WATCHING")
;;                 ;; Show this group at the end of the agenda (since it has the
;;                 ;; highest number). If you specified this group last, items
;;                 ;; with these todo keywords that e.g. have priority A would be
;;                 ;; displayed in that group instead, because items are grouped
;;                 ;; out in the order the groups are listed.
;;                 :order 9)
;;          (:priority<= "B"
;;                       ;; Show this section after "Today" and "Important", because
;;                       ;; their order is unspecified, defaulting to 0. Sections
;;                       ;; are displayed lowest-number-first.
;;                       :order 1)
;;          ;; After the last group, the agenda will display items that didn't
;;          ;; match any of these groups, with the default order position of 99
;;          )))
;;   (org-agenda nil "a"))

;; (use-package org-timeblock
;;   :ensure t
;;   :after org
;;   :hook (org-mode . org-timeblock-mode)
;;   :config
;;   (setq org-timeblock-default-block-duration 60)) ; default duration in minutes


(provide 'mg-org)
