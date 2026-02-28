;; -*- lexical-binding: t -*-
;; Configuration for org and stuff
(require 'org)

;; ----------------------------
;; TODO keyword states
;; ----------------------------
(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "WAIT(w@/!)" "|" "DONE(d!)" "CANCELLED(c@)")))

(setq org-todo-keyword-faces
      '(("TODO"      . (:foreground "#ff6c6b" :weight bold))
        ("NEXT"      . (:foreground "#98be65" :weight bold))
        ("WAIT"      . (:foreground "#da8548" :weight bold))
        ("DONE"      . (:foreground "#5b6268" :weight normal))
        ("CANCELLED" . (:foreground "#5b6268" :weight normal :strike-through t))))

;; ----------------------------
;; Global org quality-of-life settings
;; ----------------------------
(setq org-deadline-warning-days 7)
(setq org-agenda-skip-scheduled-if-done t)
(setq org-agenda-skip-deadline-if-done t)
(setq org-log-done 'time)
(setq org-log-into-drawer t)

;; Refile targets: any heading up to 3 levels deep in agenda files
(setq org-refile-targets '((org-agenda-files :maxlevel . 3)))
(setq org-refile-use-outline-path 'file)
(setq org-outline-path-complete-in-steps nil)
(advice-add 'org-refile :after (lambda (&rest _) (org-save-all-org-buffers)))

;; ----------------------------
;; Tags
;; ----------------------------
(setq org-tag-alist
      '((:startgroup)
        ("@home"     . ?h)
        ("@work"     . ?w)
        ("@computer" . ?c)
        ("@errands"  . ?e)
        (:endgroup)
        ("project"  . ?p)
        ("someday"  . ?s)
        ("reading"  . ?r)
        ("career"   . ?j)
        ("tech"     . ?t)
        ("health"   . ?H)
        ("finance"  . ?f)
        ("learning" . ?l)))

(setq org-agenda-prefix-format
      '((agenda  . " %i %-14:c%?-12t% s")
        (todo    . " %i %-14:c")
        (tags    . " %i %-14:c")
        (search  . " %i %-14:c")))

(use-package writeroom-mode
  :ensure t
  :commands (writeroom-mode)
  :init
  (setq writeroom-width 100))

'(org-babel-load-languages
   '((emacs-lisp . t) (awk . t) (python . t) (js . t) (java . t) (C . t)
     (sqlite . t) (css . t) (go . t) (lua . t)))

(add-hook 'org-mode-hook 'flyspell-mode)

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
      '(;; Quick inbox capture — triage later
        ("i" "Inbox" entry (file "~/org/inbox.org")
         "* TODO %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n%i\n"
         :empty-lines 1)

        ;; Scheduled task: forces you to pick a date
        ("t" "Task (scheduled)" entry (file+headline "~/org/gtd.org" "Tasks")
         "* TODO %?\nSCHEDULED: %^t\n:PROPERTIES:\n:CREATED: %U\n:END:\n%i\n%a"
         :empty-lines 1)

        ;; Task with deadline
        ("D" "Deadline task" entry (file+headline "~/org/gtd.org" "Tasks")
         "* TODO %?\nDEADLINE: %^t\n:PROPERTIES:\n:CREATED: %U\n:END:\n%i\n%a"
         :empty-lines 1)

        ;; Journal
        ("j" "Journal" entry (file+datetree "~/org/journal.org")
         "* %?\nEntered on %U\n%i\n%a")

        ;; Blog
        ("b" "Blog" entry (file+datetree "~/org/blog.org")
         "* #TITLE: %?\n  %i\n  %a")

        ;; Project task
        ("p" "Project task" entry (file+headline "~/org/projects.org" "Tasks")
         "* TODO %? :%^{Project tag}:\nSCHEDULED: %^t\n:PROPERTIES:\n:CREATED: %U\n:END:\n%i\n%a"
         :empty-lines 1)))


;; ----------------------------
;; Custom agenda commands
;; ----------------------------
(setq org-agenda-custom-commands
      '(;; "d" = daily driver: what to look at each morning
        ("d" "Daily Dashboard"
         ((agenda "" ((org-agenda-span 1)
                      (org-agenda-start-day nil)
                      (org-super-agenda-groups
                       '((:name "Overdue"             :scheduled past :deadline past :order 1)
                         (:name "Today"               :scheduled today :deadline today :order 2)
                         (:name "In Progress / Next"  :todo ("NEXT") :order 3)
                         (:name "Waiting"             :todo "WAIT" :order 4)
                         (:discard (:anything t))))))
          (todo "NEXT" ((org-agenda-overriding-header "All NEXT actions")
                        (org-agenda-sorting-strategy '(priority-down effort-up))))))

        ;; "w" = weekly review
        ("w" "Weekly Review"
         ((agenda "" ((org-agenda-span 7)))
          (todo "TODO" ((org-agenda-overriding-header "Unscheduled TODOs")
                        (org-agenda-skip-function
                         '(org-agenda-skip-entry-if 'scheduled 'deadline))))
          (todo "WAIT" ((org-agenda-overriding-header "Waiting on...")))))

        ;; "p" = projects
        ("p" "Projects"
         ((tags-todo "+project"
                     ((org-agenda-overriding-header "Active Projects")))))

        ;; "s" = someday/backlog
        ("s" "Someday / Backlog"
         ((todo "TODO"
                ((org-agenda-overriding-header "Someday / Backlog")
                 (org-agenda-skip-function
                  '(org-agenda-skip-entry-if 'notregexp ":someday:"))))))))

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

  ;; (load-theme 'modus-operandi)
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
  (setq org-super-agenda-header-map (make-sparse-keymap))
  (org-super-agenda-mode 1))

;; (use-package org-timeblock
;;   :ensure t
;;   :after org
;;   :hook (org-mode . org-timeblock-mode)
;;   :config
;;   (setq org-timeblock-default-block-duration 60)) ; default duration in minutes

;; Org indentation (correct & predictable)
(setq org-startup-indented t
      org-indent-indentation-per-level 2)

;; Prevent visual shifting bugs
(setq org-adapt-indentation nil)

;; Tabs break Org indentation — disable them
(setq-default indent-tabs-mode nil)

;; ----------------------------
;; Fonts (Nerd Font required)
;; ----------------------------
(set-face-attribute 'default nil
                    :font "Hack Nerd Font"
                    :height 130)

;; Use the Nerd Font for org-modern symbols (checkboxes, priorities, stars)
(set-face-attribute 'org-modern-symbol nil :family "Hack Nerd Font")

;; ----------------------------
;; Org basic appearance
;; ----------------------------
(setq org-hide-emphasis-markers t
      org-pretty-entities t
      org-ellipsis " "   ;; nf-fa-chevron_down
      org-startup-indented t)

;; ----------------------------
;; Nerd Icons
;; ----------------------------
(use-package nerd-icons
  :ensure t)


;; ----------------------------
;; Org Modern
;; ----------------------------
(use-package org-modern
  :ensure t
  :hook ((org-mode . org-modern-mode)
         (org-agenda-finalize . org-modern-agenda))
  :config
  ;; Headings
  (setq org-modern-star 'replace
        org-modern-replace-stars "◉○✸✿")

  ;; Todo keywords
  (setq org-modern-todo-faces
        '(("TODO" :inherit org-todo :weight bold)
          ("NEXT" :inherit org-todo :weight bold)
          ("WAIT" :inherit org-todo :weight bold)
          ("DONE" :inherit org-done :weight bold)))

  ;; Progress bars
  (setq org-modern-progress nil)

  ;; Tags
  (setq org-modern-tag t)

  ;; Tables
  (setq org-modern-table nil)

  ;; Timestamps & planning
  (setq org-modern-timestamp t
        org-modern-planning t)

  ;; Priority styling
  (setq org-modern-priority
        '((?A . "󰀦")   ;; nf-md-alert
          (?B . "󰀨")
          (?C . "󰀩"))))

;; ----------------------------
;; Checkbox styling
;; ----------------------------
(setq org-modern-checkbox
      '((?X . "󰄲")   ;; nf-md-checkbox_marked
        (?- . "󰄱")   ;; nf-md-checkbox_intermediate
        (?\s . "󰄰"))) ;; nf-md-checkbox_blank_outline

;; ----------------------------
;; Agenda styling
;; ----------------------------
(setq org-agenda-tags-column 0
      org-agenda-block-separator ?─)

;; ----------------------------
;; Source blocks
;; ----------------------------
(setq org-src-fontify-natively t
      org-src-tab-acts-natively t
      org-confirm-babel-evaluate nil)

;; ----------------------------
;; Optional: Prettier bullets
;; ----------------------------
(set-face-attribute 'org-level-1 nil :height 1.2 :weight 'bold)
(set-face-attribute 'org-level-2 nil :height 1.15 :weight 'bold)
(set-face-attribute 'org-level-3 nil :height 1.1 :weight 'bold)
(set-face-attribute 'org-level-4 nil :height 1.05)

(provide 'mg-org)
