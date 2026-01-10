;;; mg-wm --- Windows management for my emacs config -*- lexical-binding: t; -*-
;;; Commentary:
; Check out this article for learning more about windows management: https://www.masteringemacs.org/article/demystifying-emacs-window-manager
;; And don’t forget: you can type M-x window-toggle-side-windows to toggle them visible or hidden.
;;; Code: 

;;; NOTE: This and perspective.el causeed some weird issues with spawning a MILLION frames and crashing emacs.
;;; It's not just persp mode though and seems to be some other package interaction. 
;; (add-to-list 'display-buffer-alist
;;    '("\\*Help\\*"
;;      (display-buffer-in-child-frame)))
(add-to-list 'display-buffer-alist
   '("\\*Ibuffer\\*"
     (display-buffer-in-side-window)
     (side . left)
     (slot . 0)
     (window-width . .15)
     (window-parameters .
			((no-delete-other-windows . t) ;; will not be deleted when calling C-x 0
			 (no-other-window . t) ;; will not be selected by C-x o
			 ))))

(add-to-list 'display-buffer-alist
   '("\\*Help\\*"
     (display-buffer-in-side-window)
     (side . left)
     (slot . 0)
     (window-width . .3)
     (window-parameters .
			((no-delete-other-windows . t) ;; will not be deleted when calling C-x 0
			 (no-other-window . t) ;; will not be selected by C-x o
			 ))))

(add-to-list 'display-buffer-alist
 '("\\*Help\\*"
   (display-buffer-reuse-window display-buffer-pop-up-window)
   (inhibit-same-window . t)))

;; (add-to-list 'display-buffer-alist
;;  '("\\*Info\\*"
;;    (display-buffer-us)
;;    (inhibit-same-window . t)))

;; move compilation buffer to bottom
;; (add-to-list 'display-buffer-alist
;; 	     '((or 
;; 		(buffer-name . "\\*\\(e?shell\\|vterm\\)\\*")
;; 		(major-mode . help-mode)
;; 		(major-mode . vterm-mode)
;; 	 (major-mode . compilation-mode)
;; 	 (major-mode . eshell-mode))
;;      (displayp-buffer-in-side-window)
     ;; (display-buffer-reuse-window)
     ;; (side . bottom)
     ;; (window-width . .2)))

;; Make compilation and vterm buffers reuse the same window at bottom
(add-to-list 'display-buffer-alist
  '("\\*\\(compilation\\|vterm\\)\\*"
    (display-buffer-reuse-window display-buffer-in-direction)
    (direction . bottom)
    (window . root)
    (window-height . 0.3)
    (reusable-frames . visible)))

;; Reuse xref, grep, and occur buffers instead of opening new ones
(add-to-list 'display-buffer-alist
          `(,(rx (| "*xref*"
                    "*grep*"
                    "*Occur*"))
            display-buffer-reuse-window
            (inhibit-same-window . nil)))

(setq magit-display-buffer-function #'display-buffer)

(add-to-list 'display-buffer-alist
          `((derived-mode . magit-mode)
            (display-buffer-reuse-mode-window
             display-buffer-in-direction)
            (mode magit-mode)
            (window . root)
            (window-width . 0.30)
            (direction . right)))

;; (add-to-list 'display-buffer-alist
;;           `(,(rx (| "*compilation*" "*grep*"))
;;             display-buffer-in-side-window
;;             (side . right)
;;             (slot . 0)
;;             (window-parameters . ((no-delete-other-windows . t)))
;;             (window-width . 80)))

;; Example of moving test buffers to the right side
;; (add-to-list 'display-buffer-alist
;;          `("^test[-_]"
;;            display-buffer-in-direction
;;            (direction . right)))

;;; -------------- TABS ------------------

;; (add-to-list 'display-buffer-alist
;;           `((or (derived-mode . org-mode)
;;                 (derived-mode . org-agenda-mode))
;;             (display-buffer-in-tab display-buffer-in-direction)
;;             (ignore-current-tab . t)
;;             (direction . bottom)
;;             (window-height . .2)
;;             (tab-name . "📝 My ORG Mode Files")
;;             ;; Optional
;;             (tab-group . "Org")))


(defun mp-buffer-has-project-p (buffer action)
  (with-current-buffer buffer (project-current nil)))

(defun mp-tab-group-name (buffer alist)
  (with-current-buffer buffer (concat "🗃 " (or (cdr (project-current nil)) "🛡 Ungrouped"))))

(defun mp-tab-tab-name (buffer alist)
  (with-current-buffer buffer
    (buffer-name)))

;; (add-to-list 'display-buffer-alist
;;              '(mp-buffer-has-project-p
;;                (display-buffer-in-tab display-buffer-reuse-window)
;;                (tab-name . mp-tab-tab-name)
;;                (tab-group . mp-tab-group-name)))

;; ;;; OPTIONAL, but probably required for everything to work 100%
;; (defun tab-bar-tabs-set (tabs &optional frame)
;;   "Set a list of TABS on the FRAME."
;;   (set-frame-parameter frame 'tabs (seq-sort-by (lambda (el) (alist-get 'group el nil))
;;                                                 #'string-lessp
;;                                                 tabs)))

;; (defun mp-reload-tab-bars (&optional dummy)
;;   "Reload the tab bars... because they're buggy."
;;   (interactive)
;;   (tab-bar-tabs-set (frame-parameter nil 'tabs)))

;; (add-hook 'kill-buffer-hook #'mp-reload-tab-bars)
;; (add-hook 'window-selection-change-functions #'mp-reload-tab-bars)
;;; -------------- Helpers ------------------

(defun mp-buffer-has-project-p (buffer action)
  "Return non-nil if BUFFER is part of a project."
  (with-current-buffer buffer (project-current nil)))

(provide 'mg-wm)
;;; mg-wm.el ends here
