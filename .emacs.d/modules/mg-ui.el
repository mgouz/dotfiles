;; Add nicer modeline for reading the VC info, major mode, and Flymake syntax errors
(use-package doom-modeline
  :ensure t
  :config
  (doom-modeline-mode 1))

;; Add VC highlighing in the gutter
(use-package diff-hl
  :ensure t
  :config
  (global-diff-hl-mode 1))

(use-package treemacs
  :ensure t
  )

(use-package treemacs-evil
  :after evil
  :after treemacs
  :ensure t
  )
(use-package all-the-icons
  :ensure t
  :if (display-graphic-p))

(use-package doom-themes
  :ensure t
  :after all-the-icons
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled

  ;; DEFAULT FTW 
  ;; (load-theme 'doom-1337 t)
  ;; (load-theme 'doom-henna t)
  ;; (load-theme 'catppuccin t)

  ;; Enable flashing mode-line on errors
  ;;(doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;(setq catppuccin-flavor 'mocha) ;; or 'frappe 'latte, 'macchiato, or 'mocha
;; (with-eval-after-load 'catppuccin-get-color
;;   (load-theme 'catppuccin t))

(provide 'mg-ui)
;;; mg-ui.el ends here
