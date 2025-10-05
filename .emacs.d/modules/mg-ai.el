(use-package gptel
  :ensure t
  :bind (("C-c RET" . gptel-send)
	 ("C-c g" . gptel-menu)))

(gptel-make-gh-copilot "Copilot")

 (use-package mcp
  :ensure t
  :after gptel
  :custom (mcp-hub-servers
           `(("filesystem" . (:command "npx" :args ("-y" "@modelcontextprotocol/server-filesystem" "/home/lizqwer/MyProject/")))
             ("fetch" . (:command "uvx" :args ("mcp-server-fetch")))
             ("qdrant" . (:url "http://localhost:8000/sse"))
             ("graphlit" . (
                            :command "npx"
                            :args ("-y" "graphlit-mcp-server")
                            :env (
                                  :GRAPHLIT_ORGANIZATION_ID "your-organization-id"
                                  :GRAPHLIT_ENVIRONMENT_ID "your-environment-id"
                                  :GRAPHLIT_JWT_SECRET "your-jwt-secret")))))
  :config (require 'mcp-hub)
  :hook (after-init . mcp-hub-start-all-server))


(with-eval-after-load 'gptel
(gptel-make-ollama "Ollama"             ;Any name of your choosing
  :host "localhost:11434"               ;Where it's running
  :stream t                             ;Stream responses
  :models '(mistral:latest deepseek-r1:1.5b)))

(use-package aidermacs
  :ensure t
  :bind (("C-c a" . aidermacs-transient-menu))
  :config
  ; Set API_KEY in .bashrc, that will automatically picked up by aider or in elisp
  (setenv "ANTHROPIC_API_KEY" "")
  ; defun my-get-openrouter-api-key yourself elsewhere for security reasons
  ;; (setenv "OPENROUTER_API_KEY" (my-get-openrouter-api-key))
  :custom
  ; See the Configuration section below
  (aidermacs-default-chat-mode 'architect)
  (aidermacs-default-model "github_copilot/gpt-4.1")) ; or "o4-mini" for chatgpt)


(use-package copilot
  :vc (:url "https://github.com/copilot-emacs/copilot.el"
            :rev :newest
            :branch "main"))

(add-hook 'prog-mode-hook 'copilot-mode)
(define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
(define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)

(provide 'mg-ai)
