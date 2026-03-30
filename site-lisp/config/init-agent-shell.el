;;; init-agent-shell.el --- Configuration for agent-shell

;;; agent-shell/acp/shell-maker

;;; Require
(require 'acp)
(require 'agent-shell)
;; (require 'lazy-load)

;;; Code:

;; 设置默认agent-shell
(setq agent-shell-preferred-agent-config (agent-shell-anthropic-make-claude-code-config))
(setq agent-shell-thought-process-expand-by-default nil)   ;; 思考过程默认折叠
(setq agent-shell-tool-use-expand-by-default nil)          ;; 工具调用默认折叠
(setq agent-shell-user-message-expand-by-default nil)      ;; 用户消息默认折叠
(setq agent-shell-header-style 'text)

;; (lazy-load-set-keys
;;  '(
;;    ("RET" . company-complete-selection)
;;    )
;;  agent-shell-mode-map)

;; 继承环境变量
(setq agent-shell-anthropic-claude-environment
      (agent-shell-make-environment-variables :inherit-env t))
(setq agent-shell-anthropic-authentication
      (agent-shell-anthropic-make-authentication
       :oauth (lambda () (getenv "ANTHROPIC_AUTH_TOKEN"))))

(setq agent-shell-openai-codex-environment
      (agent-shell-make-environment-variables :inherit-env t))
(setq agent-shell-openai-authentication
      (agent-shell-openai-make-authentication
       :api-key (lambda () (getenv "OPENAI_API_KEY"))))

;; 配置共享mcp

(provide 'init-agent-shell)

;;; init-agent-shell.el ends here
