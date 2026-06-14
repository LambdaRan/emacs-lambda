;;; init-agent-shell.el --- Configuration for agent-shell -*- lexical-binding: t; -*-

;;; agent-shell/acp/shell-maker
;;; 延迟加载：acp 和 agent-shell 仅在用户实际调用时才加载。

;;; Code:

;;; 辅助函数（不依赖 agent-shell，可提前定义）
(defun ran-agent-shell--find-claude-code-cli-js ()
  "Return a usable Claude Code `cli.js' path for the GBK patch flow."
  (let ((paths nil)
        found)
    (let ((npm (executable-find "npm")))
      (when npm
        (dolist (args '(("root") ("root" "-g")))
          (let ((root (ignore-errors (car (apply #'process-lines npm args)))))
            (when root
              (push (expand-file-name "@anthropic-ai/claude-code/cli.js" root) paths))))))
    (setq paths
          (append
           (list (expand-file-name "~/.claude-patch-emacs/vendor/claude-code-2.1.112/node_modules/@anthropic-ai/claude-code/cli.js"))
           (nreverse paths)
           (list (expand-file-name "~/node_modules/@anthropic-ai/claude-code/cli.js")
                 (expand-file-name "~/scoop/apps/nvm/current/nodejs/nodejs/node_modules/@anthropic-ai/claude-code/cli.js"))))
    (while (and paths (not found))
      (let ((path (pop paths)))
        (when (file-exists-p path)
          (setq found path))))
    (or found
        (expand-file-name "~/node_modules/@anthropic-ai/claude-code/cli.js"))))

;;; 自动触发加载：首次调用交互命令时加载 acp/agent-shell
;;; init-agent-shell 本身由增量加载器加载（轻量，仅定义函数和 hook），
;;; 但 acp/agent-shell 重型包延迟到用户实际调用时才加载。
(defun ran-agent-shell-claude-select-model ()
  "选择模型后启动 Claude Code。首次调用时自动加载 agent-shell。"
  (interactive)
  (require 'acp)
  (require 'agent-shell)
  ;; with-eval-after-load 已重新定义本函数，重新调用
  (ran-agent-shell-claude-select-model))

(defun ran-agent-shell-codex-select-model ()
  "选择模型后启动 Codex。首次调用时自动加载 agent-shell。"
  (interactive)
  (require 'acp)
  (require 'agent-shell)
  ;; with-eval-after-load 已重新定义本函数，重新调用
  (ran-agent-shell-codex-select-model))

;;; 以下配置在 agent-shell 加载后生效
(with-eval-after-load 'agent-shell
  ;;;; 基础配置
  (setq agent-shell-preferred-agent-config (agent-shell-anthropic-make-claude-code-config))
  (setq agent-shell-thought-process-expand-by-default nil)   ;; 思考过程默认折叠
  (setq agent-shell-tool-use-expand-by-default nil)          ;; 工具调用默认折叠
  (setq agent-shell-user-message-expand-by-default nil)      ;; 用户消息默认折叠
  (setq agent-shell-header-style 'text)

  ;;;; 配置 Claude Code
  (let ((claude-code-cli-js (ran-agent-shell--find-claude-code-cli-js)))
    (setq agent-shell-anthropic-claude-environment
          (agent-shell-make-environment-variables
           :inherit-env t
           ;; preload.cjs 会 monkey-patch child_process.spawn，在 Claude Code
           ;; 子进程启动时自动注入 --require gbk-patch.cjs（绕过 SDK 删除 NODE_OPTIONS 的问题）
           "NODE_OPTIONS" (concat "--require " (expand-file-name "~/.claude-patch-emacs/preload.cjs"))
           "NODE_PATH" (expand-file-name "~/.claude-patch-emacs/node_modules")
           ;; 强制 SDK 走 Node.js cli.js 路径而非预编译的 claude.exe，
           ;; 这样 spawn 命令才是 node，preload.cjs 的拦截才能生效
           "CLAUDE_CODE_EXECUTABLE" claude-code-cli-js)))
  (setq agent-shell-anthropic-authentication
        (agent-shell-anthropic-make-authentication
         :oauth (lambda () (getenv "ANTHROPIC_AUTH_TOKEN"))))

  ;; 可选模型列表
  (defvar ran-claude-model-alist
    '(("opus4.6"   . "claude-opus-4-6")
      ("sonnet4.6" . "claude-sonnet-4-6")
      ("haiku4.5"  . "claude-haiku-4-5-20251001")
      ("dsv4flash" . "deepseek-v4-flash")
      ("dsv4pro" . "deepseek-v4-pro")
      ("qwen3.7max" . "qwen3.7-max")
      ("gemini3.1pro" . "gemini-3.1-pro-preview")
      ("gemini3.1flash" . "gemini-3.1-flash-image-preview")
      ("gemini3.1flashlite" . "gemini-3.1-flash-lite-preview")
      ("gemini3flash" . "gemini-3-flash-preview")
      ("glm5.1" . "glm-5.1")
      ("glm5turbo" . "glm-5-turbo")
      ("glm5" . "glm-5")
      ("glm4.7" . "glm-4.7")
      ("MiniMax2.7" . "MiniMax-M2.7"))
    "模型简称与 model ID 的映射。")

  (defun ran-agent-shell-claude-select-model ()
    "选择模型后启动 Claude Code。"
    (interactive)
    (let* ((choice (completing-read "Select model: "
                                    (mapcar #'car ran-claude-model-alist)
                                    nil t))
           (model-id (cdr (assoc choice ran-claude-model-alist))))
      (setq agent-shell-anthropic-default-model-id model-id)
      (agent-shell-anthropic-start-claude-code)))

  ;;;; 配置 OpenAI
  (setq agent-shell-openai-codex-environment
        (agent-shell-make-environment-variables :inherit-env t))
  (setq agent-shell-openai-authentication
        (agent-shell-anthropic-make-authentication
         :api-key (lambda () (getenv "OPENAI_API_KEY"))))
  ;; 可选模型列表
  (defvar ran-codex-model-alist
    '(("gpt5.4pro" . "gpt-5.4-pro")
      ("gpt5.4" . "gpt-5.4")
      ("gpt5.4mini" . "gpt-5.4-mini")
      )
    "模型简称与 model ID 的映射。")

  (defun ran-agent-shell-codex-select-model ()
    "选择模型后启动 Codex。"
    (interactive)
    (let* ((choice (completing-read "Select model: "
                                    (mapcar #'car ran-codex-model-alist)
                                    nil t))
           (model-id (cdr (assoc choice ran-codex-model-alist))))
      (setq agent-shell-openai-default-model-id model-id)
      (agent-shell-openai-start-codex)))
  ) ;; end with-eval-after-load

(provide 'init-agent-shell)

;;; init-agent-shell.el ends here
