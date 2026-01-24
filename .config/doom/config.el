;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-molokai)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type nil)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
(setq doom-leader-key "S"
      doom-localleader-key ",")

(after! treemacs (treemacs-follow-mode t))
(after! treemacs (setq treemacs-is-never-other-window nil))

(defun my-projectile-ignore-homedir (dir)
  (if (and dir (equal dir (expand-file-name "~/"))) nil dir))
(advice-add 'projectile-locate-dominating-file :filter-return #'my-projectile-ignore-homedir)

(defun my-project-ignore-homedir (project)
  (if (and project (equal (expand-file-name (nth 2 project)) (expand-file-name "~/"))) nil project))
(advice-add 'project-try-vc :filter-return #'my-project-ignore-homedir)

(after! eglot
  (setq-default eglot-inlay-hints-mode nil)
  (add-hook 'eglot-managed-mode-hook (lambda () (eglot-inlay-hints-mode -1))))
(after! centaur-tabs
  (defun centaur-tabs-buffer-groups () "" (list "Default")))

(setq gptel-directives
    '((default . "回复应简明扼要")
      (programming . "你是一位专业的编程助手，你的职责是提供高质量的代码解决方案、重构和解释")
      (writing . "你是一位专业的写作助手，回复应简明扼要")
      (chat . "回复应简明扼要")
      (rewrite . "你是一位专业的编程助手，需要改写这段代码")))
(setq gptel-model 'deepseek-chat
      gptel-backend (gptel-make-deepseek "DeepSeek" :key gptel-api-key :stream t))
(gptel-make-gemini "Gemini" :key gptel-api-key :stream t)
(gptel-make-anthropic "Claude" :models '(claude-sonnet-4-5-20250929) :key gptel-api-key :stream t)

(setq treesit-language-source-alist
      '((c "https://github.com/tree-sitter/tree-sitter-c" "v0.20.8")
        (cpp "https://github.com/tree-sitter/tree-sitter-cpp" "v0.22.3")))

(setq magit-blame-styles '((headings (heading-format . "%h %C %a %s \n"))
                           (highlight (highlight-face . magit-blame-highlight))
                           (lines (show-lines . t) (show-message . t))))

(setq auto-save-default nil)

(setq c-default-style "linux")
(add-hook 'go-mode-hook (lambda () (setq tab-width 8)))
(add-hook 'js-mode-hook (lambda () (setq js-switch-indent-offset 4)))
(setq-hook! 'sh-mode-hook +format-with :none)


(map! :leader "pt" #'+treemacs/toggle)
(map! :leader "cl" #'evilnc-comment-or-uncomment-lines)
(map! :leader "sc" #'evil-ex-nohighlight)

(map! :n "SPC" #'avy-goto-word-1)

(map! :n "gi" #'evil-jump-forward)
(map! :n "go" #'evil-jump-backward)

(map! :n "bn" #'centaur-tabs-forward-tab)
(map! :n "bp" #'centaur-tabs-backward)
(map! :n "bb" #'+vertico/switch-workspace-buffer)
(map! :n "bd" #'kill-current-buffer)

(map! :n "\\m" #'highlight-symbol)

(map! :n "+" #'evil-numbers/inc-at-pt)
(map! :n "-" #'evil-numbers/dec-at-pt)
(map! :v "s" #'evil-surround-region)

(map! :n "zf" #'vimish-fold)
(map! :n "zd" #'vimish-fold-delete)
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
