(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(setq my-package-list '(evil evil-numbers evil-vimish-fold powerline-evil
                        evil-multiedit jedi racer ace-jump-mode ggtags
                        highlight-symbol indent-guide monokai-theme fic-mode
                        python-mode markdown-mode rust-mode yaml-mode
                        mmm-jinja2 jinja2-mode salt-mode
                        flycheck flycheck-rust
                        git-blamed git-gutter+
                        project-explorer tabbar tabbar-ruler))
(when (not package-archive-contents) (package-refresh-contents))
(mapc #'package-install my-package-list)

(add-to-list 'load-path "~/.emacs.d/lisp/")
(load-file "~/.emacs.d/lisp/robot-mode.el")

(setq-default message-log-max nil)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(menu-bar-mode -1)
(setq-default indent-tabs-mode nil)

(powerline-default-theme)

(evil-vimish-fold-mode t)
(load-theme 'monokai t)
(setq inhibit-startup-screen t)

(setq show-paren-style 'parentheses)
(setq visible-bell t)
(setq-default tab-width 4)
(setq tab-width 4)
(setq split-width-threshold 10)
(standard-display-ascii ?\t "·\t")

(evil-mode t)
(add-hook 'shell-mode (lambda () (evil-mode nil)))
(define-key evil-normal-state-map "\\m" 'highlight-symbol)
(define-key evil-normal-state-map "gi" 'evil-jump-forward)
(define-key evil-normal-state-map "go" 'evil-jump-backward)
(define-key evil-normal-state-map "w" 'evil-forward-WORD-begin)
(define-key evil-normal-state-map "W" 'evil-backward-WORD-begin)
(define-key evil-normal-state-map "bn" 'tabbar-forward-tab)
(define-key evil-normal-state-map "bp" 'tabbar-backward-tab)
(define-key evil-normal-state-map "bd" 'kill-this-buffer)
(define-key evil-normal-state-map "bl" 'list-buffers)
(define-key evil-normal-state-map (kbd "C-n") 'evil-scroll-line-down)
(define-key evil-normal-state-map (kbd "C-p") 'evil-scroll-line-up)
(define-key evil-normal-state-map (kbd "C-c +") 'evil-numbers/inc-at-pt)
(define-key evil-normal-state-map (kbd "C-c -") 'evil-numbers/dec-at-pt)
(define-key evil-normal-state-map (kbd "SPC") 'ace-jump-mode)
(define-key evil-visual-state-map "R" 'evil-multiedit-match-all)
(define-key evil-visual-state-map "W" 'evil-backward-WORD-begin)
(define-key evil-visual-state-map (kbd "C-m") 'comment-or-uncomment-region)
(define-key evil-insert-state-map (kbd "C-c C-c") 'evil-normal-state)

(add-to-list 'evil-emacs-state-modes 'project-explorer-mode)
(global-set-key (kbd "M-p") 'project-explorer-toggle)
(add-hook 'pe/before-tree-lookup-hook (lambda () (define-key project-explorer-mode-map (kbd "C-f") 'scroll-up)
                                                 (define-key project-explorer-mode-map (kbd "C-b") 'scroll-down)
                                                 (define-key project-explorer-mode-map "/" 'isearch-forward)
                                                 (define-key project-explorer-mode-map "?" 'isearch-backward)))

(define-key evil-normal-state-map "p" 'evil-paste-after)

(global-git-gutter+-mode)
(setq git-gutter+-modified-sign " ")
(set-face-background 'git-gutter+-modified "purple")
(indent-guide-global-mode)

(tabbar-mode t)

(defun my-tabbar-buffer-groups ()
  (list (cond ((string-match "\\(scratch\\|epc\\|\*Flycheck\\|\*Warnings\\|\*magit\\|\*Completion\\)" (buffer-name)) "emacs")
              ((eq major-mode 'dired-mode) "emacs")
              (t "user"))))
(setq tabbar-buffer-groups-function 'my-tabbar-buffer-groups)

;; Removes *scratch* from buffer after the mode has been set.
;; (defun remove-scratch-buffer ()
;;   (if (get-buffer "*scratch*")
;;       (kill-buffer "*scratch*")))
;; (add-hook 'after-change-major-mode-hook 'remove-scratch-buffer)

;; Removes *messages* from the buffer.
(setq-default message-log-max nil)
(kill-buffer "*Messages*")

;; Removes *Completions* from buffer after you've opened a file.
;; (add-hook 'minibuffer-exit-hook
;;       '(lambda ()
;;          (let ((buffer "*Completions*"))
;;            (and (get-buffer buffer)
;;                 (kill-buffer buffer)))))

;; Don't show *Buffer list* when opening multiple files at the same time.
(setq inhibit-startup-buffer-menu t)

;; Show only one active window when opening multiple files at the same time.
(add-hook 'window-setup-hook 'delete-other-windows)

(modify-syntax-entry ?_ "w")

(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
              (ggtags-mode 1))
            (define-key evil-normal-state-map "gd" 'ggtags-find-tag-dwim)
            (define-key evil-normal-state-map "gr" 'ggtags-find-reference)
            (modify-syntax-entry ?_ "w")))

(autoload 'markdown-mode "markdown-mode" "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(add-to-list 'auto-mode-alist '("\\.jinja\\'" . jinja2-mode))

;; (defun flycheck-display-error-messages-unless-error-buffer (errors)
;;     (unless (get-buffer-window flycheck-error-list-buffer)
;;           (flycheck-display-error-messages errors)))
;;
;; (setq flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-buffer)


(autoload 'rust-mode "rust-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))
(setq racer-cmd "~/.cargo/bin/racer")
(setq racer-rust-src-path "~/.rust/src/")
(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'rust-mode-hook #'flycheck-rust-setup)
(add-hook 'rust-mode-hook #'flycheck-mode)
(add-hook 'racer-mode-hook
          (lambda ()
            (define-key evil-normal-state-map "gd" 'racer-find-definition)))
(add-hook 'racer-mode-hook #'eldoc-mode)

(autoload 'groovy-mode "groovy-mode" "Major mode for editing Groovy files" t)

(setq whitespace-style '(face lines-tail))
(setq whitespace-line-column 80)
(add-hook 'python-mode-hook #'flycheck-mode)
(add-hook 'python-mode-hook 'jedi:setup)
(add-hook 'python-mode-hook 'whitespace-mode)
(add-hook 'python-mode-hook
          (lambda ()
            (fic-mode)
            (setq jedi:complete-on-dot t)
            (define-key evil-normal-state-map "gd" 'jedi:goto-definition)))

(add-to-list 'auto-mode-alist '("\\.robot\\'" . robot-mode))

(defadvice quit-window (before quit-window-always-kill)
  "When running `quit-window', always kill the buffer."
  (ad-set-arg 0 t))
(ad-activate 'quit-window)

(defcustom fic-highlighted-words '("FIXME" "TODO" "BUG" "XXX")
  "Words to highlight"
  :group 'fic-mode)

(defcustom tabbar-hide-header-button t
  "Hide header button at left-up corner. Default is t."
  :type 'boolean
  :set (lambda (symbol value)
         (set symbol value)
         (if value
             (setq
              tabbar-scroll-left-help-function nil ;don't show help information
              tabbar-scroll-right-help-function nil
              tabbar-help-on-tab-function nil
              tabbar-home-help-function nil
              tabbar-buffer-home-button (quote (("") "")) ;don't show tabbar button
              tabbar-scroll-left-button (quote (("") ""))
              tabbar-scroll-right-button (quote (("") "")))))
  :group 'tabbar)

(defadvice quit-window (before quit-window-always-kill)
  "When running `quit-window', always kill the buffer."
  (ad-set-arg 0 t))
(ad-activate 'quit-window)

(setq tabbar-background-color "#959A79") ;; the color of the tabbar background
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(tabbar-default ((t (:inherit variable-pitch :background "#959A79" :foreground "black" :weight bold))))
 '(tabbar-highlight ((t (:underline t))))
 '(tabbar-selected ((t (:inherit tabbar-default :background "#95CA59"))))
 '(tabbar-unselected ((t (:inherit tabbar-default)))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-file-name-transforms (quote ((".*" "~/.emacs.d/autosaves/\\1" t))))
 '(backup-directory-alist (quote ((".*" . "~/.emacs.d/backups/"))))
 '(package-selected-packages
   (quote
    (tabbar flycheck mmm-jinja2 yaml-mode rust-mode jedi evil tabbar-ruler salt-mode rustfmt racer python-mode pylint project-explorer powerline-evil php-mode org nasm-mode monokai-theme mo-git-blame markdown-mode jinja2-mode jedi-direx jdee indent-guide highlight-symbol highlight-defined highlight-current-line highlight groovy-mode git-gutter+ git-blamed git-blame gh-md ggtags fringe-helper flymd flymake-rust flymake-php flycheck-rust flycheck-pyflakes flycheck-pos-tip flycheck-cython fic-mode evil-visualstar evil-vimish-fold evil-numbers evil-multiedit evil-mc evil-matchit evil-magit elisp-lint elisp-format column-marker cargo bind-key airline-themes ace-jump-mode 0blayout)))
 '(show-paren-mode t)
 '(tabbar-separator (quote (1.5)))
 '(tool-bar-mode nil))
(make-directory "~/.emacs.d/autosaves/" t)
(setq ring-bell-function 'ignore)
