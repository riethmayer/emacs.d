;; all packages
(setq debug-on-error t)

;; Straight Package manager (instead of package)
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)
;; end of straight config

;; PACKAGES
(use-package magit
  :init
  (global-set-key (kbd "C-x g") 'magit-status)
  :ensure t)

(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown"))

(use-package company
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  :ensure t)

(use-package dracula-theme
  :straight t
  :init
  (load-theme 'dracula t)
  :ensure t)

(use-package projectile
  :init
  (projectile-global-mode)
  :ensure t)

(use-package helm-ag
  :init
  (global-set-key (kbd "<f6>") 'helm-projectile-ag)
  :ensure t)

(use-package helm-projectile
  :init
  (global-set-key (kbd "<f5>") 'helm-projectile)
  :ensure t)

(use-package json-mode
  :ensure t)

(use-package expand-region
  :init
  (global-set-key (kbd "C-=") 'er/expand-region)
  (global-set-key (kbd "C--") 'er/contract-region)
  :ensure t)

(use-package ido-completing-read+
  ;; few ido changes from https://www.masteringemacs.org/article/introduction-to-ido-mode
  :init
  (setq ido-enable-flex-matching t)
  (setq ido-everywhere t)
  (ido-mode 1)
  (setq ido-create-new-buffer 'always)
  :ensure t)

(use-package recentf
  :init
  (setq recentf-auto-cleanup 'never)
  (setq recentf-max-menu-items 25)
  (recentf-mode 1)
  (global-set-key (kbd "C-x f") 'recentf-open-files)
  :ensure t)

(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(use-package copilot
  :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  :ensure t)


;; Display glyph in the fringe of each empty line at the end of the buffer
(setq-default indicate-empty-lines t)
(show-paren-mode 1)                     ;turn paren-mode on
(setq show-paren-delay 0)               ;deactivate the delay
(setq show-paren-style 'expression)

(when (window-system)
  (set-frame-font "Hack")
  (set-face-attribute 'default nil :family "Hack" :height 180)
  (set-face-font 'default "Hack"))

;; fall back on DejaVu for Unicode characters
(when (functionp 'set-fontset-font)
  (set-fontset-font "fontset-default"
                    'unicode
                    (font-spec :family "DejaVu Sans Mono"
                               :width 'normal
                               :size 24.0
                               :weight 'normal)))

(setq-default tab-width 2)
(setq-default sh-basic-offset 2)
(setq-default sh-indentation 2)
(setq-default python-indent 4)
(setq-default indent-tabs-mode nil)

(setq apropos-sort-by-scores t)
(setq visible-bell -1)
(setq css-indent-offset 2)
(setq sql-indent-offset 2)
(setq inhibit-startup-message t)
(setq max-lisp-eval-depth 100000)
(setq max-specpdl-size 100000)

(defun pbcopy ()
  "Copies region to osx clipboard"
  (interactive)
  (let ((deactivate-mark t))
    (call-process-region (point) (mark) "pbcopy")))

(defun pbpaste ()
  "Pastes osx clipboard to region"
  (interactive)
  (call-process-region (point) (if mark-active (mark) (point)) "pbpaste" t t))

(defun pbcut ()
  "Cut region to osx clipboard"
  (interactive)
  (pbcopy)
  (delete-region (region-beginning) (region-end)))


;; emacs built in config
(fset 'yes-or-no-p 'y-or-n-p)
(blink-cursor-mode nil)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(global-subword-mode 1)
(line-number-mode)
(column-number-mode)
(delete-selection-mode t)
(winner-mode 1)
(save-place-mode 1)

;; backup of files
(setq
 backup-by-copying t      ; don't clobber symlinks
 backup-directory-alist
 '(("." . "~/.saves/"))    ; don't litter my fs tree
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t)

;; keybindings

(global-set-key (kbd "C-M-k" ) 'kill-sexp)
(global-set-key (kbd "C-w") 'kill-region-or-backward-kill-word)
(global-set-key (kbd "C-x \\") 'align-regexp)
(global-set-key "\C-s" 'isearch-forward-regexp)
(global-set-key "\C-r" 'isearch-backward-regexp)
(global-set-key (kbd "C-x C-p") 'find-file-at-point)
(global-set-key (kbd "C-c y") 'bury-buffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-c n") 'indent-buffer)

(defun kill-region-or-backward-kill-word (&optional arg region)
  "`kill-region' if the region is active, otherwise `backward-kill-word'"
  (interactive
   (list (prefix-numeric-value current-prefix-arg) (use-region-p)))
  (if region
      (kill-region (region-beginning) (region-end))
    (backward-kill-word arg)))
(global-set-key (kbd "C-w") 'kill-region-or-backward-kill-word)

(add-hook 'after-init-hook
          (lambda ()
            (require 'server)
            (unless (server-running-p)
              (server-start))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["unspecified-bg" "#ff8787" "#5fff87" "#ffff87" "#5f5faf" "#af87ff" "#87d7ff" "#ffffff"])
 '(custom-safe-themes
   '("ed8e6f452855fc7338c8be77803666b34745c19c6667197db48952107fa6d983" default)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
