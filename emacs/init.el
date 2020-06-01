;; ----------------------------------------
;; init.el
;; devlaf
;; ----------------------------------------

(require 'package)

(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("melpha-stable" . "http://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))

(package-initialize)

;; ----------------------------------------
;; separate custom.el
;; ----------------------------------------

(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

;; ----------------------------------------
;; `use-package` decs
;; ----------------------------------------

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package helm
  :ensure t
  :config (progn
    (setq helm-buffers-fuzzy-matching t)
    (helm-mode 1)))

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status))
  :config
  (setq magit-ediff-dwim-show-on-hunks t))

(use-package slime
  :ensure t
  :init
  (setq slime-contribs      `(slime-fancy)
        slime-complete-symbol-function  `slime-fuzzy-complete-symbol
        slime-net-coding-system         'utf-8-unix
        slime-lisp-implementations      '((sbcl ("/usr/bin/sbcl")))))

(use-package erlang
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.P\\'" . erlang-mode))
  (add-to-list 'auto-mode-alist '("\\.E\\'" . erlang-mode))
  (add-to-list 'auto-mode-alist '("\\.S\\'" . erlang-mode))
  :config
  (add-hook 'erlang-mode-hook
            (lambda ()
              (setq mode-name "erl"
                    erlang-compile-extra-opts '((i . "../include"))
                    erlang-root-dir "/usr/lib/erlang"))))

(use-package edts
  :ensure t)

(use-package flycheck
  :ensure t
  :diminish flycheck-mode
  :config
  (add-hook 'after-init-hook 'global-flycheck-mode)
  (setq flycheck-display-errors-function nil
        flycheck-erlang-include-path '("../include")
        flycheck-erlang-library-path '()
        flycheck-check-syntax-automatically '(save)))

(use-package flycheck-tip
  :ensure t
  :config
  (setq flycheck-tip-use-timer 'verbose))

(use-package nord-theme
    :ensure t
    :init
    (add-to-list 'custom-theme-load-path(expand-file-name "~/.emacs.d/themes"))
    :config
    (load-theme 'nord t))

(use-package nlinum
    :ensure t
    :config
    (global-nlinum-mode))

;; ----------------------------------------
;; script
;; ----------------------------------------

(defun sshell (remote-path)
    "Open remote shell over ssh."
    (interactive "suser@host: ")
    (let ((default-directory (concat (concat "/ssh:" remote-path) ":"))
          (explicit-shell-file-name "/bin/bash"))
      (shell)))

(defun scroll-page-down ()
    "Scroll page down and adjust cursor to end of buffer on last page."
    (interactive)
    (forward-line
        (- (window-text-height)
            next-screen-context-lines)))

(defun scroll-page-up ()
    "Scroll page up and adjust cursor to top of buffer on first page."
    (interactive)
    (forward-line
        (- 0
            (- (window-text-height)
                next-screen-context-lines))))

(defun kill-other-buffers ()
    "Kill all other buffers except this one and scratch."
    (interactive)
        (let ((exempt (list (current-buffer) (get-buffer "*scratch*"))))
            (mapc 'kill-buffer (seq-difference (buffer-list) exempt #'eq))))
    
;; ----------------------------------------
;; rebindings
;; ----------------------------------------

(global-set-key (kbd "C-v") 'scroll-page-down)
(global-set-key (kbd "M-v") 'scroll-page-up)
(global-set-key [next] 'scroll-page-down)
(global-set-key [prior] 'scroll-page-up)

;; meta to sys (alt taken by sway)
(setq x-super-keysym 'meta)

;; ----------------------------------------
;; general
;; ----------------------------------------

(setq inferior-lisp-program "/usr/bin/sbcl")

(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "firefox")

;; don't save X clipboard manager stuff on exit (creates issues w/ wayland)
(setq x-select-enable-clipboard-manager nil)

;; send all emacs backup to /tmp
(setq backup-directory-alist `(("." . "/tmp")))
(setq backup-by-copying t)
    
;; formatting
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)
(setq-default show-trailing-whitespace t)

;; auto easypg
(require 'epa-file)
(epa-file-enable)

(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)

;; flycheck shouldn't treat init.el as package file
(setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))

