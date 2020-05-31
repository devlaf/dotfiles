;; ----------------------------------------
;; Devin LaFrance
;; init.el
;; ----------------------------------------

(require 'package)

(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("melpha-stable" . "http://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))

(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Direct customize interface to write its config elsewhere
(let ((filename "~/.emacs.d/custom.el"))
  (unless (file-exists-p filename)
    (write-region "" nil filename)))
(load "~/.emacs.d/custom.el")

;; Send all emacs backup files to /tmp
(setq backup-directory-alist `(("." . "/tmp")))
(setq backup-by-copying t)

;; Don't save X clipboard manager stuff on exit (creates issues w/ wayland)
(setq x-select-enable-clipboard-manager nil)

;; Formatting
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)
(setq-default show-trailing-whitespace t)

;; rebind meta to sys key (alt taken by sway)
(setq x-super-keysym 'meta)

;; auto easypg
(require 'epa-file)
(epa-file-enable)

(tool-bar-mode -1)
(scroll-bar-mode -1)

(setq inferior-lisp-program "/usr/bin/sbcl")

(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "firefox")

;; direct to remote shell over ssh
(defun sshell (remote-path)
    (interactive "suser@host: ")
    (let ((default-directory (concat (concat "/ssh:" remote-path) ":"))
          (explicit-shell-file-name "/bin/bash"))
      (shell)))

;; adjust cursor when scrolling at beginning/end of buffer
(defun scroll-page-down ()
    (interactive)
    (next-line
        (- (window-text-height)
            next-screen-context-lines)))
(defun scroll-page-up ()
    (interactive)
    (previous-line
        (- (window-text-height)
            next-screen-context-lines)))
(global-set-key (kbd "C-v") 'scroll-page-down)
(global-set-key (kbd "M-v") 'scroll-page-up)
(global-set-key [next] 'scroll-page-down)
(global-set-key [prior] 'scroll-page-up)

;; ----------------------------------------
;; use-package declarations
;; ----------------------------------------

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
