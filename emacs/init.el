;; ----------------------------------------
;; init.el
;; devlaf
;; ----------------------------------------

(require 'package)

(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("melpha-stable" . "https://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/"))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

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

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :config
    (exec-path-from-shell-initialize))

(use-package org
  :ensure t
  :custom
    (org-startup-folded t)
  :hook
    ((org-mode . (lambda() (setq truncate-lines nil))))
     (org-mode . (lambda() (electric-indent-local-mode -1)))
     (org-mode . flyspell-mode))

(use-package cc-mode
  :ensure nil
  :after (:all flycheck flycheck-tip)
  :mode (("\\.ino\\'" . c-mode))
  :custom
    (c-basic-offset 2)
    (c-default-style "k&r")
  :hook
    (c-mode . flycheck-mode))

(use-package helm
  :ensure t
  :custom
    (helm-buffers-fuzzy-matching t)
  :config
    (helm-mode 1))

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status))
  :custom
    (magit-ediff-dwim-show-on-hunks t))

(use-package slime
  :ensure t
  :init
    (setq inferior-lisp-program "/usr/bin/sbcl")
  :custom
    (slime-contribs                  `(slime-fancy))
    (slime-complete-symbol-function  `slime-fuzzy-complete-symbol)
    (slime-net-coding-system         'utf-8-unix)
    (slime-lisp-implementations      '((sbcl ("/usr/bin/sbcl")))))

(use-package erlang
  :ensure t
  :mode (("\\.P\\'" . erlang-mode)
         ("\\.E\\'" . erlang-mode)
         ("\\.S\\'" . erlang-mode))
  :config
    (add-hook 'erlang-mode-hook
            (lambda ()
              (setq erlang-compile-extra-opts '((i . "../include"))
                    erlang-root-dir "/usr/lib/erlang"))))

(use-package edts
  :ensure t)

(use-package flycheck
  :ensure t
  :diminish flycheck-mode
  :init
    ;; don't treat init.el as package file
    (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  :custom
    (flycheck-display-errors-function nil)
    (flycheck-check-syntax-automatically '(save))
  :hook
    (after-init . global-flycheck-mode))

(use-package flycheck-tip
  :ensure t
  :custom
    (flycheck-tip-use-timer 'verbose))

(use-package flyspell
  :ensure t
  :custom
    (ispell-program-name "aspell")
    (ispell-list-command "--list"))

(use-package nord-theme
  :ensure t
  :init
    (add-to-list 'custom-theme-load-path(expand-file-name (concat user-emacs-directory "themes")))
  :config
    (load-theme 'nord t))

(use-package nlinum
  :ensure t
  :config
      (global-nlinum-mode))

(use-package go-mode
  :ensure t
  :mode ("\\.go\\'" . go-mode)
  :bind
    (("C-c b" . compile)
     ("C-c o" . godef-jump)
     ("C-c k" . comment-or-uncomment-region))
  :hook
    ((go-mode . (lambda () (add-hook 'before-save-hook #'gofmt-before-save nil 'local)))
     (go-mode . (lambda () (set (make-local-variable 'compile-command) "echo Building... && go build -v && echo Testing... && go test -v && echo Linter... && golint")))))

(use-package vterm
  :ensure t)

(use-package yaml-mode
  :ensure t)

(use-package hcl-mode
  :ensure t
  :mode (("\\.tf\\'" . hcl-mode))
  :custom (hcl-indent-level 2))

(use-package tuareg
  :ensure t
  :mode (("\\.ml[ily]?$" . tuareg-mode)
         ("\\.topml$" . tuareg-mode)))

(use-package merlin
  :ensure t
  :custom
    (merlin-use-auto-complete-mode t)
    (merlin-error-after-save nil)
  :hook (tuareg-mode . merlin-mode))

(use-package utop
  :ensure t
  :config
  (if (executable-find "opam")
    (setq utop-command "opam config exec -- utop -emacs")
    (warn "Cannot find \"opam\" executable."))
  :hook (tuareg-mode . utop-minor-mode))

(use-package minimap
  :ensure t
  :custom
    (minimap-window-location 'right)
    (minimap-mode 1)
  :bind
    ("C-c m" . minimap-mode))

;; ----------------------------------------
;; script
;; ----------------------------------------

(defun sshell (addr port rshell)
  "Open remote shell over ssh."
  (interactive (list
                (read-string "user@host: " nil nil nil)
                (read-string "port [22]: " nil nil "22")
                (read-string "shell [/bin/bash]: " nil nil "/bin/bash")))
  (unless (> (length addr) 0)
    (signal 'wrong-type-argument "addr"))
  (let ((buf (dired (format "/ssh:%s#%s:" addr port)))
        (explicit-shell-file-name rshell))
    (with-current-buffer buf (shell))))

(defun knock (host &rest ports)
    "Port knock host with provided ports"
    (dolist (port ports)
      (shell-command (format "nmap -Pn --host-timeout 100 --max-retries 0 -p %s %s" port host))))

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
;; compilation window
;; ----------------------------------------

(setq compilation-window-height 14)
(setq compilation-scroll-output t)

(defun on-compilation-hook ()
  (when (not (get-buffer-window "*compilation*"))
    (save-selected-window
      (save-excursion
        (let* ((w (split-window-vertically))
               (h (window-height w)))
          (select-window w)
	      (switch-to-buffer "*compilation*")
	      (shrink-window (- h compilation-window-height)))))))
(add-hook 'compilation-mode-hook 'on-compilation-hook)


;; ----------------------------------------
;; rebindings
;; ----------------------------------------

;; (global-set-key (kbd "C-v") 'scroll-page-down)
;; (global-set-key (kbd "M-v") 'scroll-page-up)
;; (global-set-key [next] 'scroll-page-down)
;; (global-set-key [prior] 'scroll-page-up)
(global-set-key "\C-c$" 'toggle-truncate-lines)

;; meta to sys (alt taken by sway)
(setq x-super-keysym 'meta)

;; ----------------------------------------
;; general
;; ----------------------------------------

(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "firefox-wayland")

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
(setq scroll-error-top-bottom t)

;; auto easypg
(require 'epa-file)
(epa-file-enable)

(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)

;; stop prompting me to follow symlinks
(setq vc-follow-symlinks t)

;; cursor
(setq-default cursor-type 'bar)

(add-hook 'term-mode-hook (lambda()
    (setq bidi-paragraph-direction 'left-to-right)))
