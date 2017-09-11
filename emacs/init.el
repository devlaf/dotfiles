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
    (write-region "" nil custom-file)))
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; ----------------------------------------
;; use-package declarations
;; ----------------------------------------

(use-package evil
  :ensure t
  :config
    (evil-mode 1))

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
