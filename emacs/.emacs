(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
(eval-when-compile
  (require 'use-package))
(add-to-list 'safe-local-eval-forms
	     '(add-hook 'after-save-hook 'emacs-lisp-byte-compile nil t))
(menu-bar-mode 0)
(scroll-bar-mode 0)
(tool-bar-mode 0)
(ignore-errors (set-frame-font "Terminus:pixelsize=14"))
(ignore-errors (set-frame-font "xos4 Terminus:pixelsize=14"))
(setq inhibit-startup-screen t)
(global-set-key (kbd "C-\\") 'bury-buffer)
(icomplete-mode)
(save-place-mode)
(show-paren-mode)
(setq warning-minimum-log-level :error)
(setq make-backup-files nil)
(setq vc-follow-symlinks t)
(defalias 'yes-or-no-p 'y-or-n-p)
(require 'generic-x)
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master 'dwim)

(defun toggle-transparent ()
  "Toggle the transparancy of the current frame."
  (interactive)
  (let ((alpha (frame-parameter nil 'alpha)))
    (if (and alpha (not (eq 100 alpha)))
	(set-frame-parameter nil 'alpha 100)
      (set-frame-parameter nil 'alpha 60))))
(global-set-key (kbd "C-`") 'toggle-transparent)

;; save hooks
(defmacro define-save-minor-mode (fn &optional doc)
  "Define a minor mode `fn-mode' that triggers FN every time a file is saved."
  (let ((mode (intern (format "%s-mode" fn))))
    `(progn
       (define-minor-mode ,mode ,doc nil nil nil
	 (if ,mode
	     (add-hook 'before-save-hook (quote ,fn) nil t)
	   (remove-hook 'before-save-hook (quote ,fn) t)))
       (add-to-list 'safe-local-eval-forms '(,mode 0)))))

(use-package use-package
  :ensure t)

(use-package use-package-ensure-system-package
  :ensure t)

(use-package server
  :bind ("C-x C-z" . server-edit)
  :demand t
  :config
  (unless (daemonp)
    (setq server-name (format "server-%s" (emacs-pid)))
    (server-start))
  (setenv "PAGER" "cat")
  (setenv "EDITOR" (format "emacsclient -s %s" server-name))
  (setenv "TEXEDIT" (format "emacsclient -s %s +%%d %%s" server-name)))

(use-package reftex
  :hook (LaTeX-mode . reftex-mode)
  :hook (latex-mode . reftex-mode))

(use-package ibuffer
  :bind ("C-x C-b" . ibuffer))

(use-package ffap
  :bind ("M-]" . ffap))

(use-package caps-lock
  :bind ("C-=" . caps-lock-mode))

(use-package org
  :bind (("C-c a" . org-agenda)
	 ("C-c b" . org-iswitchb)
	 ("C-c c" . org-capture)
	 ("C-c l" . org-store-link)))

(use-package tramp
  :defer t
  :init
  (setq tramp-default-method "ssh")
  (setq auto-revert-remote-files t)
  (setq enable-remote-dir-locals t))

(use-package zone
  :disabled
  :commands zone-when-idle
  :config (zone-when-idle 300))

(use-package autorevert
  :config (global-auto-revert-mode))

(use-package dired
  :bind (:map dired-mode-map
	      ("b" . browse-url-of-dired-file)))

(use-package dired-x
  :after dired
  :hook (dired-mode . dired-omit-mode))

(use-package text-mode
  :defer t
  :init
  (setq sentence-end-double-space nil)
  (add-to-list 'auto-mode-alist '("README" . text-mode) t))

(use-package flyspell
  :hook (text-mode . flyspell-mode)
  :bind (:map flyspell-mode-map
	      ("C-M-i" . nil)))

(use-package simple
  :defer t
  :hook (text-mode . auto-fill-mode))

(use-package elisp-mode
  :defer t
  :bind (:map emacs-lisp-mode-map
	      ("C-c C-s" . apropos)
	      ("C-c C-d" . describe-symbol)))

(use-package eldoc
  :hook (emacs-lisp-mode . eldoc-mode))

(use-package company-statistics
  :ensure t
  :config (company-statistics-mode))

(use-package editorconfig
  :ensure t
  :config (editorconfig-mode))

(use-package flycheck
  :ensure t
  :hook (sh-mode . flycheck-mode))

(use-package company
  :ensure t
  :config (global-company-mode))

(use-package company-clang
  :ensure company
  :init (setq company-clang-arguments "-std=c++11")
  :config (add-to-list 'company-backends 'company-clang))

(use-package yasnippet
  :ensure t
  :config (yas-global-mode))

(use-package projectile
  :ensure t
  :config
  (projectile-mode)
  (setq projectile-completion-system 'default)
  (setq projectile-mode-line ""))

(use-package go-mode
  :ensure t
  :mode "\\.go\\'"
  :commands gofmt-before-save
  :ensure-system-package (goimports . "go get golang.org/x/tools/cmd/goimports")
  :defines gofmt-command
  :defines gofmt-show-errors
  :init
  (setq gofmt-show-errors nil)
  (setq gofmt-command "goimports")
  (add-hook 'go-mode-hook (lambda ()
			    (add-hook 'before-save-hook
				      'gofmt-before-save nil t))))

(use-package company-go
  :ensure t
  :after go-mode
  :after company
  :config (add-to-list 'company-backends 'company-go))

(use-package elpy
  :ensure t
  :init
  (setq elpy-rpc-timeout 10)
  (define-save-minor-mode elpy-format-code)
  (add-hook 'elpy-mode-hook 'elpy-format-code-mode)
  (define-save-minor-mode elpy-importmagic-fixup)
  (add-hook 'elpy-mode-hook 'elpy-importmagic-fixup-mode)
  :config
  (elpy-enable)
  (elpy-use-ipython))

(use-package company-auctex
  :ensure t
  :after tex
  :after company
  :config (company-auctex-init))

(use-package company-shell
  :ensure t
  :after sh-script
  :after company
  :config (add-to-list 'company-backends 'company-shell))

(use-package web-mode
  :ensure t
  :mode "\\.css\\'"
  :mode "\\.htm\\'"
  :mode "\\.html\\'"
  :mode "\\.jsx\\'"
  :mode "\\.php\\'"
  :mode "\\.xml\\'")

(use-package company-web
  :ensure t
  :after company
  :after web-mode
  :config
  (add-to-list 'company-backends 'company-web-html)
  (add-to-list 'company-backends 'company-web-jade)
  (add-to-list 'company-backends 'company-web-slim))

(use-package dummy-h-mode
  :ensure t
  :mode "\\.h\\'")

(use-package google-c-style
  :ensure t
  :hook (c-mode-common . google-set-c-style))

(use-package irony
  :ensure t
  :hook (c-mode-common . irony-mode)
  :init (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))

(defun irony-mode-setup-cmake ()
  "Have cmake export compile commands for irony."
  (interactive)
  ;; the `compile' function allows us to review the command and change
  ;; it if it's incorrect
  (compile "cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=ON ./build")
  (irony-cdb-autosetup-compile-options))

(defun irony-mode-setup-make ()
  "Have make export compile commands for irony."
  (interactive)
  ;; the `compile' function allows us to review the command and change
  ;; it if it's incorrect
  (compile "bear make -B")
  (irony-cdb-autosetup-compile-options))

(use-package irony-eldoc
  :ensure t
  :hook irony-mode)

(use-package company-irony
  :ensure t
  :after irony
  :after company
  :init (setq company-irony-ignore-case t)
  :config (add-to-list 'company-backends 'company-irony))

(use-package company-irony-c-headers
  :ensure t
  :after irony
  :after company
  :config (add-to-list 'company-backends 'company-irony-c-headers))

(use-package cmake-mode
  :commands cmake-unscreamify-buffer
  :ensure t
  :bind (:map cmake-mode-map
	      ("C-c C-d" . cmake-help))
  :config
  (define-save-minor-mode cmake-unscreamify-buffer)
  (add-hook 'cmake-mode-hook 'cmake-unscreamify-buffer-mode))

(use-package company-ghc
  :ensure t
  :after haskell
  :after company
  :config (add-to-list 'company-backends 'company-haskell))

(define-save-minor-mode clang-format-buffer)
(add-hook 'c-mode-common-hook 'clang-format-buffer-mode)

;; just install these and use their autoloads
(defmacro install-package (pkgname &rest args)
  "Invoke `use-package' with ``:defer' and `:ensure'."
  `(use-package ,pkgname :defer t :ensure t ,@args))
(install-package auctex)
(install-package clang-format)
(install-package csv-mode)
(install-package docker-compose-mode)
(install-package dockerfile-mode)
(install-package gitconfig-mode)
(install-package gitconfig-mode)
(install-package gitignore-mode)
(install-package google)
(install-package haskell-mode)
(install-package hc-zenburn-theme :init (load-theme 'hc-zenburn t))
(install-package json-mode)
(install-package magit)
(install-package markdown-mode)
(install-package pkgbuild-mode)
(install-package ssh-config-mode)
(install-package systemd)
(install-package yaml-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (use-package-ensure-system-package use-package pkgbuild-mode company-ghc yasnippet company-try-hard auctex caps-lock clang-format cmake-mode company company-auctex company-c-headers company-dict company-flx company-go company-irony company-irony-c-headers company-shell company-statistics company-web csv-mode docker-compose-mode dockerfile-mode dummy-h-mode editorconfig elpy flycheck-irony gitconfig-mode gitignore-mode go-mode google google-c-style haskell-mode hc-zenburn-theme irony irony-eldoc json-mode magit markdown-mode projectile ssh-config-mode systemd web-mode yaml-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Local Variables:
;; eval: (add-hook 'after-save-hook 'emacs-lisp-byte-compile nil t)
;; End:


