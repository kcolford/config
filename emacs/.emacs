(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(js-indent-level 2)
 '(package-archives
   (quote
    (("melpa-stable" . "https://stable.melpa.org/packages/")
     ("melpa" . "https://melpa.org/packages/")
     ("gnu" . "http://elpa.gnu.org/packages/"))))
 '(package-selected-packages
   (quote
    (use-package-chords diminish edit-server lorem-ipsum auto-package-update yasnippet-snippets go-snippets js2-mode prettier-js less-css-mode flycheck use-package-ensure-system-package use-package pkgbuild-mode company-ghc yasnippet company-try-hard caps-lock clang-format cmake-mode company company-auctex auctex company-c-headers company-dict company-flx company-go company-irony company-irony-c-headers company-shell company-statistics company-web csv-mode docker-compose-mode dockerfile-mode editorconfig elpy flycheck-irony gitconfig-mode gitignore-mode go-mode google google-c-style haskell-mode hc-zenburn-theme irony irony-eldoc json-mode magit markdown-mode projectile ssh-config-mode systemd web-mode yaml-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(package-initialize)
(let ((firstrun (concat user-emacs-directory "firstrun")))
  (unless (file-readable-p firstrun)
    (package-refresh-contents)
    (let ((oldfunc (symbol-function 'y-or-n-p)))
      (fset 'y-or-n-p '(lambda (&rest args) t))
      ;; (package-install-selected-packages)
      (package-install 'use-package)
      (fset 'y-or-n-p oldfunc))
    (with-temp-buffer (write-file firstrun))))
(eval-when-compile
  (require 'use-package))
(use-package auto-package-update
  :ensure t
  :if (daemonp)
  :config (auto-package-update-maybe))
(use-package bind-key
  :ensure t)
(use-package diminish
  :ensure t)
;; (add-hook 'after-save-hook		;compile the init file
;;	  (lambda () (when (equal buffer-file-name user-init-file)
;;		       (emacs-lisp-byte-compile))))
;; (menu-bar-mode 0)
(scroll-bar-mode 0)
(tool-bar-mode 0)
(ignore-errors (set-frame-font "Terminus:pixelsize=14"))
(ignore-errors (set-frame-font "xos4 Terminus:pixelsize=14"))
(setq inhibit-startup-screen t)
(icomplete-mode)
(save-place-mode)
(global-auto-revert-mode)
(show-paren-mode)
(setq warning-minimum-log-level :error)
(setq vc-follow-symlinks t)
(defalias 'yes-or-no-p 'y-or-n-p)
(setq sentence-end-double-space nil)
(setq confirm-kill-emacs 'yes-or-no-p)
(setq browse-url-browser-function 'browse-url-xdg-open)
(setq tramp-default-method "ssh")
(setq auto-revert-remote-files t)
(setq enable-remote-dir-locals t)
(add-hook 'emacs-lisp-mode-hook 'whitespace-cleanup-mode)
(bind-key "C-\\" 'bury-buffer)
(bind-key "C-x C-b" 'ibuffer)
(bind-key "M-]" 'ffap)
(use-package generic-x)

(use-package use-package-chords
  :ensure t
  :config (key-chord-mode 1))

(use-package system-packages
  :ensure t)

(defun dired-mark-dotfiles ()
  "Hide all dotfiles in dired."
  (interactive)
  (dired-mark-files-regexp "^\\."))

(defmacro define-save-minor-mode (fn &optional doc)
  "Define a minor mode `fn-mode' that triggers FN every time a file is saved."
  (let ((mode (intern (format "%s-mode" fn))))
    `(progn
       (define-minor-mode ,mode ,doc nil nil nil
	 (if ,mode
	     (add-hook 'before-save-hook (quote ,fn) nil t)
	   (remove-hook 'before-save-hook (quote ,fn) t)))
       (add-to-list 'safe-local-eval-forms '(,mode 0)))))

(define-save-minor-mode whitespace-cleanup)

(use-package prettier-js
  :ensure-system-package prettier
  :hook ((js2-mode . prettier-js-mode)
	 (js-mode . prettier-js-mode)
	 (web-mode . prettier-js-mode)
	 (markdown-mode . prettier-js-mode)
	 (css-mode . prettier-js-mode)
	 (less-css-mode . prettier-js-mode)
	 (json-mode . prettier-js-mode)))

(use-package hc-zenburn-theme
  :init (load-theme 'hc-zenburn t))

(use-package edit-server
  :if (daemonp)
  :config (edit-server-start))

(use-package server
  :config
  ;; every emacs is an emacs server so that local shells use the
  ;; current editor to edit
  (unless (daemonp)
    (setq server-name (format "server-%s" (emacs-pid)))
    (server-start))
  (setenv "PAGER" "cat")
  (setenv "EDITOR" (format "emacsclient -s %s" server-name))
  (setenv "TEXEDIT" (format "emacsclient -s %s +%%d %%s" server-name)))

(use-package reftex
  :hook (LaTeX-mode . reftex-mode)
  :hook (latex-mode . reftex-mode))

(use-package org
  :bind (("C-c a" . org-agenda)
	 ("C-c b" . org-iswitchb)
	 ("C-c c" . org-capture)
	 ("C-c l" . org-store-link)))

(use-package tex
  :ensure auctex
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master 'dwim))

(use-package zone
  :disabled
  :commands zone-when-idle
  :config (zone-when-idle 300))

(use-package autorevert
  :config (global-auto-revert-mode))

(use-package dired
  :bind (:map dired-mode-map
	      ("b" . browse-url-of-dired-file)
	      ("," . dired-mark-dotfiles)))



(use-package dired-x
  :disabled
  :after dired
  :hook (dired-mode . dired-omit-mode))

(use-package text-mode
  :mode "README")

(use-package flyspell
  :hook (text-mode . flyspell-mode)
  :bind (:map flyspell-mode-map
	      ("C-M-i" . nil)))

(use-package elisp-mode
  :defer t
  :bind (:map emacs-lisp-mode-map
	      ("C-c C-s" . apropos)
	      ("C-c C-d" . describe-symbol)))

(use-package eldoc
  :hook (emacs-lisp-mode . eldoc-mode))

(use-package company-statistics
  :config (company-statistics-mode))

(use-package editorconfig
  :config (editorconfig-mode))

(use-package flycheck
  :hook (sh-mode . flycheck-mode))

(use-package company
  :config (global-company-mode))

(use-package company-clang
  :init (setq company-clang-arguments "-std=c++11")
  :config (add-to-list 'company-backends 'company-clang))

(use-package yasnippet
  :config (yas-global-mode))

(use-package projectile
  :config
  (projectile-mode)
  (setq projectile-completion-system 'default)
  (setq projectile-mode-line ""))

(use-package go-mode
  :mode "\\.go\\'"
  :commands gofmt-before-save
  :init
  (setq gofmt-show-errors nil)
  (setq gofmt-command "goimports")
  (add-hook 'go-mode-hook (lambda ()
			    (add-hook 'before-save-hook
				      'gofmt-before-save nil t))))

(use-package company-go
  :requires (go-mode company)
  :config (add-to-list 'company-backends 'company-go))

(use-package python
  :defer t
  :init
  (setq python-shell-interpreter "ipython")
  (setq python-shell-interpreter-args "-i --simple-prompt"))

(use-package elpy
  :init
  (setq elpy-rpc-timeout 10)
  (define-save-minor-mode elpy-format-code)
  (add-hook 'elpy-mode-hook 'elpy-format-code-mode)
  (define-save-minor-mode elpy-importmagic-fixup)
  (add-hook 'elpy-mode-hook 'elpy-importmagic-fixup-mode)
  :config
  (elpy-enable))

(use-package company-auctex
  :requires (tex company)
  :config (company-auctex-init))

(use-package company-shell
  :after sh-script
  :requires company
  :config (add-to-list 'company-backends 'company-shell))

(use-package web-mode
  :mode "\\.jsx\\'"
  :mode "\\.phtml\\'"
  :mode "\\.php\\'"
  :mode "\\.erb\\'"
  :mode "\\.mustache\\'"
  :mode "\\.djhtml\\'")

(use-package company-web
  :requires (company web-mode)
  :config
  (add-to-list 'company-backends 'company-web-html)
  (add-to-list 'company-backends 'company-web-jade)
  (add-to-list 'company-backends 'company-web-slim))

(use-package google-c-style
  :hook (c-mode-common . google-set-c-style))

(use-package irony
  :ensure-system-package (cmake clang)
  :hook (c-mode-common . irony-mode)
  :init
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
  :config
  (execute-kbd-macro "\M-xirony-install-server\n\n"))

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
  :hook irony-mode)

(use-package company-irony
  :requires (irony company)
  :init (setq company-irony-ignore-case t)
  :config (add-to-list 'company-backends 'company-irony))

(use-package company-irony-c-headers
  :requires (irony company)
  :config (add-to-list 'company-backends 'company-irony-c-headers))

(use-package cmake-mode
  :commands cmake-unscreamify-buffer
  :bind (:map cmake-mode-map
	      ("C-c C-d" . cmake-help))
  :config
  (define-save-minor-mode cmake-unscreamify-buffer)
  (add-hook 'cmake-mode-hook 'cmake-unscreamify-buffer-mode))

(use-package company-ghc
  :requires (haskell-mode company)
  :config (add-to-list 'company-backends 'company-haskell))

(define-save-minor-mode clang-format-buffer)
(add-hook 'c-mode-common-hook 'clang-format-buffer-mode)

(use-package js2-mode :mode "\\.js\\'")

;; use xclip to copy/paste in emacs-nox
(unless window-system
  (when (getenv "DISPLAY")
    (system-packages-install "xclip")
    (defun xclip-cut-function (text &optional push)
      (with-temp-buffer
	(insert text)
	(call-process-region (point-min) (point-max) "xclip" nil 0 nil "-i" "-selection" "clipboard")))
    (defun xclip-paste-function()
      (let ((xclip-output (shell-command-to-string "xclip -o -selection clipboard")))
	(unless (string= (car kill-ring) xclip-output)
	  xclip-output )))
    (setq interprogram-cut-function 'xclip-cut-function)
    (setq interprogram-paste-function 'xclip-paste-function)))
