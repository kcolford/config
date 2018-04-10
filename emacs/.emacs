(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (graphviz-dot-mode ecb semi direnv ggtags use-package-chords diminish edit-server lorem-ipsum auto-package-update yasnippet-snippets go-snippets js2-mode prettier-js less-css-mode flycheck use-package-ensure-system-package use-package pkgbuild-mode company-ghc yasnippet company-try-hard caps-lock clang-format cmake-mode company company-auctex auctex company-c-headers company-dict company-flx company-go company-irony company-irony-c-headers company-shell company-statistics company-web csv-mode docker-compose-mode dockerfile-mode editorconfig elpy flycheck-irony gitconfig-mode gitignore-mode go-mode google google-c-style haskell-mode hc-zenburn-theme irony irony-eldoc json-mode magit markdown-mode projectile ssh-config-mode systemd web-mode yaml-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(defmacro define-save-minor-mode (fn &optional doc)
  "Define a minor mode `FN-mode' that triggers FN every time a file is saved.
The minor mode's documentation is specified in DOC."
  (let ((mode (intern (format "%s-mode" fn))))
    `(progn
       (define-minor-mode ,mode ,doc nil nil nil
	 (if ,mode
	     (add-hook 'before-save-hook (quote ,fn) nil t)
	   (remove-hook 'before-save-hook (quote ,fn) t)))
       (add-to-list 'safe-local-eval-forms '(,mode 0)))))

(setq package-archives '(("melpa-stable" . "https://stable.melpa.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")
			 ("gnu" . "http://elpa.gnu.org/packages/"))
      package-archive-priorities '(("melpa-stable" . 10)
				   ("gnu" . 5)
				   ("melpa" . 0)))
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

;; setup use-package
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
(use-package use-package-chords
  :ensure t
  :config (key-chord-mode 1))
(use-package system-packages
  :ensure t)

;; byte compile the init file
;; (add-hook 'after-save-hook		;compile the init file
;;	  (lambda () (when (equal buffer-file-name user-init-file)
;;		       (emacs-lisp-byte-compile))))

;; some generic settings for emacs as a whole
(scroll-bar-mode 0)
(tool-bar-mode 0)
(icomplete-mode)
(save-place-mode)
(global-auto-revert-mode)
(show-paren-mode)
(windmove-default-keybindings)
(defalias 'yes-or-no-p 'y-or-n-p)
(setq inhibit-startup-screen t)
(setq-default warning-minimum-log-level :error)
(setq completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)
(setq vc-follow-symlinks t)
(setq sentence-end-double-space nil)
(setq confirm-kill-emacs 'yes-or-no-p)
(setq browse-url-browser-function 'browse-url-xdg-open)
(bind-key "C-\\" 'bury-buffer)
(bind-key "C-x C-b" 'ibuffer)
(bind-key "M-]" 'ffap)
(bind-key "C-=" 'start-xterm)
(define-save-minor-mode whitespace-cleanup)
(add-to-list 'safe-local-variable-values '(buffer-file-coding-system . dos))

;; some generic helpers
(defun start-xterm ()
  "Start xterm in local directory."
  (interactive)
  (start-process "xterm" nil "xterm"))

;; tramp integration
(setq-default tramp-default-method "ssh")
(setq-default auto-revert-remote-files t)
(setq enable-remote-dir-locals t)

;; prevent annoying windows from popping up out of reach
(setq display-buffer-base-action '((display-buffer-use-some-window display-buffer-same-window) (nil)))
(setq-default Man-notify-method 'pushy)

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

(use-package generic-x)
(use-package hc-zenburn-theme
  :init (load-theme 'hc-zenburn t))
(use-package flycheck
  :diminish flycheck-mode
  :config (global-flycheck-mode))
(use-package flyspell
  :diminish (flyspell-mode flyspell-prog-mode)
  :hook ((text-mode . flyspell-mode)
	 (prog-mode . flyspell-prog-mode))
  :init
  (setq-default flyspell-use-meta-tab nil))
(use-package editorconfig
  :diminish editorconfig-mode
  :config (editorconfig-mode))
(use-package yasnippet
  :after (elisp-mode cc-mode go-mode)
  :config (yas-global-mode))
(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode))
(use-package direnv
  :ensure-system-package direnv
  :config (direnv-mode))

(use-package company
  :config (global-company-mode))
(use-package company-statistics
  :config (company-statistics-mode))

(defun dired-mark-dotfiles ()
  "Hide all dotfiles in `dired'."
  (interactive)
  (require 'dired)
  (dired-mark-files-regexp "^\\."))
(use-package dired
  :bind (:map dired-mode-map
	      ("b" . browse-url-of-dired-file)
	      ("," . dired-mark-dotfiles)))
(use-package dired-x
  :after dired)

(use-package text-mode
  :mode "README")

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
(use-package reftex
  :hook (LaTeX-mode . reftex-mode)
  :hook (latex-mode . reftex-mode))
(use-package company-auctex
  :after (tex company)
  :config (company-auctex-init))

(use-package elisp-mode
  :defer t
  :bind (:map emacs-lisp-mode-map
	      ("C-c C-s" . apropos)
	      ("C-c C-d" . describe-symbol)))
(use-package eldoc
  :hook (emacs-lisp-mode . eldoc-mode))
(add-hook 'emacs-lisp-mode-hook 'whitespace-cleanup-mode)

(define-save-minor-mode gofmt-before-save)
(use-package go-mode
  :mode "\\.go\\'"
  :commands gofmt-before-save
  :ensure-system-package (goimports . "go-tools")
  :init
  (setq gofmt-show-errors nil)
  (setq gofmt-command "goimports")
  (add-hook 'go-mode-hook 'gofmt-before-save-mode))
(use-package company-go
  :after (go-mode company)
  :ensure-system-package (gocode . "gocode-git")
  :config (add-to-list 'company-backends 'company-go))

(use-package python
  :defer t
  :init
  (setq python-shell-interpreter "ipython")
  (setq python-shell-interpreter-args "-i --simple-prompt"))
(define-save-minor-mode elpy-format-code)
(define-save-minor-mode elpy-importmagic-fixup)
(use-package elpy
  :ensure-system-package (flake8 autopep8 yapf ipython (true . "python-jedi") (true . "python-rope") (virtualenv . "python-virtualenv"))
  :init
  (setq elpy-rpc-timeout 10)
  ;; (add-hook 'elpy-mode-hook 'elpy-format-code-mode)
  ;; (add-hook 'elpy-mode-hook 'elpy-importmagic-fixup-mode)
  :config
  (elpy-enable))

(use-package company-shell
  :after (sh-script after)
  :config (add-to-list 'company-backends 'company-shell))

(use-package web-mode
  :mode "\\.jsx\\'"
  :mode "\\.phtml\\'"
  :mode "\\.php\\'"
  :mode "\\.erb\\'"
  :mode "\\.mustache\\'"
  :mode "\\.djhtml\\'")
(use-package company-web
  :after (company web-mode)
  :config
  (add-to-list 'company-backends 'company-web-html)
  (add-to-list 'company-backends 'company-web-jade)
  (add-to-list 'company-backends 'company-web-slim))
(use-package js2-mode :mode "\\.js\\'")
(use-package prettier-js
  :ensure-system-package prettier
  :hook ((js2-mode . prettier-js-mode)
	 (js-mode . prettier-js-mode)
	 (web-mode . prettier-js-mode)
	 (markdown-mode . prettier-js-mode)
	 (css-mode . prettier-js-mode)
	 (less-css-mode . prettier-js-mode)
	 (json-mode . prettier-js-mode)))

(use-package company-clang
  :init (setq company-clang-arguments "-std=c++11")
  :config (add-to-list 'company-backends 'company-clang))
(use-package google-c-style
  :hook (c-mode-common . google-set-c-style))
(use-package irony
  :ensure-system-package (cmake clang)
  :hook (c-mode-common . irony-mode)
  :init
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
  :config
  (defun irony-setup-cmake ()
    "Have cmake export compile commands for irony."
    (interactive)
    ;; the `compile' function allows us to review the command and change
    ;; it if it's incorrect
    (compile "cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=ON ./build")
    (irony-cdb-autosetup-compile-options))
  (defun irony-setup-make ()
    "Have make export compile commands for irony."
    (interactive)
    ;; the `compile' function allows us to review the command and change
    ;; it if it's incorrect
    (compile "bear make -B")
    (irony-cdb-autosetup-compile-options)))
(use-package irony-eldoc
  :hook irony-mode)
(use-package company-irony
  :after (irony company)
  :init (setq company-irony-ignore-case t)
  :config (add-to-list 'company-backends 'company-irony))
(use-package company-irony-c-headers
  :after (irony company)
  :config (add-to-list 'company-backends 'company-irony-c-headers))
(define-save-minor-mode clang-format-buffer)
;; (add-hook 'c-mode-common-hook 'clang-format-buffer-mode)

(define-save-minor-mode cmake-unscreamify-buffer)
(use-package cmake-mode
  :commands cmake-unscreamify-buffer
  :bind (:map cmake-mode-map
	      ("C-c C-d" . cmake-help))
  :config
  (add-hook 'cmake-mode-hook 'cmake-unscreamify-buffer-mode))

(use-package company-ghc
  :after (haskell-mode company)
  :config (add-to-list 'company-backends 'company-haskell))

(use-package server
  :config
  ;; every emacs is an emacs server so that local shells use the
  ;; current editor to edit
  (unless (daemonp)
    (setq server-name (format "server-%s" (emacs-pid)))
    (server-start))
  ;; (setenv "PAGER" "cat")
  (setenv "EDITOR" (format "emacsclient -nw -s %s" server-name))
  (setenv "VISUAL" (format "emacsclient -c -s %s" server-name))
  (setenv "TEXEDIT" (format "emacsclient -s %s +%%d %%s" server-name)))
(use-package edit-server
  :if (daemonp)
  :config (edit-server-start))
