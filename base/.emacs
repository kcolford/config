(setq-default package-selected-packages '(
					  auctex
					  caps-lock
					  clang-format
					  cmake-mode
					  company
					  company-auctex
					  company-c-headers
					  company-dict
					  company-flx
					  company-go
					  company-irony
					  company-irony-c-headers
					  company-shell
					  company-statistics
					  company-web
					  csv-mode
					  docker-compose-mode
					  dockerfile-mode
					  dummy-h-mode
					  editorconfig
					  elpy
					  flycheck-irony
					  gitconfig-mode
					  gitignore-mode
					  go-mode
					  go-snippets
					  google
					  google-c-style
					  haskell-mode
					  hc-zenburn-theme
					  irony
					  irony-eldoc
					  json-mode magit
					  markdown-mode
					  projectile
					  ssh-config-mode
					  systemd
					  web-mode
					  yaml-mode
					  yasnippet
					  ))

;; packages
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
(package-install-selected-packages)

;; better hooks
(defmacro add-hookp (mode hook)
  "Convienient wrapper around `add-hook'."
  (let ((mode-hook (intern (format "%s-hook" mode))))
    `(add-hook (quote ,mode-hook) (quote ,hook))))

;; save hooks
(defmacro define-save-minor-mode (fn &optional doc)
  "Define a minor mode `fn-mode' that triggers FN every time a file is saved."
  (let* ((mode (intern (format "%s-mode" fn))))
    `(progn
       (define-minor-mode ,mode ,doc nil nil nil
	 (if ,mode
	     (add-hook 'before-save-hook (quote ,fn) nil t)
	   (remove-hook 'before-save-hook (quote ,fn) t)))
       (add-to-list 'safe-local-eval-forms '(,mode 0)))))

;; extended with-eval-after-load
(defmacro with-eval-after-loads (files &rest body)
  (declare (indent 1))
  `(cond
    ((null ,files)
     (progn ,@body))
    ((consp ,files)
     (with-eval-after-load (car ,files)
       (with-eval-after-loads (cdr ,files)
	 ,@body)))
    (t
     (with-eval-after-load ,files
       ,@body))))

;; transparent frame
(defun toggle-transparent ()
  "Toggle the transparancy of the current frame."
  (interactive)
  (let ((alpha (frame-parameter nil 'alpha)))
    (if (and alpha (not (eq 100 alpha)))
	(set-frame-parameter nil 'alpha 100)
      (set-frame-parameter nil 'alpha 60))))
(global-set-key [?\C-`] 'toggle-transparent)

;; bugfixes
(setq xterm-extra-capabilities nil)
(setq projectile-mode-line "")

;; tramp
(setq tramp-default-method "ssh")
(setq auto-revert-remote-files t)
(setq enable-remote-dir-locals t)

;; appearance
(menu-bar-mode 0)
(scroll-bar-mode 0)
(tool-bar-mode 0)
(load-theme 'hc-zenburn t)
(set-frame-font "xos4 Terminus:pixelsize=14")
(setq inhibit-startup-screen t)

;; general usage improvements
(global-set-key [?\C-\\] 'bury-buffer)
(global-set-key [?\C-x ?\C-b] 'ibuffer)
(global-set-key [?\C-=] 'caps-lock-mode)
(icomplete-mode)
(save-place-mode)
(show-paren-mode)
(windmove-default-keybindings)
(winner-mode)
(setq vc-follow-symlinks t)
(setq enable-recursive-minibuffers t)
(defalias 'yes-or-no-p 'y-or-n-p)

;; IDE
;; (company-flx-mode)
;; (company-statistics-mode)
(editorconfig-mode)
;; (flycheck-mode)
(global-auto-revert-mode)
(global-company-mode)
(global-eldoc-mode)
(projectile-mode)
(yas-global-mode)

;; Directory navigation
(add-hookp dired-mode dired-omit-mode)
(with-eval-after-loads 'dired
  (require 'dired-x)
  (define-key dired-mode-map [b] 'browse-url-of-dired-file))

;; plain text
(add-to-list 'auto-mode-alist '("README" . text-mode) t)
(add-hookp text-mode flyspell-mode)
(add-hookp text-mode auto-fill-mode)
(add-to-list 'company-backends 'company-dict t)

;; go
(setq gofmt-show-errors nil)
(setq gofmt-command "goimports")
(define-save-minor-mode gofmt-before-save)
(add-hookp go-mode gofmt-before-save-mode)
(with-eval-after-load 'go-mode
  (add-to-list 'company-backends 'company-go))

;; python
(setq elpy-rpc-timeout 10)
(define-save-minor-mode elpy-format-code)
(define-save-minor-mode elpy-importmagic-fixup)
(add-hookp elpy-mode elpy-format-code-mode)
(add-hookp elpy-mode elpy-importmagic-fixup-mode)
(with-eval-after-loads 'python
  (elpy-enable))
(with-eval-after-loads 'elpy
  (elpy-use-ipython))

;; elisp
(add-hookp emacs-lisp-mode eldoc-mode)
(with-eval-after-load 'elisp-mode
  (define-key emacs-lisp-mode-map [?\C-c ?\C-s] 'apropos)
  (define-key emacs-lisp-mode-map [?\C-c ?\C-d] 'describe-symbol))

;; tex
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(with-eval-after-load 'auctex
  (company-auctex-init))

;; shell
(with-eval-after-load 'sh-script
  (add-to-list 'company-backends 'company-shell))

;; web
(add-to-list 'auto-mode-alist '("\\.\\(css|htm|html|jsx|php|xml\\)\\'" . web-mode))
(with-eval-after-loads '(web-mode company)
  (add-to-list 'company-backends 'company-web-html)
  (add-to-list 'company-backends 'company-web-jade)
  (add-to-list 'company-backends 'company-web-slim))

;; C/C++
(add-to-list 'auto-mode-alist '("\\.h\\'" . dummy-h-mode))
(define-save-minor-mode clang-format-buffer)
(add-hookp c-mode-common clang-format-buffer-mode)
(add-hookp c-mode-common google-set-c-style)
(add-hookp c-mode-common irony-mode)
(add-hookp irony-mode flycheck-irony-setup)
(add-hookp irony-mode irony-eldoc)
(add-to-list 'company-backends 'company-c-headers)
(add-to-list 'company-backends '(company-irony company-irony-c-headers))

;; cmake
(define-save-minor-mode cmake-unscreamify-buffer)
(add-hookp cmake-mode cmake-unscreamify-buffer-mode)
(with-eval-after-loads 'cmake-mode
  (define-key cmake-mode-map [?\C-c ?\C-d] 'cmake-help))

;; all edits in current emacs process
(unless (daemonp)
  (setq server-name (format "server-%s" (emacs-pid)))
  (server-start))
(global-set-key [?\C-x ?\C-z] 'server-edit)
(setenv "PAGER" "cat")
(with-eval-after-load 'server
  (setenv "EDITOR" (format "emacsclient -s %s" server-name))
  (setenv "TEXEDIT" (format "emacsclient -s %s +%%d %%s" server-name)))
