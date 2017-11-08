(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (dummy-h-mode company-c-headers company-flx company-irony-c-headers company-statistics company-irony flycheck-irony irony irony-eldoc cmake-mode clang-format company-jedi projectile elpy json-mode magit docker-compose-mode dockerfile-mode systemd go-snippets company-auctex company-dict company-shell company-web company-go yasnippet haskell-mode csv-mode company hc-zenburn-theme go-mode editorconfig yaml-mode web-mode ssh-config-mode markdown-mode gitignore-mode gitconfig-mode auctex))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; packages
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; better hooks
(defmacro add-hookp (mode hook)
  (let ((mode-hook (intern (format "%s-hook" mode))))
    `(add-hook (quote ,mode-hook) (quote ,hook))))

;; save hooks
(defmacro define-save-minor-mode (fn &optional doc)
  (let* ((mode (intern (format "%s-mode" fn)))
	 (save-hook (intern (format "%s--on-save" mode))))
    `(progn
       (define-minor-mode ,mode ,doc nil nil nil
	 (add-hookp before-save ,save-hook))
       (add-to-list 'safe-local-eval-forms '(,mode 0))
       (defun ,save-hook ()
	 (if ,mode (,fn))))))

;; appearance
(setq inhibit-startup-screen t)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(tool-bar-mode 0)
(load-theme 'hc-zenburn t)
(set-frame-font "xos4 Terminus:pixelsize=14")

;; navigation
(windmove-default-keybindings)
(winner-mode)
(global-set-key [?\C-\\] 'bury-buffer)
(global-set-key [?\C-x ?\C-b] 'ibuffer)

;; transparent frame
(defun toggle-transparent ()
  "Toggle the transparancy of the current frame."
  (interactive)
  (let ((alpha (frame-parameter nil 'alpha)))
    (if (and alpha (not (eq 100 alpha)))
	(set-frame-parameter nil 'alpha 100)
      (set-frame-parameter nil 'alpha 60))))
(global-set-key (kbd "C-`") 'toggle-transparent)

;; Directory navigation
(with-eval-after-load 'dired
  (require 'dired-x)
  (add-hookp dired-mode dired-omit-mode))

;; plain text
(add-to-list 'auto-mode-alist '("README" . text-mode) t)
(with-eval-after-load 'text-mode
  (add-hookp text-mode flyspell-mode)
  (add-hookp text-mode auto-fill-mode)
  (add-hookp text-mode company-mode)
  (with-eval-after-load 'company
    (add-to-list 'company-backends 'company-dict t)))

;; go
(with-eval-after-load 'go-mode
  (setq gofmt-show-errors nil)
  (setq gofmt-command "goimports")
  (define-save-minor-mode gofmt-before-save)
  (add-hookp go-mode gofmt-before-save-mode)
  (with-eval-after-load 'company
    (add-to-list 'company-backends 'company-go)))

;; python
(with-eval-after-load 'python
  (elpy-enable)
  (add-hookp python-mode company-mode)
  (with-eval-after-load 'company
    (add-to-list 'company-backends 'company-jedi)))
(with-eval-after-load 'elpy
  (setq elpy-rpc-timeout 10)
  (define-save-minor-mode elpy-format-code)
  (add-hookp elpy-mode elpy-format-code-mode)
  (define-save-minor-mode elpy-importmagic-fixup)
  (add-hookp elpy-mode elpy-importmagic-fixup-mode)
  (elpy-use-ipython))

;; elisp
(with-eval-after-load 'elisp-mode
  (add-hookp emacs-lisp-mode company-mode)
  (add-hookp emacs-list-mode eldoc-mode))

;; tex
(with-eval-after-load 'auctex
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil)
  (add-hookp TeX-tex-mode company-mode)
  (with-eval-after-load 'company
    (company-auctex-init)))

;; web
(add-to-list 'auto-mode-alist '("\\.\\(css|htm|html|jsx|php|xml\\)\\'" . web-mode))
(with-eval-after-load 'web-mode
  (add-hookp web-mode company-mode)
  (with-eval-after-load 'company
    (add-to-list 'company-backends 'company-web-html)))

;; C/C++
(add-to-list 'auto-mode-alist '("\\.h\\'" . dummy-h-mode))
(with-eval-after-load 'cc-mode
  (define-save-minor-mode clang-format-buffer)
  (add-hookp c-mode-common clang-format-buffer-mode)
  (add-hookp c-mode-common irony-mode)
  (add-hookp c-mode-common yas-minor-mode)
  (add-hookp c-mode-common company-mode)
  (with-eval-after-load 'company
    (add-to-list 'company-backends '(company-irony company-irony-c-headers))))
(with-eval-after-load 'cc-vars
  (add-to-list 'c-default-style '(other . "linux")))
(with-eval-after-load 'irony
  (add-hookp irony-mode irony-eldoc)
  (add-hookp irony-mode irony-cdb-autosetup-compile-options))

;; projectile
(projectile-mode)
(setq projectile-mode-line " Projectile")

;; yasnippets
(require 'yasnippet)
(yas-reload-all)

;; flex matching in company
(add-hookp company-mode company-flx-mode)

;; fix slow starting of emacsclient when run under xterm
(setq xterm-extra-capabilities nil)

;; tramp
(setq tramp-default-method "ssh")
(setq auto-revert-remote-files t)
(setq enable-remote-dir-locals t)

;; generic programing
(setq vc-follow-symlinks t)
(setq enable-recursive-minibuffers t)
(editorconfig-mode)
(global-auto-revert-mode)
(icomplete-mode)
(ido-mode)
(save-place-mode)
(show-paren-mode)

;; all edits in current emacs process
(unless (daemonp)
  (require 'server)
  (setq server-name (format "server-%s" (emacs-pid)))
  (server-start))
(with-eval-after-load 'server
  (setenv "EDITOR" (format "emacsclient -s %s" server-name))
  (setenv "TEXEDIT" (format "emacsclient -s %s +%%d %%s" server-name))
  (setenv "PAGER" "cat")
  (global-set-key [?\C-x ?\C-z] 'server-edit))
