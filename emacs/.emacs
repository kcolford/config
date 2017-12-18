(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (pkgbuild-mode company-ghc yasnippet company-try-hard auctex caps-lock clang-format cmake-mode company company-auctex company-c-headers company-dict company-flx company-go company-irony company-irony-c-headers company-shell company-statistics company-web csv-mode docker-compose-mode dockerfile-mode dummy-h-mode editorconfig elpy flycheck-irony gitconfig-mode gitignore-mode go-mode google google-c-style haskell-mode hc-zenburn-theme irony irony-eldoc json-mode magit markdown-mode projectile ssh-config-mode systemd web-mode yaml-mode))))
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
(package-refresh-contents)
(let ((y-or-n-p (lambda args t)))
  (package-install-selected-packages))

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

;; transparent frame
(defun toggle-transparent ()
  "Toggle the transparancy of the current frame."
  (interactive)
  (let ((alpha (frame-parameter nil 'alpha)))
    (if (and alpha (not (eq 100 alpha)))
	(set-frame-parameter nil 'alpha 100)
      (set-frame-parameter nil 'alpha 60))))
(global-set-key (kbd "C-`") 'toggle-transparent)

;; tramp
(setq tramp-default-method "ssh")
(setq auto-revert-remote-files t)
(setq enable-remote-dir-locals t)

;; appearance
;(autoload 'zone-when-idle "zone")
(zone-when-idle 300)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(tool-bar-mode 0)
(load-theme 'hc-zenburn t)
(with-demoted-errors "Error: %s"
  (set-frame-font "Terminus:pixelsize=14"))
(with-demoted-errors "Error: %s"
  (set-frame-font "xos4 Terminus:pixelsize=14"))
(setq inhibit-startup-screen t)

;; general usage improvements
(global-set-key (kbd "C-\\") 'bury-buffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-=") 'caps-lock-mode)
(global-set-key (kbd "M-]") 'ffap)
(icomplete-mode)
(save-place-mode)
(show-paren-mode)
(setq make-backup-files nil)
(setq vc-follow-symlinks t)
(defalias 'yes-or-no-p 'y-or-n-p)

;; IDE
(company-statistics-mode)
(editorconfig-mode)
(global-auto-revert-mode)
(global-company-mode)
(global-eldoc-mode)
(yas-global-mode)

;; project management
(projectile-mode)
(setq projectile-completion-system 'default)
(setq projectile-mode-line "")

;; all edits in current emacs process
(unless (daemonp)
  (setq server-name (format "server-%s" (emacs-pid)))
  (server-start))
(global-set-key (kbd "C-x C-z") 'server-edit)
(setenv "PAGER" "cat")
(with-eval-after-load 'server
  (setenv "EDITOR" (format "emacsclient -s %s" server-name))
  (setenv "TEXEDIT" (format "emacsclient -s %s +%%d %%s" server-name)))

;; Directory navigation
(add-hook 'dired-mode-hook 'dired-omit-mode)
(with-eval-after-load 'dired
  (require 'dired-x)
  (define-key dired-mode-map (kbd "b") 'browse-url-of-dired-file))

;; plain text
(setq sentence-end-double-space nil)
(add-to-list 'auto-mode-alist '("README" . text-mode) t)
(add-hook 'text-mode-hook 'flyspell-mode)
(with-eval-after-load 'flyspell
  (define-key flyspell-mode-map (kbd "C-M-i") nil))
(add-hook 'text-mode-hook 'auto-fill-mode)

;; go
(setq gofmt-show-errors nil)
(setq gofmt-command "goimports")
(define-save-minor-mode gofmt-before-save)
(add-hook 'go-mode-hook 'gofmt-before-save-mode)
(with-eval-after-load 'go-mode
  (add-to-list 'company-backends 'company-go))

;; python
(setq elpy-rpc-timeout 10)
(define-save-minor-mode elpy-format-code)
(define-save-minor-mode elpy-importmagic-fixup)
(add-hook 'elpy-mode-hook 'elpy-format-code-mode)
(add-hook 'elpy-mode-hook 'elpy-importmagic-fixup-mode)
(with-eval-after-load 'python
  (elpy-enable))
(with-eval-after-load 'elpy
  (elpy-use-ipython))

;; elisp
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
(with-eval-after-load 'elisp-mode
  (define-key emacs-lisp-mode-map (kbd "C-c C-s") 'apropos))

;; latex/tex
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master 'dwim)
(add-hook 'LaTeX-mode-hook 'reftex-mode)
(add-hook 'latex-mode-hook 'reftex-mode)
(with-eval-after-load 'auctex
  (company-auctex-init))

;; shell
(add-hook 'sh-mode-hook 'flycheck-mode)
(with-eval-after-load 'sh-script
  (add-to-list 'company-backends 'company-shell))

;; web
(add-to-list 'auto-mode-alist '("\\.\\(?:css|htm|html|jsx|php|xml\\)\\'" . web-mode))
(with-eval-after-load 'web-mode
  (add-to-list 'company-backends 'company-web-html)
  (add-to-list 'company-backends 'company-web-jade)
  (add-to-list 'company-backends 'company-web-slim))

;; C/C++
(add-to-list 'auto-mode-alist '("\\.h\\'" . dummy-h-mode))
(define-save-minor-mode clang-format-buffer)
(add-hook 'c-mode-common-hook 'clang-format-buffer-mode)
(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c-mode-common-hook 'irony-mode)
(add-hook 'irony-mode-hook 'irony-eldoc)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
(add-to-list 'company-backends 'company-c-headers)
(add-to-list 'company-backends 'company-irony)
(add-to-list 'company-backends 'company-irony-c-headers)
(with-eval-after-load 'company-clang
  (add-to-list 'company-clang-arguments "-std=c++11"))
(with-eval-after-load 'company-irony
  (setq company-irony-ignore-case t))
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

;; cmake
(define-save-minor-mode cmake-unscreamify-buffer)
(add-hook 'cmake-mode-hook 'cmake-unscreamify-buffer-mode)
(with-eval-after-load 'cmake-mode
  (define-key cmake-mode-map (kbd "C-c C-d") 'cmake-help))

;; org
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c b") 'org-iswitchb)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c l") 'org-store-link)

;; haskell
(with-eval-after-load 'haskell-mode
  (add-to-list 'company-backends 'company-haskell))
