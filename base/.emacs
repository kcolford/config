;;; .emacs -- personal init file
;;; Commentary:
;;; Code:
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (ido-yes-or-no ox-gfm auto-package-update go-snippets company-auctex company-c-headers company-dict company-quickhelp company-shell company-web company-go yasnippet yasnippit haskell-mode csv-mode company hc-zenburn-theme dockerfile-mode android-mode go-mode editorconfig yaml-mode web-mode systemd ssh-config-mode nginx-mode markdown-mode gitignore-mode gitconfig-mode auctex))))
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

;; package updates
(when (getenv "EMACS_ASYNC_COMMAND_UPDATE")
  (package-install-selected-packages)
  (package-autoremove)
  (auto-package-update-maybe)
  (kill-emacs))
(start-process-shell-command "update" nil "EMACS_ASYNC_COMMAND_UPDATE=y emacs &")

;; initscript helpers
(defun choose-mode (mode ext)
  "Assign MODE to be used for each extention listed in EXT."
  (when (consp ext)
    (add-to-list 'auto-mode-alist (cons (format "\\.%s\\'" (car ext)) mode))
    (choose-mode mode (cdr ext))))
(defmacro mode-company (hook fn)
  "Add FN to company-backends when HOOK is run."
  `(add-hook ,hook (lambda ()
		     (add-to-list (make-local-variable 'company-backends) ,fn))))

;; appearance
(setq inhibit-startup-screen t)
;(menu-bar-mode 0)
(scroll-bar-mode 0)
(tool-bar-mode 0)
(load-theme 'hc-zenburn t)
(set-frame-font "-xos4-xos4 Terminus-normal-normal-normal-*-14-*-*-*-c-80-iso10646-1")

;; Directory navigation
(with-eval-after-load "dired"
  (require 'dired-aux)
  (require 'dired-x)
  (define-key dired-mode-map (kbd "b") 'browse-url-of-dired-file))
(setq dired-listing-switches "-lh")
(define-key global-map (kbd "C-x C-d") 'dired)

;; version control
(define-key global-map (kbd "C-x v s") 'vc-git-grep)
(with-eval-after-load "vc-dir"
  (define-key vc-dir-mode-map (kbd "s") 'vc-git-grep))
(setq vc-follow-symlinks t)

;; don't pop up a new window all the time
(setq display-buffer-alist '((".*" display-buffer-same-window (nil))))
(setq-default Man-notify-method 'pushy)

;; org mode
(setq-default org-export-backends '(ascii html latex md org))

;; gtags
(setq gtags-suggested-key-mapping t)
(autoload 'gtags-mode "gtags" "" t)
(with-eval-after-load "gtags"
  (define-key gtags-mode-map (kbd "M-,") 'gtags-pop-stack))
(add-hook 'c-mode-common-hook (lambda () (gtags-mode 1)))

;; transparent frame
(defun toggle-transparent ()
  "Toggle the transparancy of the current frame."
  (interactive)
  (let ((alpha (frame-parameter nil 'alpha)))
    (if (and alpha (not (eq 100 alpha)))
	(set-frame-parameter nil 'alpha 100)
      (set-frame-parameter nil 'alpha 60))))
(define-key global-map (kbd "C-`") 'toggle-transparent)

;; web programming
(choose-mode 'web-mode '(css htm html json jsx php xml))
(mode-company 'web-mode-hook 'company-web-html)
(mode-company 'web-mode-hook 'company-web-jade)
(mode-company 'web-mode-hook 'company-web-slim)

;; pkgbuild
(add-to-list 'auto-mode-alist '("PKGBUILD" . sh-mode) t)

;; whitespace in programs
;(define-minor-mode whitespace-cleanup-mode nil nil nil nil
;  (add-hook 'before-save-hook 'whitespace-cleanup nil t))
;(add-hook 'prog-mode-hook 'whitespace-cleanup-mode)

;; plain text
(setq-default flyspell-use-meta-tab nil)
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'text-mode-hook 'auto-fill-mode)
(add-to-list 'auto-mode-alist '("README" . text-mode) t)

;; go
(mode-company 'go-mode-hook 'company-go)
(add-hook 'go-mode-hook
	  (lambda () (add-hook 'before-save-hook 'gofmt-before-save nil t)))
(setq-default gofmt-show-errors nil)
(setq-default gofmt-command "goimports")

;; c
(mode-company 'c-mode-hook 'company-c-headers)

;; company
(company-auctex-init)
(company-quickhelp-mode)
(global-company-mode)

;; misc
(editorconfig-mode)
(global-auto-revert-mode)
(icomplete-mode)
(ido-mode)
(save-place-mode)
(show-paren-mode)
(windmove-default-keybindings)
(yas-global-mode)
(add-hook 'before-save-hook 'time-stamp)
(setq password-cache-expiry 300)
(setq send-mail-function 'sendmail-send-it)

;; all edits in current emacs process
(require 'server)
(unless (daemonp)
  (setq server-name (format "server-%s" (emacs-pid)))
  (add-hook 'after-init-hook 'server-start))
(setenv "EMACS_SERVER" server-name)
(setenv "EDITOR" (format "emacsclient -s %s" server-name))
(setenv "VISUAL" (getenv "EDITOR"))
(setenv "TEXEDIT" (format "emacsclient -s %s +%%d %%s" server-name))
(define-key global-map (kbd "C-x C-z") 'server-edit)

(provide '.emacs)
;;; .emacs ends here
