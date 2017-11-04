(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (ag cmake-mode clang-format ggtags company-jedi esh-help projectile buffer-move edit-indirect ein ein-mumamo elpy json-mode py-yapf magit docker-compose-mode dockerfile-mode systemd ox-gfm go-snippets company-auctex company-c-headers company-dict company-shell company-web company-go yasnippet haskell-mode csv-mode company hc-zenburn-theme android-mode go-mode editorconfig yaml-mode web-mode ssh-config-mode nginx-mode markdown-mode gitignore-mode gitconfig-mode auctex))))
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

(defmacro define-save-minor-mode (fn)
  (let ((fnmode (intern (format "%s-mode" fn))))
    `(define-minor-mode ,fnmode
       (add-hook 'before-save-hook (lambda () (if ,fnmode (,fn))) nil t))))

;; appearance
(setq inhibit-startup-screen t)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(tool-bar-mode 0)
(load-theme 'hc-zenburn t)
(set-frame-font "xos4 Terminus:pixelsize=14")

;; Directory navigation
(setq dired-listing-switches "-lha")
(add-hook 'dired-mode-hook (lambda () (require 'dired-aux)))
(add-hook 'dired-mode-hook (lambda () (require 'dired-x)))
(add-hook 'dired-mode-hook (lambda () (local-set-key (kbd "b") 'browse-url-of-dired-file)))
(add-hook 'dired-mode-hook (lambda () (if (string-equal dired-directory "~/")
					  (dired-sort-other "-lh"))))

;; navigation
(add-hook 'after-init-hook 'windmove-default-keybindings)
(global-set-key (kbd "C-\\") 'bury-buffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(add-hook 'after-init-hook 'winner-mode)

;; transparent frame
(defun toggle-transparent ()
  "Toggle the transparancy of the current frame."
  (interactive)
  (let ((alpha (frame-parameter nil 'alpha)))
    (if (and alpha (not (eq 100 alpha)))
	(set-frame-parameter nil 'alpha 100)
      (set-frame-parameter nil 'alpha 60))))
(global-set-key (kbd "C-`") 'toggle-transparent)

;; plain text
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'text-mode-hook 'auto-fill-mode)
(add-to-list 'auto-mode-alist '("README" . text-mode) t)

;; go
(define-save-minor-mode gofmt-before-save)
(setq gofmt-show-errors nil)
(setq gofmt-command "goimports")
(add-hook 'go-mode-hook 'gofmt-before-save-mode)

;; python
(define-save-minor-mode elpy-format-code)
(define-save-minor-mode elpy-importmagic-fixup)
(setq elpy-rpc-timeout 10)
(add-hook 'after-init-hook 'elpy-enable)
(add-hook 'elpy-mode-hook 'elpy-use-ipython)
(add-hook 'elpy-mode-hook 'elpy-format-code-mode)
(add-hook 'elpy-mode-hook 'elpy-importmagic-fixup-mode)

;; company
(add-hook 'after-init-hook 'global-company-mode)
(with-eval-after-load 'company
  (with-eval-after-load 'auctex
    (company-auctex-init))
  (add-to-list 'company-backends '(company-c-headers company-clang))
  (add-to-list 'company-backends 'company-go)
  (add-to-list 'company-backends 'company-web-html))

;; C/C++
(define-save-minor-mode clang-format-buffer)
(setq company-clang-arguments '("-std=c++11"))
(add-hook 'c-mode-common-hook 'clang-format-buffer-mode)
(add-hook 'c-mode-common-hook 'cwarn-mode)

;; generic programing
(require 'generic-x)
(define-save-minor-mode copyright-update)
(define-save-minor-mode whitespace-cleanup)
(define-save-minor-mode time-stamp)
(setq Man-notify-method 'pushy)
(setq erc-prompt-for-password nil)
(setq org-export-backends '(ascii html latex md org))
(setq password-cache-expiry 300)
(setq vc-follow-symlinks t)
(add-to-list 'auto-mode-alist '("\\.\\(css|htm|html|jsx|php|xml\\)\\'" . web-mode))
(add-hook 'after-init-hook 'editorconfig-mode)
(add-hook 'after-init-hook 'global-auto-revert-mode)
(add-hook 'after-init-hook 'global-eldoc-mode)
(add-hook 'after-init-hook 'icomplete-mode)
(add-hook 'after-init-hook 'ido-mode)
(add-hook 'after-init-hook 'save-place-mode)
(add-hook 'after-init-hook 'show-paren-mode)
(add-hook 'after-init-hook 'yas-global-mode)

(setq projectile-mode-line " Projectile")
(add-hook 'after-init-hook 'projectile-mode)

;; tramp
(setq tramp-default-method "ssh")
(setq auto-revert-remote-files t)
(setq enable-remote-dir-locals t)

;; all edits in current emacs process
(require 'server)
(unless (daemonp)
  (setq server-name (format "server-%s" (emacs-pid)))
  (add-hook 'after-init-hook 'server-start))
(setenv "EDITOR" (format "emacsclient -s %s" server-name))
(setenv "TEXEDIT" (format "emacsclient -s %s +%%d %%s" server-name))
(setenv "PAGER" "cat")
(define-key global-map (kbd "C-x C-z") 'server-edit)
