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
    (anaconda-mode company-anaconda company-dict kivy-mode yasnippit magit haskell-mode company-auctex company-ghc company-go company-jedi csv-mode auto-package-update company company-c-headers company-quickhelp company-shell company-web hc-zenburn-theme sass-mode dockerfile-mode android-mode flycheck go-mode pkgbuild-mode ggtags editorconfig yaml-mode web-mode systemd ssh-config-mode nginx-mode markdown-mode gitignore-mode gitconfig-mode auctex)))
 '(send-mail-function (quote sendmail-send-it)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "xos4 Terminus" :foundry "xos4" :slant normal :weight normal :height 105 :width normal)))))

;; force load some stuff
(require 'dired-aux)
(require 'dired-x)
(require 'package)
(require 'server)

;; make installing packages easier
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(defun heavy-setup ()
  "Heavy setup to only be done sometimes."
  (interactive)
  (package-refresh-contents)
  (package-install-selected-packages)
  (package-autoremove)
  (save-excursion
    (list-packages)
    (package-menu-mark-upgrades)
    (package-menu-execute)))

;; minimal UI
(setq inhibit-startup-screen t)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(tool-bar-mode 0)

;; better usability
(load-theme 'hc-zenburn t)
(setq dired-listing-switches "-la")
(setq display-buffer-alist '((".*" display-buffer-same-window (nil))))
(setq password-cache-expiry 300)
(setq vc-follow-symlinks t)
(setq-default Man-notify-method 'pushy)

;; transparency
(defun toggle-transparent ()
  "Toggle the transparancy of the current frame."
  (interactive)
  (let ((alpha (frame-parameter nil 'alpha)))
    (if (and alpha (not (eq 100 alpha)))
	(set-frame-parameter nil 'alpha 100)
      (set-frame-parameter nil 'alpha 60))))

;; mode settings
(defun choose-mode (mode ext)
  "Assign MODE to be used for each extention listed in EXT."
  (when ext
    (add-to-list 'auto-mode-alist (cons (format "\\.%s\\'" (car ext)) mode))
    (choose-mode mode (cdr ext))))
(choose-mode 'web-mode '(css htm html json jsx php xml))
(choose-mode 'c++-mode '(h))
(add-to-list 'auto-mode-alist '("README" . text-mode) t)
(add-hook 'before-save-hook 'time-stamp)
(define-minor-mode whitespace-cleanup-mode nil nil nil nil
  (add-hook 'before-save-hook 'whitespace-cleanup nil t))
(add-hook 'prog-mode-hook 'whitespace-cleanup-mode)
;(add-hook 'prog-mode-hook 'editorconfig-mode)
;(add-hook 'prog-mode-hook 'flycheck-mode)
;(add-hook 'prog-mode-hook 'flyspell-prog-mode)
(add-hook 'prog-mode-hook 'yas-minor-mode)
(add-hook 'text-mode-hook 'flyspell-mode)
(column-number-mode)
(display-time-mode)
(display-battery-mode)
(global-auto-revert-mode)
(ido-mode)
(save-place-mode)
(show-paren-mode)
(windmove-default-keybindings)

;; setup current Emacs as editor
(unless (daemonp)
  (setq server-name (format "server-%s" (emacs-pid)))
  (add-hook 'after-init-hook 'server-start))
(setenv "EMACS_SERVER" server-name)
(setenv "EDITOR" (format "emacsclient -s %s" server-name))
(setenv "VISUAL" (getenv "EDITOR"))
(setenv "TEXEDIT" (format "emacsclient -s %s +%%d %%s" server-name))
(global-set-key (kbd "C-x C-z") 'server-edit)

(provide '.emacs)
;;; .emacs ends here
