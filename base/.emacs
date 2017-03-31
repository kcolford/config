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
    (csv-mode auth-password-store password-store auto-package-update company company-c-headers company-quickhelp company-shell company-web ac-c-headers auto-complete hc-zenburn-theme sass-mode dockerfile-mode magit android-mode flycheck go-mode pkgbuild-mode ggtags editorconfig yaml-mode web-mode systemd ssh-config-mode nginx-mode markdown-mode gitignore-mode gitconfig-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; make installing packages easier
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
(package-install-selected-packages)
(package-autoremove)
(require 'auto-package-update)
(auto-package-update-maybe)

;; keep stuff out of the way
(setq inhibit-startup-screen t)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(tool-bar-mode 0)

;; better usability
(load-theme 'hc-zenburn t)
(setq default-frame-alist '((height . 24) (width . 80)))
(setq display-buffer-alist '((".*" display-buffer-same-window (nil))))
(setq password-cache-expiry 300)
(setq vc-follow-symlinks t)
(setq view-read-only t)
(setq-default Man-notify-method 'pushy)
(setq-default tramp-default-method "ssh")

;; minor modes
(android-mode)
(auto-complete-mode)
(column-number-mode)
(editorconfig-mode)
(global-auto-revert-mode)
(global-company-mode)
(ido-mode)
(save-place-mode)
(show-paren-mode)

;; interactive commands
(defun eterm ()
  "Start an Emacs based terminal emulator."
  (interactive)
  (ansi-term (getenv "SHELL")))
(defun toggle-transparent ()
  "Toggle the transparancy of the current frame."
  (interactive)
  (let ((alpha (frame-parameter nil 'alpha)))
    (if (and alpha (not (eq 100 alpha)))
	(set-frame-parameter nil 'alpha 100)
      (set-frame-parameter nil 'alpha 60))))

;; better keybindings (note that super isn't used by anything in emacs
;; at all)
(global-set-key (kbd "s-b") 'eterm)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "s-y") 'term-paste)
(global-set-key (kbd "s-u") 'toggle-transparent)
;; the following bindings are for managing many buffers using a single
;; emacs frame
(global-set-key (kbd "s-j") 'windmove-left)
(global-set-key (kbd "s-k") 'windmove-down)
(global-set-key (kbd "s-i") 'windmove-up)
(global-set-key (kbd "s-l") 'windmove-right)

;; these are usually safe from desktop managers
(global-set-key (kbd "s--") 'split-window-horizontally)
(global-set-key (kbd "s-=") 'split-window-vertically)
(global-set-key (kbd "s-\\") 'kill-buffer-and-window)

;; mode settings
(defun choose-mode (mode ext)
  "Assign MODE to be used for each extention listed in EXT."
  (when ext
    (add-to-list 'auto-mode-alist (cons (format "\\.%s\\'" (car ext)) mode))
    (choose-mode mode (cdr ext))))
(choose-mode 'web-mode '(css htm html json jsx php xml))
(add-to-list 'auto-mode-alist '("README" . text-mode))

;; setup mode hooks
(add-hook 'prog-mode-hook 'flycheck-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)
(add-hook 'text-mode-hook 'flyspell-mode)

;; setup current emacs as editor
(require 'server)
(unless (daemonp)
  (setq-default server-name (format "server-%s" (emacs-pid)))
  (server-start))
(setenv "EDITOR" (format "emacsclient -s %s" server-name))
(defun clean-end-emacs ()
  "Cleanly terminate Emacs."
  (interactive)
  (if server-clients
      (if (server-edit)
	  (delete-frame))
    (save-buffers-kill-terminal)))
(global-set-key (kbd "C-x C-c") 'clean-end-emacs)

(provide '.emacs)
;;; .emacs ends here
