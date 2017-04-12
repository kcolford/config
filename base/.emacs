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
    (yasnippit magit haskell-mode company-auctex company-ghc company-go company-jedi csv-mode auto-package-update company company-c-headers company-quickhelp company-shell company-web hc-zenburn-theme sass-mode dockerfile-mode android-mode flycheck go-mode pkgbuild-mode ggtags editorconfig yaml-mode web-mode systemd ssh-config-mode nginx-mode markdown-mode gitignore-mode gitconfig-mode))))
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
(column-number-mode)
(company-quickhelp-mode)
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
(choose-mode 'c++-mode '(h))
(add-to-list 'auto-mode-alist '("README" . text-mode))

;; setup mode hooks
(add-hook 'prog-mode-hook 'flycheck-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)
(add-hook 'prog-mode-hook 'yas-minor-mode)
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'python-mode-hook (lambda () (add-to-list 'company-backends 'company-jedi)))
(add-hook 'after-init-hook 'auto-package-update-maybe)

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
