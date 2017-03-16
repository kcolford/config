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
    (hc-zenburn-theme sass-mode dockerfile-mode magit android-mode flycheck go-mode pkgbuild-mode ggtags editorconfig yaml-mode web-mode systemd ssh-config-mode nginx-mode markdown-mode gitignore-mode gitconfig-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; make flycheck happy
(require 'package)
(require 'server)
(require 'term/xterm)

;; make installing packages easier
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
(package-install-selected-packages)
(package-autoremove)
(defun package-update ()
  "Update the full set of Emacs packages."
  (save-window-excursion
    (list-packages)
    (package-menu-mark-upgrades)
    (package-menu-execute 'no-query)))

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
(editorconfig-mode)
(global-auto-revert-mode)
(ido-mode)
(save-place-mode)
(show-paren-mode)
(windmove-default-keybindings)

;; interactive commands
(defun clean-end-emacs ()
  "Cleanly terminate emacs/finish editing a server buffer."
  (interactive)
  (if server-clients
      (if (server-edit) (delete-frame))
    (save-buffers-kill-terminal)))
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
(defvar colors-initialized nil)
(defun terminal-init-st-256color ()
  "Initialize terminal colors just once."
  (unless colors-initialized
    (setq colors-initialized t)
    (terminal-init-xterm)))

;; better keybindings (note that super isn't used by anything in emacs
;; at all)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x C-c") 'clean-end-emacs)
(global-set-key (kbd "s-b") 'eterm)
(global-set-key (kbd "s-n") 'make-frame-command)
(global-set-key (kbd "s-u") 'eshell)
(global-set-key (kbd "s-y") 'toggle-transparent)
(global-set-key (kbd "s-j") 'windmove-left)
(global-set-key (kbd "s-k") 'windmove-down)
(global-set-key (kbd "s-i") 'windmove-up)
(global-set-key (kbd "s-l") 'windmove-right)
(global-set-key (kbd "s-h") 'split-window-horizontally)
(global-set-key (kbd "s-v") 'split-window-vertically)
(global-set-key (kbd "s-;") 'kill-buffer-and-window)

;; mode settings
(defun choose-mode (mode ext)
  "Assign MODE to be used for each extention listed in EXT."
  (when ext
    (add-to-list 'auto-mode-alist (cons (format "\\.%s\\'" (car ext)) mode))
    (choose-mode mode (cdr ext))))
(choose-mode 'web-mode '(css htm html js json jsx php xml))
(add-hook 'elisp-mode 'flycheck-mode)
(add-hook 'sh-mode 'flycheck-mode)

;; if we're not called to start a daemon, start our own daemon
(unless (daemonp)
  (setq-default server-name (format "server-%s" (random)))
  (server-start))
(setenv "EMACS_SERVER_NAME" server-name)

(provide '.emacs)
;;; .emacs ends here
