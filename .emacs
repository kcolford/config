;; personal .emacs file

;; make installing more packages easier
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; nicer interface
(setq inhibit-startup-screen t)
(setq view-read-only t)
(show-paren-mode)
(global-auto-revert-mode)
(windmove-default-keybindings)
(tool-bar-mode 0)
(load-theme 'hc-zenburn t)
(add-to-list 'default-frame-alist '(alpha . 85))
(global-set-key "\C-x\C-b" 'buffer-menu)
(setq erc-nick "kcolford")

;; set up server for subjobs
(setq server-name (format "autoserver-%d" (random)))
(server-start)
(setenv "EDITOR" (format "emacsclient -s %s" server-name))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (yaml-mode web-mode systemd ssh-config-mode smart-tabs-mode s pkgbuild-mode nginx-mode markdown-mode hc-zenburn-theme gitignore-mode gitconfig-mode bison-mode auctex))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
