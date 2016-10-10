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
(add-to-list 'default-frame-alist '(alpha . (85 . 50)))
(global-set-key "\C-x\C-b" 'electric-buffer-list)

;; set up server for subjobs
(setq server-name (format "autoserver-%d" (random)))
(server-start)
(setenv "EDITOR" (format "emacsclient -s %s" server-name))
