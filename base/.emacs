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
    (anaconda-mode company-anaconda company-dict kivy-mode yasnippit magit haskell-mode company-auctex company-ghc company-go company-jedi csv-mode auto-package-update company company-c-headers company-quickhelp company-shell company-web hc-zenburn-theme sass-mode dockerfile-mode android-mode flycheck go-mode pkgbuild-mode ggtags editorconfig yaml-mode web-mode systemd ssh-config-mode nginx-mode markdown-mode gitignore-mode gitconfig-mode auctex))))
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
(add-hook 'after-init-hook 'auto-package-update-maybe)

;; minimal UI
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
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; terminal usage
(defun eterm ()
  "Start an Emacs based terminal emulator."
  (interactive)
  (ansi-term (getenv "SHELL")))
(global-set-key (kbd "s-b") 'eterm)
(add-hook 'term-mode-hook (lambda () (local-set-key (kbd "s-y") 'term-paste)))

;; my shell
(defvar mshell-index 0 "Index of MShell.")
(defun mshell ()
  "Start a new MShell session.
MShell is just like EShell but better."
  (interactive)
  (eshell (setq mshell-index (+ 1 mshell-index))))
(global-set-key (kbd "s-m") 'mshell)

;; some commands
(defun ssh (dest)
  "SSH into DEST."
  (interactive)
  (dired (format "/ssh:%s:" dest)))

;; transparency
(defun toggle-transparent ()
  "Toggle the transparancy of the current frame."
  (interactive)
  (let ((alpha (frame-parameter nil 'alpha)))
    (if (and alpha (not (eq 100 alpha)))
	(set-frame-parameter nil 'alpha 100)
      (set-frame-parameter nil 'alpha 60))))
(global-set-key (kbd "s-u") 'toggle-transparent)

;; management of multiple windows/buffers
(windmove-default-keybindings)
(global-set-key (kbd "s-j") 'windmove-left)
(global-set-key (kbd "s-k") 'windmove-down)
(global-set-key (kbd "s-i") 'windmove-up)
(global-set-key (kbd "s-l") 'windmove-right)
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
(add-to-list 'auto-mode-alist '("README" . text-mode) t)
(define-minor-mode whitespace-cleanup-mode nil nil nil nil
  (add-hook 'before-save-hook 'whitespace-cleanup nil t))
(add-hook 'prog-mode-hook 'whitespace-cleanup-mode)
(add-hook 'prog-mode-hook 'flycheck-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)
(add-hook 'prog-mode-hook 'yas-minor-mode)
(add-hook 'prog-mode-hook 'company-mode)
(add-hook 'after-init-hook 'editorconfig-mode)
(add-hook 'company-mode-hook 'company-quickhelp-local-mode)
(add-hook 'python-mode-hook 'anaconda-mode)
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'after-init-hook 'android-mode)
(add-hook 'after-init-hook 'column-number-mode)
(add-hook 'after-init-hook 'global-auto-revert-mode)
(add-hook 'after-init-hook 'ido-mode)
(add-hook 'after-init-hook 'save-place-mode)
(add-hook 'after-init-hook 'show-paren-mode)

;; company setup
(require 'company)
(add-to-list 'company-backends 'company-dict t)

;; setup current Emacs as editor
(require 'server)
(unless (daemonp)
  (setenv "EMACS_SERVER" (format "server-%s" (emacs-pid)))
  (add-hook 'after-init-hook 'server-start))
(setq server-name (getenv "EMACS_SERVER"))
(global-set-key (kbd "C-x C-z") 'server-edit)

(provide '.emacs)
;;; .emacs ends here
