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
(add-to-list 'package-archives '("elpy" . "https://jorgenschaefer.github.io/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; initscript helpers
(defun choose-mode (mode ext)
  "Assign MODE to be used for each extention listed in EXT."
  (when (consp ext)
    (add-to-list 'auto-mode-alist (cons (format "\\.%s\\'" (car ext)) mode))
    (choose-mode mode (cdr ext))))

(defmacro with-interactive (&rest body)
  "Run BODY only when in an interactive environment."
  (declare (indent 0))
  `(unless noninteractive
     ,@body))

(defmacro define-save-minor-mode (fn)
  (let ((fnmode (intern (format "%s-mode" fn))))
    `(define-minor-mode ,fnmode
       (add-hook 'before-save-hook (lambda () (if ,fnmode (,fn))) nil t))))

;; appearance
(setq inhibit-startup-screen t)
(with-interactive
  ;; (menu-bar-mode 0)
  (scroll-bar-mode 0)
  (tool-bar-mode 0)
  (load-theme 'hc-zenburn t)
  (set-frame-font "-xos4-xos4 Terminus-normal-normal-normal-*-14-*-*-*-c-80-iso10646-1"))

;; Directory navigation
(setq dired-listing-switches "-lha")
(add-hook 'dired-mode-hook (lambda () (require 'dired-aux)))
(add-hook 'dired-mode-hook (lambda () (require 'dired-x)))
(add-hook 'dired-mode-hook (lambda () (local-set-key (kbd "b") 'browse-url-of-dired-file)))
(add-hook 'dired-mode-hook (lambda () (if (string-equal dired-directory "~/")
					  (dired-sort-other "-lh"))))

;; version control
(setq vc-follow-symlinks t)
(global-set-key (kbd "C-x g") 'magit-status)

;; don't pop up a new window all the time
;; (setq display-buffer-alist '((".*" display-buffer-same-window (nil))))
(setq Man-notify-method 'pushy)

;; navigation
(windmove-default-keybindings)
(global-set-key (kbd "C-\\") 'bury-buffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(winner-mode)

;; transparent frame
(defun toggle-transparent ()
  "Toggle the transparancy of the current frame."
  (interactive)
  (let ((alpha (frame-parameter nil 'alpha)))
    (if (and alpha (not (eq 100 alpha)))
	(set-frame-parameter nil 'alpha 100)
      (set-frame-parameter nil 'alpha 60))))
(global-set-key (kbd "C-`") 'toggle-transparent)

;; ;; whitespace in programs
;; (define-minor-mode whitespace-cleanup-mode nil nil nil nil
;;   (add-hook 'before-save-hook 'whitespace-cleanup nil t))
;; (add-hook 'prog-mode-hook 'whitespace-cleanup-mode)

;; plain text
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'text-mode-hook 'auto-fill-mode)
(add-to-list 'auto-mode-alist '("README" . text-mode) t)

;; go
(define-save-minor-mode gofmt-before-save)
(add-hook 'go-mode-hook 'gofmt-before-save-mode)
(setq gofmt-show-errors nil)
(setq gofmt-command "goimports")

;; email
(add-to-list 'auto-mode-alist '("/mutt" . mail-mode))


;; (add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e")
;; (if noninteractive
;;     (autoload 'mu4e "mu4e")
;;   (require 'mu4e))
;; (setq
;;  ;; sending mail
;;  message-send-mail-function 'smtpmail-send-it
;;  smtpmail-smtp-server "smtp.gmail.com"
;;  smtpmail-smtp-service "submission"
;;  smtpmail-stream-type 'starttls
;;  imap-server "imap.gmail.com"
;;  ;; recieving mail
;;  mu4e-maildir "~/mail"
;;  ;; mbsync
;;  mu4e-get-mail-command "mbsync default"
;;  mu4e-change-filenames-when-moving t
;;  mu4e-update-interval 300
;;  ;; handled by gmail
;;  mu4e-sent-messages-behavior 'delete
;;  ;; viewing preferences
;;  mu4e-view-html-plaintext-ratio-heuristic most-positive-fixnum
;;  mu4e-view-show-addresses t
;;  mu4e-headers-skip-duplicates t
;;  ;; emacs wide settings
;;  mail-user-agent 'mu4e-user-agent
;;  read-mail-command 'mu4e)
;; (add-hook 'mu4e-view-mode-hook (lambda () (local-set-key (kbd "<backspace>") 'scroll-down-command)))
;; (add-hook 'mu4e-view-mode-hook (lambda () (local-set-key (kbd "SPC") 'scroll-up-command)))
;; (defun email-user ()
;;   "Print the user name for my email account."
;;   (email-password ':user))
;; (defun email-password (&optional info)
;;   "Print the password for my email account."
;;   (require 'auth-source)
;;   (let* ((info (or info ':secret))
;; 	 (acc (auth-source-search :host imap-server :require `(,info)))
;; 	 (pass (plist-get (nth 0 acc) info))
;; 	 (pass (if (functionp pass) (funcall pass) pass)))
;;     (princ pass)
;;     (princ "\n")))
;; (defalias 'email 'mu4e)

;; rfc
(define-derived-mode rfc-mode view-mode "RFC")
(add-to-list 'auto-mode-alist '("/rfc[0-9]+\\.txt\\'" . rfc-mode))

;; python
(setq elpy-dedicated-shells t
      elpy-rpc-timeout 10)
(with-interactive
  (elpy-enable))
(define-save-minor-mode elpy-format-code)
(add-hook 'elpy-mode-hook 'elpy-format-code-mode)
(define-save-minor-mode elpy-importmagic-fixup)
(add-hook 'elpy-mode-hook 'elpy-importmagic-fixup-mode)

;; shells
(with-eval-after-load 'term
  (term-set-escape-char ?\C-x))
(add-hook 'term-mode-hook (lambda () (local-set-key (kbd "C-V") 'term-paste)))

;; company
(with-interactive
  (global-company-mode))
(with-eval-after-load 'company
  (with-eval-after-load 'auctex
    (company-auctex-init))
  (add-to-list 'company-backends '(company-c-headers company-clang))
  (add-to-list 'company-backends 'company-go)
  (add-to-list 'company-backends 'company-web-html))

;; C/C++
(define-save-minor-mode clang-format-buffer)
(add-hook 'c-mode-common-hook 'clang-format-buffer-mode)
;; (add-hook 'c-mode-common-hook 'ggtags-mode)
(add-hook 'c-mode-common-hook 'cwarn-mode)
(setq company-clang-arguments '("-std=c++11"))

;; generic programing
(define-save-minor-mode copyright-update)
(add-hook 'prog-mode-hook 'copyright-update-mode)
(add-hook 'before-save-hook 'time-stamp)

;; web
(add-to-list 'auto-mode-alist '("\\.\\(css|htm|html|jsx|php|xml\\)\\'" . web-mode))

;; additional modes
(with-interactive
  (require 'generic-x))

;; battery 
(add-hook 'after-init-hook 'display-battery-mode)

;; editor config
(add-hook 'after-init-hook 'editorconfig-mode)

(add-hook 'after-init-hook 'global-auto-revert-mode)

(add-hook 'after-init-hook 'global-eldoc-mode)

(add-hook 'after-init-hook 'icomplete-mode)

(add-hook 'after-init-hook 'ido-mode)

(add-hook 'after-init-hook 'projectile-mode)
(setq projectile-mode-line " Projectile")

(add-hook 'after-init-hook 'save-place-mode)

(add-hook 'after-init-hook 'show-paren-mode)

(add-hook 'after-init-hook 'yas-global-mode)

(add-to-list 'auto-mode-alist '("PKGBUILD" . sh-mode) t)

(setq erc-prompt-for-password nil)

(setq org-export-backends '(ascii html latex md org))

(setq password-cache-expiry 300)

;; tramp
(setq tramp-default-method "ssh"
      auto-revert-remote-files t
      enable-remote-dir-locals t)

;; all edits in current emacs process
(with-interactive
  (require 'server)
  (unless (daemonp)
    (setq server-name (format "server-%s" (emacs-pid)))
    (add-hook 'after-init-hook 'server-start))
  (setenv "EMACS_SERVER" server-name)
  (setenv "EDITOR" (format "emacsclient -s %s" server-name))
  (setenv "VISUAL" (getenv "EDITOR"))
  (setenv "TEXEDIT" (format "emacsclient -s %s +%%d %%s" server-name))
  (setenv "PAGER" "cat")
  (define-key global-map (kbd "C-x C-z") 'server-edit))
