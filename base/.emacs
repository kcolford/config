(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (py-yapf python-x company-jedi magit docker-compose-mode dockerfile-mode systemd pydoc ido-yes-or-no ox-gfm go-snippets company-auctex company-c-headers company-dict company-quickhelp company-shell company-web company-go yasnippet haskell-mode csv-mode company hc-zenburn-theme android-mode go-mode editorconfig yaml-mode web-mode ssh-config-mode nginx-mode markdown-mode gitignore-mode gitconfig-mode auctex))))
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

(defmacro run-in-async-process (name &rest body)
  "Run BODY in an asynchronous process separate from the current emacs."
  (declare (indent 1))
  `(let ((envname (upcase (concat "emacs_async_" ,name))))
     (when (getenv envname)
       ,@body
       (kill-emacs))
     (with-interactive
       (let ((buffer (get-buffer-create (concat "*" ,name "*"))))
	 (with-current-buffer buffer
	   (view-mode))
	 (setenv envname "y")
	 (start-process (capitalize ,name) buffer
			"emacs" "--batch" "--load" user-init-file)
	 (setenv envname)))))

(defmacro with-interactive (&rest body)
  "Run BODY only when in an interactive environment."
  (declare (indent 0))
  `(unless noninteractive
     ,@body))

;; update packages
(run-in-async-process "update"
  (package-refresh-contents)
  (mapc (lambda (pkg) (package-install pkg)) package-selected-packages)
  (package--mapc (lambda (pkg) (unless (or (not (package-installed-p pkg))
					   (package--newest-p pkg))
				 (package-reinstall pkg))))
  (mapc (lambda (pkg) (package-delete (car (alist-get pkg package-alist))))
	(package--removable-packages)))

;; company
(with-interactive
  (global-company-mode)
  (company-auctex-init)
  (company-quickhelp-mode))
(require 'company)

;; appearance
(setq inhibit-startup-screen t)
(with-interactive
  ;;(menu-bar-mode 0)
  (scroll-bar-mode 0)
  (tool-bar-mode 0)
  (load-theme 'hc-zenburn t)
  (set-frame-font "-xos4-xos4 Terminus-normal-normal-normal-*-14-*-*-*-c-80-iso10646-1"))

;; Directory navigation
(with-eval-after-load 'dired
  (require 'dired-aux)
  (require 'dired-x)
  (define-key dired-mode-map (kbd "b") 'browse-url-of-dired-file))
(setq dired-listing-switches "-lha")
(defun dired-hide-dotfiles-in-home ()
  "Hook to hide dot files when looking at the home directory."
  (if (string-equal dired-directory "~/")
      (dired-sort-other "-lh")))
(add-hook 'dired-mode-hook 'dired-hide-dotfiles-in-home)
(define-key global-map (kbd "C-x C-d") 'dired)

;; version control
(define-key global-map (kbd "C-x v s") 'vc-git-grep)
(with-eval-after-load 'vc-dir
  (define-key vc-dir-mode-map (kbd "s") 'vc-git-grep))
(setq vc-follow-symlinks t)
(global-set-key (kbd "C-x g") 'magit-status)

;; don't pop up a new window all the time
(setq display-buffer-alist '((".*" display-buffer-same-window (nil))))
(setq Man-notify-method 'pushy)

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
(global-set-key (kbd "C-`") 'toggle-transparent)

;; web programming
(choose-mode 'web-mode '(css htm html json jsx php xml))
(add-to-list 'company-backends 'company-web-html)

;; whitespace in programs
;(define-minor-mode whitespace-cleanup-mode nil nil nil nil
;  (add-hook 'before-save-hook 'whitespace-cleanup nil t))
;(add-hook 'prog-mode-hook 'whitespace-cleanup-mode)

;; plain text
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'text-mode-hook 'auto-fill-mode)
(add-to-list 'auto-mode-alist '("README" . text-mode) t)

;; go
(add-to-list 'company-backends 'company-go)
(add-hook 'go-mode-hook
	  (lambda () (add-hook 'before-save-hook 'gofmt-before-save nil t)))
(setq gofmt-show-errors nil)
(setq gofmt-command "goimports")

;; email
(add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e")
(if noninteractive
    (autoload 'mu4e "mu4e")
  (require 'mu4e))
(setq
 ;; sending mail
 message-send-mail-function 'smtpmail-send-it
 smtpmail-smtp-server "smtp.gmail.com"
 smtpmail-smtp-service "submission"
 smtpmail-stream-type 'starttls
 imap-server "imap.gmail.com"
 ;; recieving mail
 mu4e-maildir "~/mail"
 ;; mbsync
 mu4e-get-mail-command "mbsync default"
 mu4e-change-filenames-when-moving t
 mu4e-update-interval 300
 ;; handled by gmail
 mu4e-sent-messages-behavior 'delete
 ;; viewing preferences
 mu4e-view-html-plaintext-ratio-heuristic most-positive-fixnum
 mu4e-view-show-addresses t
 mu4e-headers-skip-duplicates t
 ;; emacs wide settings
 mail-user-agent 'mu4e-user-agent
 read-mail-command 'mu4e)
(with-eval-after-load 'mu4e
  (define-key mu4e-view-mode-map (kbd "<backspace>") 'scroll-down-command)
  (define-key mu4e-view-mode-map (kbd "SPC") 'scroll-up-command))
(global-set-key (kbd "C-=") 'mu4e)
(defun email-user ()
  "Print the user name for my email account."
  (email-password ':user))
(defun email-password (&optional info)
  "Print the password for my email account."
  (require 'auth-source)
  (let* ((info (or info ':secret))
	 (acc (auth-source-search :host imap-server :require `(,info)))
	 (pass (plist-get (nth 0 acc) info))
	 (pass (if (functionp pass) (funcall pass) pass)))
    (princ pass)
    (princ "\n")))

;; python
(add-to-list 'company-backends 'company-jedi)
(with-eval-after-load 'pydoc
  (define-key pydoc-mode-map (kbd "m") 'pydoc))
(with-eval-after-load 'python
  (python-x-setup)
  (define-key inferior-python-mode-map (kbd "C-c C-z")
    (lambda () (interactive) (switch-to-buffer python-shell--parent-buffer))))
(add-hook 'python-mode-hook 'py-yapf-enable-on-save)
(setq gud-pdb-command-name "python -m pdb")


;; misc
(with-interactive
  (require 'generic-x)
  ;;(editorconfig-mode)
  (display-battery-mode)
  (global-auto-revert-mode)
  (global-eldoc-mode)
  (icomplete-mode)
  (ido-mode)
  (save-place-mode)
  (show-paren-mode)
  (windmove-default-keybindings)
  (yas-global-mode))
(add-hook 'before-save-hook 'time-stamp)
(add-to-list 'auto-mode-alist '("PKGBUILD" . sh-mode) t)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x C-o") 'other-window)
(setq erc-prompt-for-password nil)
(setq password-cache-expiry 300)
(setq view-read-only t)
(setq org-export-backends '(ascii html latex md org))
(add-to-list 'company-backends 'company-c-headers)
(with-eval-after-load 'term
  (term-set-escape-char ?\C-x))

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
