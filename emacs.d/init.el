;;; Init.el --- Cusomizations for Emacs
;;; Commentary:
;;; Code:

;;;; UI
;; Change UI early to prevent flickering

(if (display-graphic-p)
    (progn
      (tool-bar-mode -1) ; Hiding toolbar
      (scroll-bar-mode -1) ; Hide scrollbars
      ;(tabbar-mode -1) ; Disable tabbar
	  ))
(if (not (string-equal system-type "darwin"))
	(menu-bar-mode -1))

(setq inhibit-startup-message t
	  initial-scratch-message nil)

; Include column number int mode-line
(setq column-number-mode t
	  line-number-mode t)

;;;; ENVIRONMENT
(add-to-list 'load-path "~/.emacs.d/vendor/")
(add-to-list 'custom-theme-load-path (file-name-as-directory "~/.emacs.d/replace-colorthemes/"))

(if (string-equal system-type "darwin")
	(let
		((path '("/usr/local/bin" "/usr/bin" "/bin"
				 "/usr/local/sbin" "/usr/sbin" "/sbin"
				 "/opt/X11/bin" "/opt/ImageMagick/bin"
				 "/usr/local/MacGPG2/bin" "/usr/texbin")))
	  (progn
		(set 'exec-path (delete-dups (append path exec-path)))
		(setenv "PATH" (mapconcat 'identity path ":")))))

;;;; PACKAGES

;;; Add package repository
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

; Make sure that we have use-package installed
(if (not (package-installed-p 'use-package))
	(progn
	  (package-refresh-contents)
	  (package-install 'use-package)))
(require 'use-package)

(use-package uniquify
  :config (setq-default mode-line-buffer-identification
			  `(:eval
				(let ((s (format-mode-line
						  (propertized-buffer-identification (buffer-name)))))
				  (when (and (boundp 'uniquify-managed) uniquify-managed)
					(unless (string= (buffer-name) (uniquify-buffer-base-name))
					  (let ((base-len (length (uniquify-buffer-base-name)))
							(full-len (length (buffer-name)))
							(pre (eq uniquify-buffer-name-style 'forward)))
						(let ((start (if pre 0 base-len))
							  (end (if pre (- full-len base-len) full-len)))
						  (set-text-properties start end nil s)))))
				  s))))

; Pymacs in ~/.emacs.d/vendor/ (for ropemacs)

;;;;; Adaptive prefix
(use-package adaptive-wrap)

;;;;; Auto complete
(use-package auto-complete
  :config
  (add-to-list 'ac-dictionary-directories "~/.emacs.d/dict")
  (setq ac-use-menu-map t)
  (define-key ac-menu-map "\C-n" 'ac-next)
  (define-key ac-menu-map "\C-p" 'ac-previous))

(with-eval-after-load "auto-complete-config"
  (ac-config-default)
  (when (file-exists-p (expand-file-name "~/.emacs.d/vendor/pymacs.el"))
    (ac-ropemacs-initialize)
	(ac-ropemacs-setup)))

(with-eval-after-load "auto-complete-autoloads"
  (autoload 'auto-complete-mode "auto-complete" "enable auto-complete-mode" t nil)
  (add-hook 'python-mode-hook
	    (lambda ()
	      (require 'auto-complete-config)
	      (add-to-list 'ac-sources 'ac-source-ropemacs)
	      (auto-complete-mode)
	      (require 'nose)
	      ))
  (message "After auto-complete-autoloads")
  )

;;;;; Dash.app

(use-package dash-at-point
  :commands dash-at-point)

;;;;; Diminish

(use-package diminish)
;Perhaps replace with rich-minority https://github.com/Bruce-Connor/rich-minority

(with-eval-after-load "afafafdiminish"
  (with-eval-after-load "adaptive-wrap-prefix"
	(diminish 'adaptive-wrap-prefix-mode))
  (diminish 'hs-minor-mode)
  (with-eval-after-load "projectile"
	(diminish 'projectile-mode "P"))
  (diminish 'undo-tree-mode)
  (with-eval-after-load "ws-trim"
	(diminish 'ws-trim-mode)))

;;;;; Estimation

(use-package estimation
  :mode ("\\.est\\'" . estimation-mode)
  :interpreter ("estimation" . estimation-mode)
  :load-path "estimation"
  :config
  (add-hook 'estimation-mode-hook
	    (lambda () (progn
			 (hs-minor-mode 1)
			 (visual-line-mode 1)
			 (adaptive-wrap-prefix-mode 1)))))

;;;;; Evil

(defun my-move-key (keymap-from keymap-to key)
  "Move key binding for KEY from keymap KEYMAP-FROM to KEYMAP-TO, deleting from the old location."
  (define-key keymap-to key (lookup-key keymap-from key))
  (define-key keymap-from key nil))

;;; esc quits
;(define-key evil-normal-state-map [escape] 'keyboard-quit)
;(define-key evil-visual-state-map [escape] 'keyboard-quit)
;(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
;(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
;(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
;(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
;(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

(defun ex-mode-mapping (cmd)
  (let ((binding (car cmd))
		(fn (cdr cmd)))
	(evil-ex-define-cmd binding fn)))

(use-package evil
  :ensure
  :config
  (evil-mode t)

  (define-key evil-normal-state-map "," nil)
  (define-key evil-normal-state-map "\C-j" 'evil-jump-to-tag)
  (define-key evil-normal-state-map "\C-k" 'evil-jump-backward)

  (define-key evil-normal-state-map "\C-f" nil)
  (define-key evil-normal-state-map "\C-\M-wn" 'new-frame)
  (define-key evil-normal-state-map "\C-\M-wc" 'delete-frame)
  (define-key evil-normal-state-map "\C-\M-wl" 'ns-next-frame)
  (define-key evil-normal-state-map "\C-\M-wh" 'ns-prev-frame)

  (my-move-key evil-motion-state-map evil-normal-state-map (kbd "RET"))
  (my-move-key evil-motion-state-map evil-normal-state-map " ")
  (mapc 'ex-mode-mapping
		'(("gstatus" . magit-status)
		  ("whitespace" . whitespace-mode)
		  ("test" . projectile-test-project)
		  ("dash" . dash-at-point)
		  ("ack" . ack-and-a-half)))

  (use-package evil-rebellion
	:load-path "vendor/evil-rebellion/")

  (use-package evil-leader
	:config
	(global-evil-leader-mode)
	(evil-leader/set-leader ","))

  (use-package evil-surround
	:config
	(global-evil-surround-mode 1)))

;;;;; Flycheck
;;; Enable syntax-checking with flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)

(use-package flycheck)


; (with-eval-after-load "flycheck"
;   (flycheck-define-checker python3-pyflakes
;     "A Python syntax and style checker using the pyflakes utility.
; 
; See URL `http://pypi.python.org/pypi/pyflakes'."
;     :command ("python3 -m pyflakes" source-inplace)
;     :error-patterns
;     ((error line-start (file-name) ":" line ":" (message) line-end))
;     :modes python-mode)
; 
;   (add-to-list 'flycheck-checkers 'python3-pyflakes))

;;;;; Flyspell
(with-eval-after-load "flyspell"
  (setq flyspell-issue-welcome-flag nil
		ispell-program-name "aspell"
		ispell-list-command "list"))

;;;;; Haskell

;; Sane indentation for haskell
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

;;;;; Helm

(defun imenu-elisp-sections ()
  ;(setq imenu-prev-index-position-function nil)
  (add-to-list 'imenu-generic-expression '("Section" "^;;;; \\(.+\\)$" 1) t)
  (add-to-list 'imenu-generic-expression '("Subsection" "^;;;;; \\(.+\\)$" 1) t))
(add-hook 'emacs-lisp-mode-hook 'imenu-elisp-sections)

(use-package helm-config
  :ensure helm
  :config

  (use-package helm-projectile
    :ensure)

  (use-package helm-flycheck
    :ensure)

  (with-eval-after-load "evil"

    (define-key evil-normal-state-map ", f" 'helm-find-files)
    (define-key evil-normal-state-map  ", o" 'helm-imenu)
	(define-key evil-normal-state-map ", b" 'helm-buffers-list)

	(define-key evil-normal-state-map ", c" 'helm-flycheck)

	(with-eval-after-load "lacarte"
	  (define-key evil-normal-state-map ", m" 'helm-browse-menubar))

	(with-eval-after-load "projectile"
	  (define-key evil-normal-state-map ", n" 'helm-projectile)
	  (define-key evil-normal-state-map  ", p" 'helm-projectile-switch-project))
	
	; Fix for updated projectile
	(defalias 'helm-buffers-list--match-fn 'helm-buffer-match-major-mode)))

;;;;; Ido

(ido-mode 1)
(setq ido-everywhere 1)

(with-eval-after-load "ido"
  (setq ido-use-faces nil) ; Use flx hightlits instead of ido faces

  ;; Fuzzy matching for ido with flx-ido
  (use-package flx-ido
    :ensure
    :config
    (flx-ido-mode 1))

  (use-package ido-ubiquitous
    :ensure
    :config
    (ido-ubiquitous-mode 1)) ; Ido everywhere!!!

  ;; Vertical list for ido
  (use-package ido-vertical-mode
    :ensure
    :config
    (ido-vertical-mode 1)))

;;;;; la Carte

; Used by helm to browse menus
(use-package lacarte
  :ensure t)

;;;;; Magit

(use-package magit
  :ensure
  :config
  (setq magit-commit-show-notes t)
  (setq magit-last-seen-setup-instructions "1.4.0")
  (with-eval-after-load "evil"
	(define-key evil-normal-state-map ",gs" 'magit-status)))

;;;;; Markdown-mode

(use-package markdown-mode
  :ensure
  :config
  (add-hook 'markdown-mode-hook
			(lambda ()
			  (visual-line-mode t)
			  (adaptive-wrap-prefix-mode t)
			  (ws-trim-mode nil))))
(use-package markdown-mode+
  :ensure)
(use-package cm-mode
  :ensure)

;;;;; Modeline-posn

(use-package modeline-posn
  :ensure
  :init
  (setq modelinepos-column-limit 70))

;;;;; Neotree

; (with-eval-after-load "neotree"
;   (global-set-key [f2] 'neotree-toggle))

;;;;; Projectile

(use-package projectile
  :ensure
  :config
  (projectile-global-mode t)
  (add-to-list 'projectile-globally-ignored-directories ".ropeproject")
  (add-to-list 'projectile-globally-ignored-directories "virtualenvs")
  (with-eval-after-load "evil"
	(define-key evil-normal-state-map ",ps" 'projectile-switch-project)))

;;;;; Sr Speedbar

(use-package sr-speedbar
  :config
  (global-set-key [f2] 'sr-speedbar-toggle))


;;;; BEHAVIOR
;; Fix option-key
;(setq default-input-method "MacOSX")
(setq mac-option-modifier nil
      mac-command-modifier 'meta
      mac-allow-antialiasing t
      mac-command-key-is-meta t)

(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))

(setq tags-revert-without-query t)

;(global-set-key [f2] 'speedbar)

(setq-default tab-width 4)
(use-package smart-tabs-mode
  :ensure
  :config
  (smart-tabs-insinuate 'c
						'c++))


;;; Enable yasnippets
;(add-hook 'prog-mode-hook #'yas-minor-mode)

(server-start)

;;;; NON-PACKAGES

(use-package ws-trim
  :load-path "vendor"
  :diminish ws-trim-mode
  :config
  (global-ws-trim-mode 1)
  (setq ws-trim-mode 1))

;;;; EXTENSIONS

; Timestamps
(defvar current-date-time-format "%Y-%m-%d %H:%M:%S"
  "Format for date to insert with `insert-current-date-time. See help of `format-time-string' for possible replacements.")

(defun insert-current-date-time ()
  "Insert the current date and time into the current buffer"
  (interactive)
  (insert (format-time-string current-date-time-format (current-time))))

(with-eval-after-load "evil"
  (define-key evil-normal-state-map ",dt" 'insert-current-date-time))

; Dropbox
(define-minor-mode dropbox-mode
  "For files located in dropbox.
Turns off backup creation and auto saving."

  ;; Initial value
  nil

  ;; Mode line indicator
  " Db"

  ;; Minor mode bindings
  nil
  (if (symbol-value dropbox-mode)
	  (progn
		;; Disable backups
		(set (make-local-variable 'backup-inhibited) t)
		;; Disable auto-save
		(if auto-save-default
			(auto-save-mode -1)))
	; Resort to default value of backup-inhibited
	(kill-local-variable 'backup-inhibited)
	; Resort to default auto save setting
	(if auto-save-default
		(auto-save-mode 1))))
(setq auto-mode-alist
	  (append '(("/Dropbox/" . dropbox-mode))
			  auto-mode-alist))

; Worklog
(defun worklog-open-today ()
  "Opens worklog-file for today."
  (interactive)
  (let ((file-name (format-time-string "~/Dropbox/Worklog/%Y/%m %B/%Y-%m-%d.wlog" (current-time))))
	(find-file file-name)
	(end-of-buffer)))

(defun worklog-new-entry ()
  "Adds a new line with a timestamp."
  (interactive)
  (insert (format-time-string (concat "### " current-date-time-format) (current-time))))


; Spelling stuff
(with-eval-after-load "evil"
  (define-key evil-normal-state-map ",ss" 'flyspell-mode)
  (define-key evil-normal-state-map ",sl" 'ispell-change-dictionary))


(defun tabs-enable ()
  "Enable indentation with tabs."
  (interactive)
  (setq indent-tabs-mode t))

(defun tabs-disable ()
  "Disable indentation with tabs."
  (interactive)
  (setq indent-tabs-mode nil))


(defvar marked-name)
(setq marked-name "Marked 2")
(defun markdown-preview-file ()
  "Run Marked on the current file and revert the buffer."
  (interactive)
  (shell-command (format "open -a '%s' '%s'"
						 marked-name
						 (buffer-file-name))))


(defun copy-file-name-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

(with-eval-after-load "projectile"
  (defun copy-project-file-name-to-clipboard ()
	"Copy the current buffer path to the clipboard"
	(interactive)
	(let ((filename (buffer-file-name))
		  (root-length (if (projectile-project-p)
						   (length (projectile-project-root))
						 0)))
	  (let ((project-filename (substring filename root-length)))
		(kill-new project-filename)
		(message "Copied buffer file name '%s' to the clipboard." project-filename)))))
  

;;;; THEME
(use-package flatui-theme
  :ensure
  :config
  (load-theme 'flatui t))
(global-hl-line-mode)

; Switch mode-line color from flatuicolors.com
(setq green-sea "#16a085" turquoise "#1abc9c" turquoise-hover "#1ABC9C"
      nephritis "#27ae60" emerald "#2ecc71" emerald-hover "#40d47e"
      belize-hole "#2980b9" peter-river "#3498db" peter-river-hover "#4aa3df"
      wisteria "#8e44ad" amethyst "#9b59b6" amethyst-hover "#a66bbe"
      midnight "#2c3e5e" wet-asphalt "#34495e" wet-asphalt-hover "#3d566e"
      orange "#f39c12" sun-flower "#f1c40f" sun-flower-hover "#f2ca27"
      pumpkin "#d35400" carrot "#e67e22" carrot-hover "#e98b39"
      pomegranate "#c0392b" alizarin "#e74c3c" alizarin-hover "#ea6153"
      silver "#bdc3c7" clouds "#ecf0f1" cluds-hover "#fbfcfc"
      asbestos "#7f8c8d" concrete "#95a5a6" concrete-hover "#a3b1b2")

(set-face-background 'mode-line-inactive silver)
(set-face-foreground 'mode-line-inactive asbestos)
(set-face-attribute 'mode-line-inactive nil :box nil)

(let ((current-color (lambda ()
		       (let ((color (cond ((minibufferp) `(,wisteria . ,amethyst-hover))
					  ((evil-insert-state-p) `(,nephritis . ,emerald-hover))
					  ((evil-normal-state-p) `(,belize-hole . ,peter-river-hover))
					  ((evil-emacs-state-p) `(,green-sea . ,turquoise-hover))
					  ((evil-visual-state-p) `(,pumpkin . ,carrot-hover))
					  ((evil-replace-state-p) `(,pomegranate . ,alizarin-hover))
					  ((evil-motion-state-p) `(,midnight . ,wet-asphalt-hover))
					  (t `(,asbestos . ,concrete))
					  )))
			 (set-face-background 'mode-line (car color))
			 (set-face-foreground 'mode-line (cdr color))
			 ))))
  (add-hook 'post-command-hook current-color))

; Previous font '(default ((t (:height 110 :width normal :foundry "nil" :family "Menlo"))))
;;;; CUSTOMIZE

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ecb-options-version "2.40")
 '(ediff-split-window-function (quote split-window-horizontally))
 '(projectile-mode-line (quote (:eval (format " P[%s]" (projectile-project-name)))))
 '(show-paren-mode t)
 '(uniquify-buffer-name-style (quote forward) nil (uniquify))
 '(uniquify-separator ":"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:height 90 :width normal :foundry "nil" :family "ProfontIIx"))))
 '(mode-line ((t (:box nil))))
 '(mode-line-buffer-id ((t (:foreground "White"))))
 '(mode-line-emphasis ((t (:inverse-video t :weight bold))))
 '(modelinepos-column-warning ((t (:foreground "white"))))
 '(modelinepos-region ((t (:foreground "white")))))

(provide 'init)
;;; init.el ends here
