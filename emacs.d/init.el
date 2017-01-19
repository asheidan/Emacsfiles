;;; Init.el --- Cusomizations for Emacs
;;; Commentary:
;;; Code:

; Profiling
; /Applications/Emacs.app/Contents/MacOS/Emacs -Q -l ./vendor/profile-dotemacs.el --eval "(setq profile-dotemacs-file (setq load-file-name\"~/.emacs.d/init.el\"))" -f profile-dotemacs


;;;; UI
;; Change UI early to prevent flickering

(if (display-graphic-p)
    (progn
      (tool-bar-mode -1) ; Hiding toolbar
      (scroll-bar-mode -1) ; Hide scrollbars
      ;(tabbar-mode -1) ; Disable tabbar
	  ))

(if (not (and (display-graphic-p) (string-equal system-type "darwin")))
	(menu-bar-mode -1))

(setq inhibit-startup-message t
	  initial-scratch-message nil)

; Include column number int mode-line
(setq column-number-mode t
	  line-number-mode t)

;;;; Colors

(defvar flatui-color-green-sea "#16a085")
(defvar flatui-color-turquoise "#1abc9c")
(defvar flatui-color-turquoise-hover "#1ABC9C")
(defvar flatui-color-nephritis "#27ae60")
(defvar flatui-color-emerald "#2ecc71")
(defvar flatui-color-emerald-hover "#40d47e")
(defvar flatui-color-belize-hole "#2980b9")
(defvar flatui-color-peter-river "#3498db")
(defvar flatui-color-peter-river-hover "#4aa3df")
(defvar flatui-color-wisteria "#8e44ad")
(defvar flatui-color-amethyst "#9b59b6")
(defvar flatui-color-amethyst-hover "#a66bbe")
(defvar flatui-color-midnight "#2c3e5e")
(defvar flatui-color-wet-asphalt "#34495e")
(defvar flatui-color-wet-asphalt-hover "#3d566e")
(defvar flatui-color-orange "#f39c12")
(defvar flatui-color-sun-flower "#f1c40f")
(defvar flatui-color-sun-flower-hover "#f2ca27")
(defvar flatui-color-pumpkin "#d35400")
(defvar flatui-color-carrot "#e67e22")
(defvar flatui-color-carrot-hover "#e98b39")
(defvar flatui-color-pomegranate "#c0392b")
(defvar flatui-color-alizarin "#e74c3c")
(defvar flatui-color-alizarin-hover "#ea6153")
(defvar flatui-color-silver "#bdc3c7")
(defvar flatui-color-silver-hover "#cad1d5")
(defvar flatui-color-clouds "#ecf0f1")
(defvar flatui-color-clouds-hover "#fbfcfc")
(defvar flatui-color-asbestos "#7f8c8d")
(defvar flatui-color-concrete "#95a5a6")
(defvar flatui-color-concrete-hover "#a3b1b2")

;;;; ENVIRONMENT
(add-to-list 'load-path "~/.emacs.d/vendor/")
(add-to-list 'custom-theme-load-path (file-name-as-directory "~/.emacs.d/replace-colorthemes/"))
(add-to-list 'custom-theme-load-path (file-name-as-directory "~/.emacs.d/random-colorthemes/"))

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

(eval-when-compile
  (if (not (package-installed-p 'use-package)) ; Make sure that we have use-package installed
	  (progn
		(package-refresh-contents)
		(package-install 'use-package)))
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

; Pymacs in ~/.emacs.d/vendor/ (for ropemacs)

;;;;; Evil
; Keeping this first to make all keymaps available early

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
  "Bind CMD as a evil-ex command."
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
  (define-key evil-normal-state-map "\C-\M-wn" 'make-frame)
  (define-key evil-normal-state-map "\C-\M-wc" 'delete-frame)
  (define-key evil-normal-state-map "\C-\M-wl" 'ns-next-frame)
  (define-key evil-normal-state-map "\C-\M-wh" 'ns-prev-frame)

  (my-move-key evil-motion-state-map evil-normal-state-map (kbd "RET"))
  (my-move-key evil-motion-state-map evil-normal-state-map " ")

  (mapc 'ex-mode-mapping
		'(("gstatus" . magit-status)
		  ("whitespace" . whitespace-mode)
		  ("test" . projectile-test-project)
		  ("make" . projectile-compile-project)
		  ("dash" . dash-at-point)
		  ("ack" . ack-and-a-half)
		  ("co" . flycheck-list-errors))))

  ;(use-package evil-rebellion
  ;  :load-path "vendor/evil-rebellion/")

  ;(use-package evil-leader
  ; :ensure
  ; :config
  ; (global-evil-leader-mode)
  ; (evil-leader/set-leader ","))

  (use-package evil-surround
	:ensure
    :config
	(global-evil-surround-mode 1))

  (use-package evil-text-object-python
	:ensure
	:config
	(add-hook 'python-mode-hook 'evil-text-object-python-add-bindings))

  (use-package evil-visualstar
    :ensure)

;;;;; Adaptive prefix
(use-package adaptive-wrap
  :ensure)

;;;;; Aaaaaaaaall the icons
(use-package all-the-icons
  :ensure t)

(use-package all-the-icons-dired
  :ensure t
  :commands all-the-icons-dired-mode
  :init
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

;;;;; Anzu
(use-package anzu
  :ensure t
  :config
  (setq-default anzu-cons-mode-line-p nil)
  (global-anzu-mode))

;;;;; Auto complete
(use-package auto-complete
  ;:defer t
  :config
  (add-to-list 'ac-dictionary-directories "~/.emacs.d/dict")
  (setq ac-use-menu-map t)
  (define-key ac-menu-map "\C-n" 'ac-next)
  (define-key ac-menu-map "\C-p" 'ac-previous)
  (define-key ac-mode-map [(control tab)] 'auto-complete)

  ; Trying to get ac-clang to look better
  (set-face-background 'popup-face flatui-color-clouds-hover)
  (set-face-foreground 'popup-face flatui-color-midnight)

  (set-face-background 'popup-menu-mouse-face flatui-color-nephritis)
  (set-face-foreground 'popup-menu-mouse-face flatui-color-clouds)

  (set-face-background 'popup-tip-face flatui-color-clouds-hover)
  (set-face-foreground 'popup-tip-face flatui-color-midnight)

  (set-face-foreground 'ac-candidate-face flatui-color-wet-asphalt-hover)
  (set-face-background 'ac-candidate-face flatui-color-clouds-hover)

  (set-face-foreground 'ac-completion-face flatui-color-asbestos)

  (set-face-background 'ac-selection-face flatui-color-wisteria)
  (set-face-foreground 'ac-selection-face flatui-color-clouds))

(use-package auto-complete-clang
  ;:defer t
  :config
  (add-hook 'objc-mode-hook (lambda ()(add-to-list 'ac-sources 'ac-source-clang)))
  (add-hook 'c++-mode-hook (lambda ()(add-to-list 'ac-sources 'ac-source-clang)))
  (add-hook 'c-mode-hook (lambda ()(add-to-list 'ac-sources 'ac-source-clang)))
  (setq ac-clang-flags
		(mapcar (lambda (item)(concat "-I" item))
				(split-string "
 /Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/include/c++/v1
 /usr/local/include
 /Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/lib/clang/7.0.0/include
 /Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/include
 /usr/include
 /System/Library/Frameworks
 /Library/Frameworks")))

  (set-face-background 'ac-clang-candidate-face flatui-color-clouds-hover)
  (set-face-foreground 'ac-clang-candidate-face flatui-color-midnight)

  (set-face-background 'ac-clang-selection-face flatui-color-belize-hole)
  (set-face-foreground 'ac-clang-selection-face flatui-color-clouds))

(use-package jedi
  :commands jedi:setup
  ;:defer t
  :init
  (add-hook 'python-mode-hook 'jedi:setup))


;;;;; Company

;(use-package company)
;(use-package company-jedi
;  :config
;  (add-hook 'python-mode-hook (lambda () (add-to-list 'company-backends 'company-jedi))))

;;;;; Dash.app

(use-package dash-at-point
  :commands dash-at-point
  :bind (:map evil-normal-state-map
			(",dp" . dash-at-point)
			(", d" . helm-dash)))

(use-package hideshow
  :commands (hs-minor-mode)
  ;:diminish hs-minor-mode
  :init
  (add-hook 'estimation-mode 'hs-minor-mode)
  :config
  (add-to-list 'hs-special-modes-alist
			   `(yaml-mode ":" nil "#"
						   ,(lambda (_arg) (let ((block-indentation (current-indentation))
												 (block-began-from-list (line-is-list-item)))
											 (while (progn
													  (forward-line 1)
													  (and (not (eobp))
														   (or (> (current-indentation) block-indentation)
															   (and (not block-began-from-list)
																	(= (current-indentation) block-indentation)
																	(line-is-list-item)))))))) nil)))

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


;;;;; Flycheck
;;; Enable syntax-checking with flycheck

(use-package flycheck
  :ensure
  :commands #'global-flycheck-mode
  ;:defer t
  :init
  (add-hook 'after-init-hook #'global-flycheck-mode))


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
(use-package flyspell
  :bind
  (:map evil-normal-state-map
		(", s s" . flyspell-mode)
		(", s l" . ispell-change-dictionary)
		("z n" . flyspell-goto-next-error))
  :config
  (setq flyspell-issue-welcome-flag nil
		ispell-program-name "aspell"
		ispell-list-command "list"))


;;;;; Git Gutter

(use-package git-gutter
  :ensure
  ;:defer t
  ;:diminish git-gutter-mode
  :bind (:map evil-normal-state-map
				  (",gg" . git-gutter-mode)
				  (",gn" . git-gutter:next-hunk)
				  (",gp" . git-gutter:previous-hunk)))

;;;;; Haskell

;; Sane indentation for haskell
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

;;;;; Helm

(defun imenu-elisp-sections ()
  (add-to-list 'imenu-generic-expression '("Section" "^;;;; \\(.+\\)$" 1) t)
  (add-to-list 'imenu-generic-expression '("Subsection" "^;;;;; \\(.+\\)$" 1) t)
  ;(setq imenu-prev-index-position-function nil)
  )
(add-hook 'emacs-lisp-mode-hook 'imenu-elisp-sections)

(use-package helm-config
  :ensure helm
  :diminish helm-mode
  ;:defer 1

  :config
  (helm-mode 1)

  :bind
  (("M-x". helm-M-x)
   :map evil-normal-state-map
   (", SPC f" . helm-find-files)
   (", SPC o" . helm-imenu)
   (", SPC b" . helm-buffers-list)

   (", SPC x" . helm-M-x)

   (", SPC t" . helm-etags-select)

   (", SPC c" . helm-flycheck)))

;;;;; Indent guide
(use-package indent-guide
  :ensure
  ;:diminish indent-guide-mode
  :bind
  (:map evil-normal-state-map (", i g" . indent-guide-mode)))

;;;;; la Carte

; Used by helm to browse menus
(use-package lacarte
  :ensure
  :bind
  (:map evil-normal-state-map (", SPC m" . helm-browse-menubar)))

(use-package less-css-mode
  :init
  (add-hook 'less-css-mode-hook
			(function (lambda ()
						(add-to-list 'imenu-generic-expression '("Rule" "[{}]\\([ \t\na-z0-9:_-]\\){" 1))))))

;;;;; Magit

(use-package magit
  :ensure
  :bind (:map evil-normal-state-map
			  (",gs" . magit-status)
			  (",gb" . magit-blame))
  :config
  (defvar magit-commit-show-notes)
  (setq magit-commit-show-notes t)
  (defvar magit-push-always-verify)
  (setq magit-push-always-verify nil)
  (defvar magit-last-seen-setup-instructions)
  (setq magit-last-seen-setup-instructions "1.4.0")
  (use-package evil-magit
	:ensure))


;;;;; Markdown-mode

(use-package markdown-mode
  :ensure
  :mode "\\.\\(md\\|mdown\\|markdown\\|wlog\\)\\'"
  :commands markdown-mode
  :config

  ; Trying to get markdown to look better
  (set-face-foreground 'markdown-header-face flatui-color-belize-hole)
  (set-face-foreground 'markdown-header-face-1 flatui-color-belize-hole)
  (set-face-foreground 'markdown-header-face-2 flatui-color-nephritis)
  (set-face-foreground 'markdown-header-face-3 flatui-color-pumpkin)
  (set-face-foreground 'markdown-header-face-4 flatui-color-wisteria)
  (set-face-foreground 'markdown-header-face-5 flatui-color-belize-hole)
  (set-face-foreground 'markdown-header-face-6 flatui-color-nephritis)

  (add-hook 'markdown-mode-hook
			(lambda ()
			  (visual-line-mode t)
			  (adaptive-wrap-prefix-mode t)
			  (ws-trim-mode nil))))

;;;;; Markdown critic-mode
(use-package cm-mode
	:ensure)

;;;;; Modeline stuff
  
(use-package powerline)
(use-package spaceline :after powerline
  :ensure t)
(use-package spaceline-all-the-icons :after spaceline
  :disabled t
  :load-path "vendor/spaceline-all-the-icons")
(use-package spaceline :after powerline
  :config ;(setq-default mode-line-format '("%e" (:eval (spaceline-ml-ati))))
  )

; http://amitp.blogspot.se/2017/01/emacs-spaceline-mode-line.html
(use-package spaceline-config :after spaceline
  :config
  ; NOTE: This only works if there is a graphic element in hte mode-line
  ;(setq-default powerline-height 16)
  (setq-default mode-line-format '("%e" (:eval (spaceline-ml-main))))

  ; http://spacemacs.org/doc/DOCUMENTATION.html#mode-line
  ; https://github.com/domtronn/all-the-icons.el/wiki/Mode-Line
  (setq-default powerline-default-separator nil)
  ;(setq-default spaceline-separator-dir-left '(right. right))
  ;(setq-default spaceline-separator-dir-right '(left . left))
  (spaceline-helm-mode 1)

  ; Change colors when swiching mode
  (setq-default spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)
  ;(spaceline-spacemacs-theme
  ; '((projectile-root)))

  (spaceline-define-segment my/evil-state
	"The current verbose name of evil state. Requires `evil-mode' to be enabled."
	(when (bound-and-true-p evil-local-mode)
	  (s-trim (evil-state-property evil-state :tag t))))

  (spaceline-define-segment my/projectile
	"Tweaked project name"
	(when (projectile-project-p)
	  (let* ((project-name (projectile-project-name))
			 (short-project-name (if (<= (length project-name) 10)
									 project-name
								   (concat (substring project-name 0 4)
										   "…"
										   (substring project-name -5 nil)))))
		(propertize short-project-name
					;'face '(:height 0.9 :inherit)
					;'display '(raise 0.1)
					'help-echo (concat "Projectile: " (projectile-project-root))))))

  (spaceline-define-segment my/project-path
	"The relative path of the current file. Requires `projectile'."
		(let* ((root-length (if (projectile-project-p)
							   (length (projectile-project-root))
							 0))
			   (relative-path (directory-file-name (substring default-directory root-length)))
			   (short-path
				(mapconcat (lambda (s) (if (<= (length s) 4)
										   s
										 (concat (substring s 0 3) "…")))
						   (split-string relative-path "/")
						   "/")))
		  (propertize short-path
					  'help-echo relative-path))
		)


  (spaceline-install
   '((my/evil-state :face highlight-face :when active)
	 ;auto-compile
	 ;buffer-size
	 
	 (((my/projectile
	    ((my/project-path
		  buffer-id
		  )
		 :separator "/"
		 )
	    remote-host)
	   :separator " | ")
	  (buffer-modified :when active))
	 anzu
	 (major-mode :when active)
	 (process :when active)
	 ;(mu4e-alert-segment :when active)
	 ;(erc-track :when active)
	 (version-control :when active)
	 (org-pomodoro :when active)
	 (org-clock :when active)
	 nyan-cat)

   `(which-function
	 (python-pyvenv :fallback python-pyenv)
	 ((flycheck-error flycheck-warning flycheck-info)
	  :when active)
	 ;(minor-modes :when active)
	 ;purpose
	 (battery :when active)
	 selection-info
	 input-method
	 ;(buffer-encoding-abbrev :when active)
	 (global :when active)
	 ((buffer-position
	   point-position
	   line-column)
	  :separator " | "
	  :face highlight-face)
	 ;,@additional-segments
	 ;hud
	 ))
  )

;;;;; Modeline-posn

(use-package modeline-posn
  :disabled t
  :ensure
  :init
  (defvar modelinepos-column-limit)
  (setq modelinepos-column-limit 70))

;;;;; Neotree

(use-package neotree
  :config
  (add-to-list 'neo-hidden-regexp-list "^__pycache__$")
  :bind (([f2] . neotree-toggle)))

;;;;; Org-mode

(use-package org
  :commands org-mode
  :mode ("\\.org\\'" . org-mode)
  :config
  (use-package evil-org
    :ensure
    :defer t))


;;;;; Pass

(use-package pass
  :ensure t
  :commands (pass-mode))

(use-package helm-pass
  :ensure t
  :commands (helm-pass))


;;;;; Projectile

(use-package projectile
  :ensure
  :commands projectile-mode
  :diminish projectile-mode
  :bind
  (:map evil-normal-state-map
		(", SPC n" . helm-projectile)
		(", SPC n" . helm-projectile)
		(", SPC B" . helm-projectile-switch-to-buffer)
		(", SPC p" . helm-projectile-switch-project))
  :init
  (projectile-mode t)
  :config
  (setq projectile-completion-system 'helm)
  (helm-projectile-on)

  (add-to-list 'projectile-globally-ignored-directories ".ropeproject")
  (add-to-list 'projectile-globally-ignored-directories ".virtualenvs")
  (add-to-list 'projectile-globally-ignored-directories ".virtualenv")
  (add-to-list 'projectile-globally-ignored-directories "virtualenvs")
  (add-to-list 'projectile-globally-ignored-directories "virtualenv")

	
  ; Fix for updated projectile
  (defalias 'helm-buffers-list--match-fn 'helm-buffer-match-major-mode))

;(use-package sr-speedbar
;  :ensure
;  :config (global-set-key [f2] 'sr-speedbar-toggle)
;  )

(use-package speedbar
  :disabled t
  :config
  (speedbar-add-supported-extension ".css")
  (speedbar-add-supported-extension ".less")
  (speedbar-add-supported-extension ".jsx")
  (speedbar-add-supported-extension ".md")
  (speedbar-add-supported-extension ".sh")
  (speedbar-add-supported-extension ".xml")
  (speedbar-add-supported-extension ".wlog")
  (defun nm-speedbar-expand-line-list (&optional arg)
	(when arg
	  ;(message (car arg))
	  (re-search-forward (concat " " (car arg) "$"))
	  (speedbar-expand-line (car arg))
	  (speedbar-next 1) ;; Move into the list.
	  (nm-speedbar-expand-line-list (cdr arg))))
  (defun nm-speedbar-open-current-buffer-in-tree ()
	(interactive)
	(let* ((root-dir (projectile-project-root))
		   (original-buffer-file-directory (file-name-directory (buffer-file-name)))
		   (relative-buffer-path (cadr (split-string original-buffer-file-directory root-dir)))
		   (parents (butlast (split-string relative-buffer-path "/"))))
	  (save-excursion
		;;(speedbar 1) ;; Open speedbar
		(set-buffer speedbar-buffer)
		(if (not (string-equal default-directory root-dir))
			(progn
			  (setq default-directory root-dir)
			  (speedbar-update-contents)))
		(goto-char (point-min))
		(nm-speedbar-expand-line-list parents)))))

;(global-set-key [f2] 'speedbar)

;(use-package projectile-speedbar
;  :ensure
;  :bind ("M-<f2>" . projectile-speedbar-open-current-buffer-in-tree)
;  )

;;;;; Smart tabs

(use-package smart-tabs-mode
  :ensure
  :config
  (smart-tabs-insinuate 'c
						'c++))

;;;;; Textile-mode

(use-package textile-mode
  :mode "\\.textile\\'"
  :config
  (add-hook 'textile-mode-hook
			(lambda ()
			  (visual-line-mode t)
			  (adaptive-wrap-prefix-mode t)
			  (ws-trim-mode nil))))

;;;;; Todo.txt mode
(use-package todotxt-mode
  :ensure
  :config
  (setq todotxt-default-file (expand-file-name "~/Dropbox/todo/todo.txt"))
  (add-hook 'todotxt-mode-hook 'toggle-truncate-lines)
  :bind
  (:map evil-normal-state-map
		(",tt" . todotxt-open-file)
		(",ta" . todotxt-add-todo)
		(",td" . todotxt-toggle-done)
		(",tf-" . todotxt-clear-filter)
		(",tfs" . todotxt-filter-by-status)
		(",tfp" . todotxt-filter-by-project)
		(",tft" . todotxt-filter-by-tag)))


;;;;; Vagrant

(use-package vagrant
  :commands vagrant-up)

(use-package vagrant-tramp
  :disabled t)

;;;;; Web mode
(use-package web-mode
  :ensure
  :mode ("\\.html\\'" "\\.erb\\'"
		 "\\.js\\'" "\\.jsx\\'"))

;;;;; Undo tree
(use-package undo-tree
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode 1))


;;;;; Uniquify
(use-package uniquify
  :disabled t
  :config
  (setq-default mode-line-buffer-identification
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
					(concat (projectile-project-name) "|" s)))))

;;;;; Yaml-mode

(use-package yaml-mode
  :ensure)

;;;;; Yasnippet
(use-package yasnippet
  :commands (yas-mode yas-global-mode)
  :diminish yas-minor-mode
  ;:defer t
  :config
  (yas-global-mode 1))

;;;;; nXML

(use-package nxml-mode
  :mode ("\\.xml\\'" . nxml-mode)
  :config
  (setq nxml-child-indent 4))

;;;; NON-PACKAGES

(use-package ws-trim
  :load-path "vendor"
  :diminish ws-trim-mode
  :commands (global-ws-trim-mode ws-trim-mode)
  :config
  (global-ws-trim-mode 1)
  (setq ws-trim-mode 1))

;(use-package auto-complete-clang-objc :load-path "vendor/auto-complete-clang-objc")

;;;; BEHAVIOR
;; Fix option-key
;(setq default-input-method "MacOSX")
(defvar mac-command-modifier)
(defvar mac-allow-antialiasing)
(defvar mac-command-key-is-meta)
(if (string-equal system-type "darwin")
    (setq mac-option-modifier nil
	  mac-command-modifier 'meta
	  mac-allow-antialiasing t
	  mac-command-key-is-meta t))
(defvar x-meta-keysym)
(defvar x-super-keysym)
(if (string-equal system-type "gnu/linux")
    (setq x-meta-keysym 'super
		  x-super-keysym 'meta))

(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))

(defvar tags-revert-without-query)
(setq tags-revert-without-query t)

(setq-default tab-width 4)

(if (not (and (boundp 'server-process)
			  (memq (process-status server-process) '(connect listen open run))))
	(server-start))

(defun switch-to-minibuffer ()
  "Switch to minibuffer window."
  (interactive)
  (if (active-minibuffer-window)
	  (select-window (active-minibuffer-window))
	(error "Minibuffer is not active")))
(message "Bind switch-to-minibubber to something")

(setq split-height-threshold 80
	  split-width-threshold 220)


;;;;; Email

(defface message-double-quoted-text-face
  `((t (:foreground "blue"))) "Quote level 2"
  :group 'basic-faces)
(defface message-multiple-quoted-text-face
  `((t (:foreground "green"))) "Quote level 3+"
  :group 'basic-faces)

;(use-package bbdb
;  :config
;  (bbdb-initialize 'message)
;  (bbdb-insinuate-message))

(add-hook 'message-mode-hook
		  (lambda ()
			(visual-line-mode t)
			(adaptive-wrap-prefix-mode t)
			(font-lock-add-keywords nil
									'(("^[ \t]*>[ \t]*>[ \t]*>.*$"
									   (0 'message-multiple-quoted-text-face))
									  ("^[ \t]*>[ \t]*>.*$"
									   (0 'message-double-quoted-text-face))
									  ))))

;(add-hook 'mail-mode-hook
;		  (lambda ()
;			(visual-line-mode t)
;			adaptive-wrap-prefix-mode t))

;;;; EXTENSIONS

; Timestamps
(defvar current-date-time-format "%Y-%m-%d %H:%M:%S"
  "Format for date to insert with `insert-current-date-time.
See help of `format-time-string' for possible replacements.")

(defun insert-current-date-time ()
  "Insert the current date and time into the current buffer."
  (interactive)
  (insert (format-time-string current-date-time-format (current-time))))

;(with-eval-after-load "evil"
;  (define-key evil-normal-state-map ",dt" 'insert-current-date-time))

; Dir-local
(defun reload-dir-locals-for-current-buffer()
  "Reload dir local variables for the current buffer"
  (interactive)
  (let ((enable-local-variables :all))
	(hack-dir-local-variables-non-file-buffer)))
(defun reload-dir-locals-for-all-buffer-in-this-directory ()
  "For every buffer with the same `default-directory` as the current buffer's, reload dir-locals."
  (interactive)
  (let ((dir default-directory))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (equal default-directory dir))
		(reload-dir-locals-for-current-buffer)))))
(add-hook 'emacs-lisp-mode-hook
		  (defun enable-autoreload-for-dir-locals ()
			(when (and (buffer-file-name)
					   (equal dir-locals-file
							  (file-name-nondirectory (buffer-file-name))))
			  (add-hook (make-variable-buffer-local 'after-save-hook)
						'reload-dir-locals-for-all-buffer-in-this-directory))))

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
  "Open worklog-file for today."
  (interactive)
  (let ((file-name (format-time-string "~/Dropbox/Worklog/%Y/%m %B/%Y-%m-%d.wlog" (current-time))))
	(find-file file-name)
	(goto-char (point-max))
	(markdown-mode)))
(define-key evil-normal-state-map ",wt" 'worklog-open-today)

(defun worklog-new-entry ()
  "Add a new line with a timestamp."
  (interactive)
  (goto-char (point-max))
  (insert (format-time-string (concat "\n## " current-date-time-format) (current-time))))
(define-key evil-normal-state-map ",wn" 'worklog-new-entry)


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


; Filepath with number
(defun copy-file-name-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
	  (message "Copied buffer file name '%s' to the clipboard." filename))))

(fset 'review-file-name 'copy-file-name-to-clipboard)

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
		(message "Copied buffer file name '%s' to the clipboard." project-filename))))
  (fset 'review-file-name 'copy-project-file-name-to-clipboard))

(defun file-path-with-number ()
  "Set clipboard to the path of the file corresponding to the current buffer."
  (interactive)
  (let ((path-with-number (concat (review-file-name) ":" (number-to-string (line-number-at-pos)))))
	(kill-new path-with-number)
	(message "Copied buffer file name '%s' to the clipboard." path-with-number)))

(defun tmux-make ()
  "Send command to specific tmux-session."
  (interactive)
  (call-process-shell-command "tmux send-keys -t brbes:1 make space test_backend enter"))


;;;; THEME
(use-package flatui-theme
  :ensure
  :config
  (load-theme 'flatui t))

(global-hl-line-mode)

; Switch mode-line color from flatuicolors.com
;(set-face-background 'mode-line-inactive flatui-color-silver)
;(set-face-foreground 'mode-line-inactive flatui-color-concrete)
;(set-face-attribute 'mode-line-inactive nil :box nil)
;(set-face-attribute 'mode-line nil :box nil)

;(let ((current-color (lambda ()
;		       (let ((color (cond ((minibufferp) `(,flatui-color-wisteria . ,flatui-color-amethyst-hover))
;								  ((evil-insert-state-p) `(,flatui-color-nephritis . ,flatui-color-emerald-hover))
;								  ((evil-normal-state-p) `(,flatui-color-belize-hole . ,flatui-color-peter-river-hover))
;								  ((evil-emacs-state-p) `(,flatui-color-green-sea . ,flatui-color-turquoise-hover))
;								  ((evil-visual-state-p) `(,flatui-color-pumpkin . ,flatui-color-carrot-hover))
;								  ((evil-replace-state-p) `(,flatui-color-pomegranate . ,flatui-color-alizarin-hover))
;								  ((evil-motion-state-p) `(,flatui-color-midnight . ,flatui-color-wet-asphalt-hover))
;								  (t `(,flatui-color-asbestos . ,flatui-color-concrete))
;								  )))
;			 (set-face-background 'mode-line (car color))
;			 (set-face-foreground 'mode-line (cdr color))
;			 ))))
;  (add-hook 'post-command-hook current-color))



; Previous font '(default ((t (:height 110 :width normal :foundry "nil" :family "Menlo"))))
;;;; CUSTOMIZE

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   ["#ecf0f1" "#e74c3c" "#2ecc71" "#f1c40f" "#2492db" "#9b59b6" "#1abc9c" "#2c3e50"])
 '(compilation-message-face (quote default))
 '(custom-safe-themes
   (quote
	("4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "3b24f986084001ae46aa29ca791d2bc7f005c5c939646d2b800143526ab4d323" "3a91205cfbbe87a78889e7871000b73e89783dde76550dc32fd6d379a1fe70da" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" default)))
 '(ecb-options-version "2.40")
 '(ediff-split-window-function (quote split-window-horizontally))
 '(ediff-window-setup-function (quote ediff-setup-windows-plain))
 '(fci-rule-color "#f1c40f")
 '(frame-brackground-mode (quote dark))
 '(helm-buffer-max-length 40)
 '(highlight-changes-colors (quote ("#FD5FF0" "#AE81FF")))
 '(highlight-tail-colors
   (quote
	(("#49483E" . 0)
	 ("#67930F" . 20)
	 ("#349B8D" . 30)
	 ("#21889B" . 50)
	 ("#968B26" . 60)
	 ("#A45E0A" . 70)
	 ("#A41F99" . 85)
	 ("#49483E" . 100))))
 '(hl-paren-background-colors (quote ("#2492db" "#95a5a6" nil)))
 '(hl-paren-colors (quote ("#ecf0f1" "#ecf0f1" "#c0392b")))
 '(magit-diff-use-overlays nil)
 '(neo-mode-line-type (quote neotree))
 '(neo-smart-open t)
 '(neo-theme (quote nerd))
 '(neo-vc-integration nil)
 '(package-selected-packages
   (quote
	(all-the-icons-dired all-the-icons-dired-mode all-the-icons molokai-theme anzu smart-mode-line spaceline pass helm-pass material-theme less-css-mode dockerfile-mode vagrant yasnippet yaml-mode web-mode use-package todotxt-mode textmate textile-mode swift-mode sublime-themes sr-speedbar spacegray-theme smart-tabs-mode scss-mode pyvenv puppet-mode php-mode php+-mode peg notmuch-labeler nose neotree multiple-cursors monokai-theme modeline-posn markdown-mode+ magit-tramp magit-svn magit-push-remote lacarte jedi indent-guide ido-vertical-mode ido-ubiquitous helm-themes helm-projectile helm-git helm-flycheck helm-dash helm-css-scss haskell-mode gruvbox-theme gruber-darker-theme groovy-mode graphviz-dot-mode gradle-mode gnuplot-mode git-gutter flycheck-pyflakes flx-ido flatui-theme flatland-theme fiplr evil-visualstar evil-text-object-python evil-surround evil-org evil-matchit evil-magit emamux ecb dayone dash-at-point cyberpunk-theme color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized color-theme coffee-mode cm-mode bbdb-vcard auto-indent-mode auto-complete-clang-async auto-complete-clang adaptive-wrap ack-and-a-half)))
 '(pos-tip-background-color "#A6E22E")
 '(pos-tip-foreground-color "#272822")
 '(projectile-mode-line (quote (:eval (format " P[%s]" (projectile-project-name)))))
 '(safe-local-variable-values
   (quote
	((css-indent-offset . 2)
	 (indent-tabs-mode . false)
	 (projectile-project-name . "brb-dml")
	 (projectile-project-name . "brb-es"))))
 '(send-mail-function (quote mailclient-send-it))
 '(show-paren-mode t)
 '(sml/active-background-color "#34495e")
 '(sml/active-foreground-color "#ecf0f1")
 '(sml/inactive-background-color "#dfe4ea")
 '(sml/inactive-foreground-color "#34495e")
 '(speedbar-directory-unshown-regexp "^\\(\\..*\\|__pycache__\\)\\'")
 '(speedbar-indentation-width 2)
 '(speedbar-use-images nil)
 '(speedbar-verbosity-level 0)
 '(uniquify-buffer-name-style (quote forward) nil (uniquify))
 '(uniquify-separator "/")
 '(uniquify-strip-common-suffix nil)
 '(vc-annotate-background "#ecf0f1")
 '(vc-annotate-color-map
   (quote
	((30 . "#e74c3c")
	 (60 . "#c0392b")
	 (90 . "#e67e22")
	 (120 . "#d35400")
	 (150 . "#f1c40f")
	 (180 . "#d98c10")
	 (210 . "#2ecc71")
	 (240 . "#27ae60")
	 (270 . "#1abc9c")
	 (300 . "#16a085")
	 (330 . "#2492db")
	 (360 . "#0a74b9"))))
 '(vc-annotate-very-old-color "#0a74b9")
 '(weechat-color-list
   (unspecified "#272822" "#49483E" "#A20C41" "#F92672" "#67930F" "#A6E22E" "#968B26" "#E6DB74" "#21889B" "#66D9EF" "#A41F99" "#FD5FF0" "#349B8D" "#A1EFE4" "#F8F8F2" "#F8F8F0")))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(mode-line ((t (:box nil))))
 '(mode-line-buffer-id ((t (:foreground "White"))))
 '(mode-line-emphasis ((t (:inverse-video t :weight bold))))
 '(mode-line-inactive ((t (:box nil))))
 '(modelinepos-column-warning ((t (:foreground "white"))))
 '(modelinepos-region ((t (:foreground "white")))))


(put 'narrow-to-region 'disabled nil)

(defvar my-default-font-family)
(defvar my-default-font-height)
(if (string-equal system-type "darwin")
	;(:foundry "nil" :family "ProfontIIx")
	; Source Code Pro:110
	(setq my-default-font-family "ProfontIIx"
		  my-default-font-height 90))
(if (string-equal system-type "gnu/linux")
	;(:foundry "unknown" :family "ProFontWindows")
	(setq my-default-font-family "ProFontWindows"
		  my-default-font-height 90))

(set-face-attribute 'default nil :height my-default-font-height :family my-default-font-family)


(provide 'init)
;;; init.el ends here


;; Added by Kite
;(load-file "~/.emacs.d/kite.el")
