;;; INIT.el --- Cusomizations for Emacs
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

;(aquamacs-autoface-mode -1)
;(set-face-attribute 'mode-line nil :inherit 'unspecified)
;(set-face-attribute 'echo-area nil :family 'unspecified)

(setq inhibit-startup-message t
	  initial-scratch-message nil)


;;;; CUSTOMIZE

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ediff-split-window-function (quote split-window-horizontally))
 '(show-paren-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:height 110 :width normal :foundry "nil" :family "Menlo"))))
 '(mode-line ((t (:box nil))))
 '(mode-line-buffer-id ((t (:foreground "White"))))
 '(mode-line-emphasis ((t (:inverse-video t :weight bold)))))

;;;; ENVIRONMENT
(add-to-list 'load-path "~/.emacs.d/vendor/")
(add-to-list 'load-path "~/.emacs.d/vendor/evil-rebellion/")
(let
    ((path '("/usr/local/bin" "/usr/bin" "/bin"
	     "/usr/local/sbin" "/usr/sbin" "/sbin"
	     "/opt/X11/bin" "/opt/ImageMagick/bin"
	     "/usr/local/MacGPG2/bin" "/usr/texbin")))
  (progn
    (set 'exec-path (delete-dups (append path exec-path)))
    (setenv "PATH" (mapconcat 'identity path ":"))))

;;;; PACKAGES
(defmacro after (mode &rest body)
  `(eval-after-load ,mode
     '(progn ,@body)))

; Pymacs in ~/.emacs.d/vendor/ (for ropemacs)
(with-eval-after-load "auto-complete"
  (add-to-list 'ac-dictionary-directories "~/.emacs.d/dict")
  (setq ac-use-menu-map t)
  (define-key ac-menu-map "\C-n" 'ac-next)
  (define-key ac-menu-map "\C-p" 'ac-previous)
  (message "After auto-complete")
  )

(with-eval-after-load "auto-complete-config"
  (ac-config-default)
  (when (file-exists-p (expand-file-name "~/.emacs.d/vendor/pymacs.el"))
    (ac-ropemacs-initialize)
    (ac-ropemacs-setup))
  (message "After auto-complete-config")
  )

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

(defun my-move-key (keymap-from keymap-to key)
  "Moves key binding from one keymap to another, deleting from the old location. "
  (define-key keymap-to key (lookup-key keymap-from key))
  (define-key keymap-from key nil))

(with-eval-after-load "evil"
  (define-key evil-normal-state-map "," nil)
  (define-key evil-normal-state-map "\C-j" 'evil-jump-to-tag)
  (require 'evil-rebellion)
  (require 'evil-leader)
  (require 'evil-surround))

(with-eval-after-load "evil-leader"
  (global-evil-leader-mode)
  (evil-leader/set-leader ","))

(with-eval-after-load "evil-surround"
  (global-evil-surround-mode 1))

(defun imenu-elisp-sections ()
  (setq imenu-prev-index-position-function nil)
  (add-to-list 'imenu-generic-expression '("Section" "^;;;; \\(.+\\)$" 1) t))
(add-hook 'emacs-lisp-mode-hook 'imenu-elisp-sections)
(with-eval-after-load "helm-config"
  (with-eval-after-load "evil"
    (define-key evil-normal-state-map ", f" 'helm-find-files)
    (define-key evil-normal-state-map ", n" 'helm-projectile)
    (define-key evil-normal-state-map  ", o" 'helm-imenu)
    (define-key evil-normal-state-map ", b" 'helm-buffers-list)))

(with-eval-after-load "projectile"
  (projectile-global-mode)
  (define-key evil-normal-state-map ",ps" 'projectile-switch-project))

(with-eval-after-load "magit"
  (define-key evil-normal-state-map ",gs" 'magit-status))

; (with-eval-after-load "neotree"
;   (global-set-key [f2] 'neotree-toggle))

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

;;; Add package repository
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(with-eval-after-load "flyspell"
  (setq flyspell-issue-welcome-flag nil
		ispell-program-name "aspell"
		ispell-list-command "list"))

;;;; BEHAVIOR
;; Fix option-key
;(setq default-input-method "MacOSX")
(setq mac-option-modifier nil
      mac-command-modifier 'meta
      mac-allow-antialiasing t
      mac-command-key-is-meta t)

(global-set-key [f2] 'speedbar)

(require 'helm-config)

(setq-default tab-width 4)
(smart-tabs-insinuate 'c
					  'c++)

(setq marked-name "Marked 2")
(defun markdown-preview-file ()
  "run Marked on the current file and revert the buffer"
  (interactive)
  (shell-command (format "open -a '%s' '%s'"
						 marked-name
						 (buffer-file-name)))
)

;;; VIM FOREVER
(require 'evil)
(evil-mode t)
(my-move-key evil-motion-state-map evil-normal-state-map (kbd "RET"))
(my-move-key evil-motion-state-map evil-normal-state-map " ")


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
      asbestos "#7f8c8d" concrete "#95a5a6" concrete-hover "#a3b1b2"
      )

(set-face-background 'mode-line-inactive silver)
(set-face-foreground 'mode-line-inactive asbestos)

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

;(require 'smart-mode-line)
;(sml/setup)
;(sml/apply-theme 'dark)
;(add-to-list 'sml/replacer-regexp-list '(":Doc:Codemill/Burberry/" ":BRB:") t)

;;; Enable syntax-checking with flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)

;; Sane indentation for haskell
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

;;; Enable yasnippets
;(add-hook 'prog-mode-hook #'yas-minor-mode)

;;; Ido hacking
(setq ido-use-faces nil) ; Use flx hightlits instead of ido faces

;; Fuzzy matching for ido with flx-ido
(require 'flx-ido)
(flx-ido-mode 1)
;; Vertical list for ido
(require 'ido-vertical-mode)
(ido-vertical-mode 1)

(require 'ido-ubiquitous) ; Ido everywhere!!!
(ido-ubiquitous-mode 1)

(ido-mode 1)
(ido-everywhere 1)
