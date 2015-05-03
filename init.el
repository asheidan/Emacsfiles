; Change UI
(if (display-graphic-p)
    (progn
      (tool-bar-mode -1)
      (scroll-bar-mode -1)))

;(menu-bar-mode -1)

(setq inhibit-startup-message t
      initial-scratch-message nil)

; Include column and line number
(setq column-number-mode t
      line-number-mode t)

;;;;; Environment
(add-to-list 'load-path "~/.emacs.d/vendor/")
(add-to-list 'load-path "~/.emacs.d/estimation/")
(add-to-list 'load-path "~/.emacs.d/vendor/evil-rebellion/")
(add-to-list 'custom-theme-load-path (file-name-as-directory "~/.emacs.d/replace/color-themes/"))

;;;;; Packages
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

; Make sure to install use-package
(if (not (package-installed-p 'use-package))
    (progn
      (package-refresh-contents)
      (package-install 'use-package)))
(require 'use-package)

(use-package evil
  :ensure t
  :config
  (evil-mode t))

(use-package flyspell
  :config
  (setq flyspell-issue-welcome-flag nil
	ispell-program-name "aspell"
	ispell-list-command "list"))

(use-package helm-config
  :ensure helm
  :config
  (define-key evil-normal-state-map ", f" 'helm-find-files)
  (define-key evil-normal-state-map ", o" 'helm-imenu)
  (define-key evil-normal-state-map ", b" 'helm-buffers-list))


(use-package helm-projectile
  :ensure t
  :config
  (define-key evil-normal-state-map ", n" 'helm-projectile)
  (define-key evil-normal-state-map ", p" 'helm-projectile-switch-project))

(setq magit-last-seen-setup-instructions "1.4.0")
(use-package magit
  :ensure t
  :config
  (setq magit-commit-show-notes t)
  (define-key evil-normal-state-map ",gs" 'magit-status))

(use-package modeline-posn
  :ensure t
  :config
  (setq modelinepos-column-limit 70))

(use-package projectile
  :ensure t
  :config
  (projectile-global-mode t)
  (define-key evil-normal-state-map ",ps" 'projectile-switch-project))

(use-package tramp
  :config
  (setq tramp-default-method "ssh"))

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

;;;; THEME
;(load-theme 'flatui t)
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

;;;; Behaviour

(setq x-super-keysym 'meta)
                 
;;;; CUSTOMIZE   

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ecb-options-version "2.40")
 '(ediff-split-window-function (quote split-window-horizontally))
 '(show-paren-mode t)
 '(uniquify-buffer-name-style (quote forward) nil (uniquify))
 '(uniquify-separator ":"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(mode-line ((t (:box nil))))
 '(mode-line-buffer-id ((t (:foreground "White"))))
 '(mode-line-emphasis ((t (:inverse-video t :weight bold))))
 '(modelinepos-column-warning ((t (:foreground "white"))))
 '(modelinepos-region ((t (:foreground "white")))))
