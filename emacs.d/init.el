;;; Init.el --- Cusomizations for Emacs
;;; Commentary:
;;; Code:

; Increase GC-limit during initialization
(defvar my-old-gc-cons-threshold)
(setq my-old-gc-cons-threshold gc-cons-threshold)
(setq gc-cons-threshold 100000000)

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

(if (not
     (and
      (display-graphic-p)
      (string-equal system-type "darwin")))
    (menu-bar-mode -1))

(require 'package)
;;; Add package repository
(add-to-list 'package-archives
			 '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)


(org-babel-load-file (concat user-emacs-directory "config.org"))


(put 'narrow-to-region 'disabled nil)

; Restore GC-limit
(setq gc-cons-threshold my-old-gc-cons-threshold)
(provide 'init)
;;; init.el ends here
