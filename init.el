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

(package-initialize)
;(require 'package)
;;; Add package repository
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(eval-when-compile
;  (add-to-list 'load-path "~/.emacs.d/elpa")
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

(require 'ob-tangle)
(defun my/org-babel-load-file (file &optional compile)
  "Load Emacs Lisp source code blocks in the Org FILE.
This function exports the source code using `org-babel-tangle'
and then loads the resulting file using `load-file'.  With prefix
arg (noninteractively: 2nd arg) COMPILE the tangled Emacs Lisp
file to 'byte-code' before it is loaded."
  (interactive "fFile to load: \nP")
  (let* ((age (lambda (file)
                (float-time
                 (time-subtract (current-time)
                                (nth 5 (or (file-attributes (file-truename file))
                                           (file-attributes file)))))))
         (base-name (file-name-sans-extension file))
         (exported-file (concat base-name ".el"))
         (compiled-file (if compile
                            (concat exported-file "c")
                          exported-file)))
    ;; tangle if the Org file is newer than the elisp file
    (unless (and (file-exists-p exported-file)
                 (> (funcall age file) (funcall age exported-file)))
      ;; Tangle-file traversal returns reversed list of tangled files
      ;; and we want to evaluate the first target.
      (setq exported-file
            (car (last (org-babel-tangle-file file exported-file "emacs-lisp")))))
    ;; byte-compile if we should compile and compiled file is older
    (if (and compile
             (not (and (file-exists-p compiled-file)
                       (> (funcall age exported-file) (funcall age compiled-file)))))
	     (progn (byte-compile-file exported-file 'load)
		    (message "Compiled and loaded %s" compiled-file))
	(progn (load-file compiled-file)
               (message "Loaded %s" compiled-file)))))

(my/org-babel-load-file (concat user-emacs-directory "config.org") 'compile)

(put 'narrow-to-region 'disabled nil)

; Restore GC-limit
;(setq gc-cons-threshold my-old-gc-cons-threshold)
(setq gc-cons-threshold (* 32 1000 1000))
(provide 'init)
;;; init.el ends here
