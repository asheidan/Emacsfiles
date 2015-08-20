;;; estimation-mode --- Summary
;;; Commentary:
;;; Code:
;;###autoload
(add-to-list 'auto-mode-alist '("\\.est\\'" . estimation-mode))

;;;; Syntax keywords
(defconst estimation-font-lock-keywords
  (list
   '("^---$\\|#,*$" . font-lock-comment-face)
   '("^[[:space:]]*-" . font-lock-builtin-face)
   '("" . font-lock-keyword-face)
   '("" . font-lock-string-face)
   '("[+~-](\\|)" . font-lock-reference-face)
   '("\\<[[:digit:]]+\\(.[[:digit:]]+\\)?[hdwp]\\>" . font-lock-type-face)
   '("\\<\\(description\\|estimate\\|notes\\|questions\\|tasks\\):" . font-lock-variable-name-face))
  "Minimal highlighting expression for estimation mode")

(defun char-at-indent ()
  "Tries to return the character which begins this line"
  (let ((beg-post (point)))
	(back-to-indentation)
	(let ((first-char (char-after)))
	  (goto-char beg-post)
	  first-char)))
;(char-at-indent)

(defun line-is-list-item ()
  "Returns t if line is a list item"
  (interactive)
  (char-equal ?- (char-at-indent)))

(defun estimation-nav-end-of-block ()
  "Whatever"
  (interactive)
  (let ((block-indentation (current-indentation))
		(block-began-from-list (line-is-list-item)))
	(while (progn
			 (forward-line 1)
			 (and (not (eobp))
				  (or (> (current-indentation) block-indentation)
					  (and (not block-began-from-list)
						   (= (current-indentation) block-indentation)
						   (line-is-list-item)))))))
  (unless (eobp)
	(forward-line -1))
  (end-of-line nil))

(require 'yaml-mode)

;(defvar estimation-mode-syntax-table
;  (let ((st (copy-syntax-table yaml-mode-syntax-table)))
;	(modify-syntax-entry ?' "w" st)))

;;;###autoload
(define-derived-mode estimation-mode yaml-mode "Estimation"
  "Major mode for editing Yaml masquerading as estimates"
  ;(set-syntax-table yaml-mode-syntax-table)
  (set (make-local-variable 'font-lock-defaults) '(estimation-font-lock-keywords))
  (add-to-list 'hs-special-modes-alist
			   `(estimation-mode ":" nil "#"
								 ,(lambda (_arg) (estimation-nav-end-of-block)) nil))
  ;(setq outline-regexp "[[:space:]]*-")
  (modify-syntax-entry ?\" "." estimation-mode-syntax-table)
  (modify-syntax-entry ?' "." estimation-mode-syntax-table)
  )



(provide 'estimation)
