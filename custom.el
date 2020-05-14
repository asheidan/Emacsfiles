(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   ["#ecf0f1" "#e74c3c" "#2ecc71" "#f1c40f" "#2492db" "#9b59b6" "#1abc9c" "#2c3e50"])
 '(calendar-week-start-day 1)
 '(compilation-message-face 'default)
 '(custom-safe-themes
   '("15348febfa2266c4def59a08ef2846f6032c0797f001d7b9148f30ace0d08bcf" "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "3b24f986084001ae46aa29ca791d2bc7f005c5c939646d2b800143526ab4d323" "3a91205cfbbe87a78889e7871000b73e89783dde76550dc32fd6d379a1fe70da" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" default))
 '(ecb-options-version "2.40")
 '(ediff-split-window-function 'split-window-horizontally)
 '(ediff-window-setup-function 'ediff-setup-windows-plain)
 '(fci-rule-color "#f1c40f")
 '(flycheck-check-syntax-automatically '(save mode-enabled))
 '(flycheck-python-pycompile-executable "python3")
 '(frame-brackground-mode 'dark)
 '(git-gutter:ask-p nil)
 '(helm-buffer-max-length 40)
 '(helm-completion-style 'emacs)
 '(imenu-list-size 35)
 '(ledger-default-date-format "%Y-%m-%d")
 '(lsp-enable-symbol-highlighting nil)
 '(lsp-prefer-flymake nil t)
 '(lsp-pyls-plugins-pycodestyle-enabled nil t)
 '(lsp-ui-doc-delay 0.5)
 '(lsp-ui-doc-enable nil)
 '(lsp-ui-sideline-show-code-actions nil)
 '(magit-diff-use-overlays nil)
 '(neo-click-changes-root nil t)
 '(neo-force-change-root t t)
 '(neo-hidden-regexp-list
   '("^__pycache__$" "^\\." "\\.pyc$" "\\.o$" "~$" "^#.*#$" "\\.elc$") t)
 '(neo-mode-line-type 'neotree)
 '(neo-theme 'ascii)
 '(neo-vc-integration '(face char))
 '(neo-window-fixed-size nil t)
 '(neo-window-width 35 t)
 '(nrepl-message-colors
   '("#dc322f" "#cb4b16" "#b58900" "#546E00" "#B4C342" "#00629D" "#2aa198" "#d33682" "#6c71c4"))
 '(ns-antialias-text t)
 '(org-agenda-clock-consistency-checks
   '(:max-duration "10:00" :min-duration 1 :max-gap "0:05" :gap-ok-around
				   ("4:00")
				   :default-face
				   ((:background "DarkRed")
					(:foreground "white"))
				   :overlap-face nil :gap-face nil :no-end-time-face nil :long-face nil :short-face nil))
 '(org-agenda-clockreport-parameter-plist '(:link t :maxlevel 3 :fileskip0 t :step day :stepskip0 t))
 '(org-agenda-custom-commands
   '(("n" "Agenda and all TODOs"
	  ((agenda "" nil)
	   (alltodo "" nil))
	  nil)
	 ("i" "Inbox (Unscheduled, available TODOs)" tags-todo "-SCHEDULED={.+}"
	  ((org-agenda-overriding-header "Inbox (Unscheduled)"))
	  nil)))
 '(org-agenda-files
   '("~/Worklog/current.org" "~/Worklog/inbox.org" "~/Worklog/2020/03 March" "~/Worklog/2020/04 April" "~/Worklog/2020/05 May" "~/Worklog/Codemill"))
 '(org-agenda-skip-deadline-if-done t)
 '(org-agenda-skip-scheduled-if-deadline-is-shown t)
 '(org-agenda-skip-scheduled-if-done t)
 '(org-agenda-span 'day)
 '(org-agenda-start-day nil t)
 '(org-agenda-time-grid
   '((daily today require-timed remove-match)
	 (800 1000 1200 1400 1600 1800)
	 "......" "----------------"))
 '(org-agenda-window-frame-fractions '(0.2 0.75))
 '(org-agenda-window-setup 'other-window)
 '(org-clock-clocked-in-display nil)
 '(org-clock-in-resume t)
 '(org-clock-out-remove-zero-time-clocks t)
 '(org-clock-out-when-done nil)
 '(org-clock-persist 'history)
 '(org-clock-persist-query-resume nil)
 '(org-deadline-warning-days 5)
 '(org-default-priority 67)
 '(org-directory "~/Worklog")
 '(org-hide-leading-stars t)
 '(org-highest-priority 65)
 '(org-log-done 'time)
 '(org-log-into-drawer "LOGBOOK")
 '(org-log-state-notes-insert-after-drawers t)
 '(org-lowest-priority 69)
 '(org-outline-path-complete-in-steps nil)
 '(org-refile-allow-creating-parent-nodes t)
 '(org-refile-targets
   '(("~/Worklog/current.org" :maxlevel . 1)
	 ("~/Worklog/Codemill/recurring.org" :maxlevel . 1)
	 ("~/Worklog/Codemill/longterm.org" :maxlevel . 1)
	 (my-refile-targets :level . 1)))
 '(org-refile-use-outline-path 'file)
 '(org-super-agenda-groups
   '((:name "Schedule" :time-grid t)
	 (:name "Important" :and
			(:priority "A" :todo "TODO"))
	 (:name "Stale" :scheduled
			(before "2020-04-13")
			:deadline
			(before "2020-04-13")
			:order 200)
	 (:name "Waiting" :todo "WAIT" :order 100)))
 '(org-tag-alist
   '((:startgrouptag)
	 ("work")
	 (:grouptags)
	 ("codemill")
	 ("pro7")
	 (:endgrouptag)
	 (:startgrouptag)
	 ("pro7")
	 (:grouptags)
	 ("ucp")
	 (:endgrouptag)
	 (:startgrouptag)
	 ("codemill")
	 (:grouptags)
	 ("ap_com")
	 (:endgrouptag)))
 '(org-tags-exclude-from-inheritance '("PROJECT" "GOAL"))
 '(org-todo-keywords
   '((sequence "TODO(t)" "|" "DONE(d)")
	 (sequence "WAIT(w@/!)" "|")
	 (sequence "|" "CANCELED(c@)")))
 '(package-selected-packages
   '(all-the-icons org-roam treemacs spacemacs-theme ack-and-a-half adaptive-wrap all-the-icons-dired-mode anaconda-mode ansible auto-indent-mode cm-mode color-theme color-theme-sanityinc-tomorrow company company-anaconda cyberpunk-theme dash-at-point dayone diminish docker docker-compose-mode docker-tramp dockerfile-mode evil-leader evil-magit evil-matchit evil-org evil-surround evil-text-object-python evil-visualstar flatland-theme flatui-theme flycheck-pyflakes flycheck-rust git-gutter gnuplot-mode gotham-theme gradle-mode graphviz-dot-mode groovy-mode gruber-darker-theme gruvbox-theme haskell-mode helm-ack helm-css-scss helm-dash helm-descbinds helm-flycheck helm-git helm-pass helm-projectile helm-themes helm-tramp htmlize indent-guide jedi jq-mode kubernetes kubernetes-evil lacarte less-css-mode magit-push-remote magit-svn magit-tramp markdown-mode markdown-mode+ material-theme modeline-posn molokai-theme monokai-theme multiple-cursors neotree pass projectile puppet-mode pyvenv racer rust-mode scss-mode smart-mode-line smart-tabs-mode solarized-theme spacegray-theme sr-speedbar sublime-themes textile-mode todotxt-mode toml-mode use-package vagrant vagrant-tramp web-mode yaml-mode yasnippet))
 '(pos-tip-background-color "#A6E22E")
 '(pos-tip-foreground-color "#272822")
 '(projectile-mode-line '(:eval (format " P[%s]" (projectile-project-name))))
 '(safe-local-variable-values
   '((pyvenv-default-virtual-env-name . ".venv/adlede")
	 (lsp-pyls-plugins-pycodestyle-enabled)
	 (lsp-pyls-configuration-sources .
									 ["flake8"])
	 (lsp-pyls-plugins-pylint-enabled . t)
	 (lsp-pyls-plugins-pydocstyle-enabled . t)
	 (lsp-pyls-plugins-mypy-enabled . t)
	 (lsp-pyls-plugins-flake8-enabled . t)
	 (lsp-pyls-plugins-pyflakes-enabled)
	 (eval progn
		   (make-local-variable 'process-environment)
		   (setq process-environment
				 (copy-sequence process-environment))
		   (if
			   (projectile-project-p)
			   (setenv "PYTHONPATH"
					   (concat
						(projectile-project-root)
						"src"))))
	 (python-shell-extra-pythonpaths . "/Users/emieri/Documents/Codemill/Pro7/stargate-migrator/src")
	 (company-clang-arguments "-std=c++11")
	 (company-clang-arguments "-std=c++17")
	 (css-indent-offset . 2)
	 (indent-tabs-mode . false)
	 (projectile-project-name . "brb-dml")
	 (projectile-project-name . "brb-es")
	 (tmux-test-command . "./runtests.sh")))
 '(send-mail-function 'mailclient-send-it)
 '(show-paren-mode t)
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#073642" 0.2))
 '(sql-postgres-program "/Applications/Postgres.app/Contents/Versions/10/bin/psql")
 '(term-default-bg-color "#002b36")
 '(term-default-fg-color "#839496")
 '(treemacs-collapse-dirs 10)
 '(treemacs-filewatch-mode t)
 '(treemacs-follow-mode t)
 '(treemacs-fringe-indicator-mode t)
 '(treemacs-no-png-images nil)
 '(treemacs-show-hidden-files nil)
 '(uniquify-buffer-name-style 'forward nil (uniquify))
 '(uniquify-separator "/")
 '(uniquify-strip-common-suffix nil)
 '(vc-annotate-background "#ecf0f1")
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map
   '((30 . "#e74c3c")
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
	 (360 . "#0a74b9")))
 '(vc-display-status t)
 '(which-key-add-column-padding 2)
 '(which-key-separator ": "))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-date-today ((t (:foreground "#16a085" :slant italic :weight bold)))))
