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
 '(frame-brackground-mode 'dark)
 '(helm-buffer-max-length 40)
 '(hl-paren-background-colors (quote ("#2492db" "#95a5a6" nil)))
 '(hl-paren-colors (quote ("#ecf0f1" "#ecf0f1" "#c0392b")))
 '(imenu-list-size 35 t)
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
 '(package-selected-packages
   (quote
	(ack-and-a-half adaptive-wrap all-the-icons-dired-mode anaconda-mode ansible auto-indent-mode cm-mode color-theme color-theme-sanityinc-tomorrow company company-anaconda cyberpunk-theme dash-at-point dayone diminish docker docker-compose-mode docker-tramp dockerfile-mode evil-leader evil-magit evil-matchit evil-org evil-surround evil-text-object-python evil-visualstar flatland-theme flatui-theme flycheck-pyflakes flycheck-rust git-gutter gnuplot-mode gotham-theme gradle-mode graphviz-dot-mode groovy-mode gruber-darker-theme gruvbox-theme haskell-mode helm-ack helm-css-scss helm-dash helm-descbinds helm-flycheck helm-git helm-pass helm-projectile helm-themes helm-tramp htmlize indent-guide jedi jq-mode kubernetes kubernetes-evil lacarte less-css-mode magit-push-remote magit-svn magit-tramp markdown-mode markdown-mode+ material-theme modeline-posn molokai-theme monokai-theme multiple-cursors neotree org pass projectile puppet-mode pyvenv racer rust-mode scss-mode smart-mode-line smart-tabs-mode solarized-theme spacegray-theme spaceline sr-speedbar sublime-themes textile-mode todotxt-mode toml-mode use-package vagrant vagrant-tramp web-mode yaml-mode yasnippet)))
 '(pos-tip-background-color "#A6E22E")
 '(pos-tip-foreground-color "#272822")
 '(projectile-mode-line (quote (:eval (format " P[%s]" (projectile-project-name)))))
 '(safe-local-variable-values
   '((lsp-pyls-plugins-pylint-enabled . t)
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
	 (tmux-test-command . "./runtests.sh"))))
 '(send-mail-function (quote mailclient-send-it))
 '(show-paren-mode t)
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#073642" 0.2))
 '(term-default-bg-color "#002b36")
 '(term-default-fg-color "#839496")
 '(uniquify-buffer-name-style (quote forward) nil (uniquify))
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
 '(vc-display-status t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-date-today ((t (:foreground "#16a085" :slant italic :weight bold)))))
