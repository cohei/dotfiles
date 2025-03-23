;;; init --- emacs config file

;;; Commentary:

;; Emacs config file.

;;; Code:

;;; Language

(set-language-environment "Japanese")
(prefer-coding-system 'utf-8)

;;; Packages

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'leaf)
(straight-use-package 'leaf-keywords)

(leaf leaf-keywords
  :config
  (leaf-keywords-init))

;; for leaf's blackout keyword
(leaf blackout
  :straight t)

;; make an effect early
(leaf gcmh
  :straight t
  :global-minor-mode t
  :blackout t)

(leaf align
  :bind
  ("C-c a" . align)
  ("C-x a r" . align-regexp)
  :push
  ((align-rules-list . '(ruby19-hash (regexp . ":\\(\s-*\\)") (modes . (ruby-mode))))
   (align-rules-list . '(ruby-assignment (regexp . "\\(\s-*\\)=") (modes . (ruby-mode))))))

(leaf ansi-color
  :config
  (defun my/ansi-colorize-current-buffer ()
    "Colorize ansi escape sequences in the current buffer."
    (interactive)
    (ansi-color-apply-on-region (point-min) (point-max))))

(leaf apib-mode
  :straight t
  :mode "\\.apib\\'")

(leaf autorevert
  :blackout auto-revert-mode)

(leaf avy
  :straight t
  :config
  (avy-setup-default))

(leaf beacon
  :straight t
  :global-minor-mode t
  :blackout t)

(leaf browse-at-remote
  :straight t)

(leaf cape
  :straight t
  :config
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-nonexclusive)
  :hook
  (prog-mode-hook
   . (lambda ()
       (dolist (f (list #'cape-file #'cape-dabbrev #'cape-keyword))
         (add-hook 'completion-at-point-functions f nil t)))))

(leaf cc-mode
  :config
  (defun my/indent-by-two ()
    (setq-local c-basic-offset 2)
    (c-set-offset 'case-label '+))
  :hook
  (java-mode-hook . my/indent-by-two))

(leaf coffee-mode
  :straight t
  :custom
  (coffee-tab-width . 2))

(leaf consult
  :straight t
  :bind
  ("C-c C-r" . consult-recent-file)
  ("C-; g" . consult-git-grep)
  ([remap goto-line] . consult-goto-line)
  ([remap switch-to-buffer] . consult-buffer)
  ([remap yank-pop] . consult-yank-pop)
  (flymake-mode-map
   ("C-c !" . consult-flymake))
  :custom
  (xref-show-definitions-function . #'consult-xref)
  (xref-show-xrefs-function . #'consult-xref))

(leaf consult-hoogle
  :straight t)

(leaf consult-ls-git
  :straight t
  :bind
  ("C-; l" . consult-ls-git))

(leaf consult-project-extra
  :straight t
  :bind
  ("C-; f" . consult-project-extra-find))

(leaf corfu
  :straight t
  :global-minor-mode global-corfu-mode
  :custom
  (corfu-auto . t)
  (corfu-auto-prefix . 2))

(leaf ctrlf
  :straight t
  :global-minor-mode t
  :defer-config
  (add-to-list 'ctrlf-style-alist '(migemo :prompt "migemo"
                                           :translator migemo-search-pattern-get
                                           :case-fold ctrlf-no-uppercase-regexp-p))
  :custom
  (ctrlf-auto-recenter . t))

(leaf cursor-sensor
  :hook
  (minibuffer-setup-hook . cursor-intangible-mode)
  :custom
  (minibuffer-prompt-properties . `,(plist-put minibuffer-prompt-properties 'cursor-intangible t)))

(leaf cus-edit
  :init
  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
  :config
  (defun my/delete-custom-file ()
    (if (file-exists-p custom-file) (delete-file custom-file)))
  :hook
  (kill-emacs-hook . my/delete-custom-file))

(leaf csv-mode
  :straight t)

(leaf dabbrev
  :config
  (defun my/dabbrev-completion-with-all-buffers ()
    (interactive)
    (dabbrev-completion 16)) ; C-u C-u to search all buffers
  :bind
  (("M-/" . my/dabbrev-completion-with-all-buffers)
   ("C-M-/" . dabbrev-expand))
  :custom
  (dabbrev-case-replace . nil))

(leaf delsel
  :global-minor-mode delete-selection-mode)

(leaf desktop
  :global-minor-mode desktop-save-mode
  :custom
  (desktop-load-locked-desktop . t))

(leaf dhall-mode
  :straight t)

(leaf diff-hl
  :straight t
  :global-minor-mode global-diff-hl-mode
  :hook
  (dired-mode-hook . diff-hl-dired-mode))

(leaf dimmer
  :straight t
  :global-minor-mode t
  :custom
  (dimmer-fraction . 0.3))

(leaf dired
  :custom
  (dired-dwim-target . t))

(leaf direnv
  :straight t
  :global-minor-mode t)

(leaf dmacro
  :straight t
  :global-minor-mode global-dmacro-mode
  :blackout t
  :custom
  (dmacro-key . `,(kbd "C-c d")))

(leaf dockerfile-mode
  :straight t)

(leaf dumb-jump
  :straight t
  :hook
  (xref-backend-functions . dumb-jump-xref-activate))

(leaf ediff
  :custom
  (ediff-split-window-function . 'split-window-horizontally)
  (ediff-window-setup-function . 'ediff-setup-windows-plain))

(leaf eglot
  :straight (eglot :source gnu-elpa-mirror)
  :hook
  ((haskell-mode-hook js-base-mode-hook nix-mode-hook ruby-base-mode-hook scala-mode-hook sh-mode-hook yaml-mode-hook) . eglot-ensure)
  :bind
  (eglot-mode-map
   ("C-c e" . 'eglot-code-actions)))

(leaf eglot-booster
  :straight (eglot-booster :host github :repo "jdtsmith/eglot-booster")
  :after eglot
  :config
  (eglot-booster-mode))

(leaf eldoc
  :custom
  (eldoc-minor-mode-string . nil))

(leaf elec-pair
  :global-minor-mode electric-pair-mode)

(leaf elm-mode
  :straight t)

(leaf emacs
  :bind
  ("C-c C-d" . delete-pair)
  :custom
  (enable-recursive-minibuffers . t)
  (indent-tabs-mode . nil)
  (indicate-buffer-boundaries . 'right)
  (inhibit-startup-screen . t)
  (initial-scratch-message . nil)
  (scroll-conservatively . 1000)
  (scroll-margin . 5)
  (use-dialog-box . nil)
  (use-short-answers . t))

(leaf emacs-lock
  :blackout t
  :config
  (with-current-buffer "*scratch*"
    (emacs-lock-mode 'kill)))

(leaf embark
  :straight t
  :bind
  ("C-." . embark-act))

(leaf embark-consult
  :straight t)

(leaf emmet-mode
  :straight t
  :hook
  (sgml-mode-hook css-mode-hook)
  :custom
  (emmet-move-cursor-between-quotes . t))

(leaf exec-path-from-shell
  :straight t
  :config
  (exec-path-from-shell-initialize))

(leaf executable
  :hook
  (after-save-hook . executable-make-buffer-file-executable-if-script-p))

(leaf expand-region
  :straight t
  :bind
  ("C-=" . er/expand-region))

(leaf files
  :config
  (add-to-list 'auto-mode-alist '("\\.envrc\\'" . shell-script-mode))
  (add-to-list 'backup-directory-alist `("\\.*$" . ,(expand-file-name "backup" user-emacs-directory)))
  (defun my/reload-init-file ()
    "Reload user’s initialization file."
    (interactive)
    (load-file user-init-file))
  :custom
  (backup-by-copying . t)
  (confirm-kill-emacs . 'y-or-n-p)
  (remote-file-name-inhibit-cache . 600)
  (require-final-newline . 'visit))

(leaf fish-mode
  :straight t)

(leaf flymake
  :hook prog-mode-hook)

(leaf flymake-diagnostic-at-point
  :straight t
  :hook flymake-mode-hook
  :custom
  (flymake-diagnostic-at-point-timer-delay . 1))

(leaf frame
  :global-minor-mode blink-cursor-mode
  :custom
  (blink-cursor-blinks . 0)
  (default-frame-alist . '((fullscreen . fullboth)
                           (font . "Cica-14")
                           (vertical-scroll-bars . nil))))

(leaf free-keys
  :straight t)

(leaf google-this
  :straight t
  :global-minor-mode t
  :blackout t)

(leaf goto-addr
  :hook
  (prog-mode-hook . goto-address-prog-mode)
  (text-mode-hook . goto-address-mode))

(leaf groovy-mode
  :straight t)

(leaf haml-mode
  :straight t
  :mode "\\.hamlc\\'")

(leaf haskell-mode
  :straight t)

(leaf helpful
  :straight t
  :bind
  ("C-c C-h" . helpful-at-point)
  ([remap describe-function] . helpful-callable)
  ([remap describe-key] . helpful-key)
  ([remap describe-variable] . helpful-variable)
  :push
  ((shackle-rules . '(helpful-mode :align right :size 72))))

(leaf highlight-indent-guides
  :straight t
  :blackout t
  :hook prog-mode-hook yaml-mode-hook
  :custom
  (highlight-indent-guides-method . 'bitmap)
  (highlight-indent-guides-bitmap-function . 'highlight-indent-guides--bitmap-line)
  (highlight-indent-guides-auto-character-face-perc . 100))

(leaf hledger-mode
  :straight t
  :mode "\\.journal\\'"
  :config
  (defun my/hledger-set-tab-width ()
    (setq tab-width 4))
  :hook
  (hledger-mode-hook . my/hledger-set-tab-width))

(leaf imenu-list
  :straight t
  :bind
  ("C-'" . imenu-list-smart-toggle)
  :custom
  (imenu-list-auto-resize . t)
  (imenu-list-focus-after-activation . t))

(leaf js2-mode
  :straight t
  :mode "\\.js\\'"
  :custom
  (js-indent-level . 2)
  (js2-indent-switch-body . t)
  (js2-strict-missing-semi-warning . nil))

(leaf magit
  :straight t
  :after shackle
  :global-minor-mode global-git-commit-mode
  :bind
  ("C-; m" . magit-status-here)
  :custom
  (magit-diff-refine-hunk . 'all)
  (shackle-rules . `((magit-status-mode :align t :size 0.6) ,@shackle-rules)))

(leaf markdown-mode
  :straight t
  :mode
  ("README\\.md\\'" . gfm-mode)
  :custom
  (markdown-fontify-code-blocks-natively . t))

(leaf marginalia
  :straight t
  :global-minor-mode t
  :bind
  (minibuffer-local-map
   ("M-A" . marginalia-cycle)))

(leaf mb-depth
  :global-minor-mode minibuffer-depth-indicate-mode)

(leaf migemo
  :straight t
  :require t
  :config
  (migemo-init)
  :custom
  (migemo-dictionary . `,(expand-file-name "~/.nix-profile/share/migemo/utf-8/migemo-dict"))
  (migemo-user-dictionary . nil)
  (migemo-regex-dictionary . nil)
  (migemo-use-default-isearch-keybinding . nil))

(leaf misc
  :bind
  ("C-c C-c" . duplicate-dwim))

(leaf multiple-cursors
  :straight t
  :bind
  ("C-c m e" . mc/edit-lines)
  ("C-c m n" . mc/mark-next-like-this))

(leaf mwim
  :straight t
  :bind
  ([remap move-beginning-of-line] . mwim-beginning-of-line-or-code)
  ([rempa move-end-of-line] . mwim-end-of-line-or-code))

(leaf nix-mode
  :straight t
  :mode "\\.nix\\'")

(leaf open-junk-file
  :straight t
  :commands open-junk-file
  :custom
  (open-junk-file-format . `,(expand-file-name "junk/%Y/%m/%d-%H%M%S." user-emacs-directory))
  (open-junk-file-find-file-function . 'find-file))

(leaf orderless
  :straight t
  :custom
  (completion-styles . '(orderless basic))
  (completion-category-overrides . '((file (styles basic partial-completion)))))

(leaf paren
  :custom
  (show-paren-mode . nil))

(leaf peep-dired
  :straight t
  :bind
  (dired-mode-map
   ("C-x x" . peep-dired))
  (peep-dired-mode-map
   ("C-x x" . peep-dired)))

(leaf project
  :custom
  (project-vc-extra-root-markers . '(".jj"))
  (project-vc-ignores . '(".jj")))

(leaf purescript-mode
  :straight t
  :hook
  (purescript-mode-hook . turn-on-purescript-indentation))

(leaf rainbow-delimiters
  :straight t
  :hook prog-mode-hook)

(leaf rainbow-mode
  :straight t
  :hook
  (css-mode-hook scss-mode-hook html-mode-hook lisp-mode-hook web-mode-hook))

(leaf recentf
  :global-minor-mode t
  :custom
  (recentf-max-saved-items . 100)
  (recentf-save-file . `,(expand-file-name "recentf" user-emacs-directory)))

(leaf ruby-mode
  :mode "\\.cap\\'" "\\.Brewfile\\'"
  :custom
  (ruby-insert-encoding-magic-comment . nil))

(leaf ruby-end
  :straight t
  :blackout t)

(leaf ruby-hash-syntax
  :straight t)

(leaf ruby-interpolation
  :straight t
  :blackout t
  :hook
  ruby-base-mode-hook)

(leaf rust-mode
  :straight t)

(leaf savehist
  :global-minor-mode t)

(leaf scala-mode
  :straight t)

(leaf scss-mode
  :straight t
  :custom
  (css-indent-offset . 2)
  (scss-compile-at-save . nil))

(leaf selected
  :straight t
  :blackout selected-minor-mode
  :hook
  ((prog-mode-hook text-mode-hook) . selected-minor-mode)
  :bind
  (selected-keymap
   ("%" . query-replace)
   (";" . comment-dwim)
   ("c" . duplicate-dwim)
   ("e" . mc/edit-lines)
   ("g" . google-this-region)
   ("i" . indent-rigidly)
   ("m" . apply-macro-to-region-lines)
   ("n" . count-words-region)
   ("q" . selected-off)
   ("r" . reverse-region)
   ("s" . sort-lines)
   ("w" . kill-ring-save)))

(leaf server
  :require t
  :config
  (unless (server-running-p) (server-start)))

(leaf shackle
  :straight t
  :global-minor-mode t
  :custom
  (shackle-rules .
   '(("*Warnings*" :size 0.3)
     (Buffer-menu-mode :align t :size 0.2 :select t)
     (grep-mode :align t :size 0.3 :select t)
     (help-mode :align right :size 72 :select t)
     (xref--xref-buffer-mode :align t :size 0.3))))

(leaf shrink-whitespace
  :straight t
  :bind
  ([remap cycle-spacing] . shrink-whitespace))

(leaf simple
  :global-minor-mode column-number-mode
  :hook
  (before-save-hook . delete-trailing-whitespace)
  :custom
  (kill-whole-line . t)
  (set-mark-command-repeat-pop . t))

(leaf solarized-theme
  :straight t
  :config
  (load-theme 'solarized-dark t))

(leaf subword
  :hook
  (haskell-mode-hook js-base-mode-hook nix-mode-hook ruby-base-mode-hook))

(leaf string-inflection
  :straight t
  :config
  (defun my/string-inflection-for-ruby ()
    (keymap-local-set "C-c C-u" 'string-inflection-ruby-style-cycle))
  (defun my/string-inflection-for-java ()
    (keymap-local-set "C-c C-u" 'string-inflection-java-style-cycle))
  :bind
  ("C-c C-u" . string-inflection-all-cycle)
  :hook
  (ruby-base-mode-hook . my/string-inflection-for-ruby)
  (java-mode-hook . my/string-inflection-for-java))

(leaf terminal
  :custom
  (ring-bell-function . (lambda () (princ "[RING] "))))

(leaf terraform-mode
  :straight t)

(leaf textile-mode
  :straight t
  :mode "\\.textile\\'")

(leaf time
  :init
  (defun my/display-init-time ()
    (message "init time: %s" (emacs-init-time)))
  :hook
  (after-init-hook . my/display-init-time))

(leaf tool-bar
  :custom
  (tool-bar-mode . nil))

(leaf treesit-auto
  :straight t
  :global-minor-mode global-treesit-auto-mode
  :custom
  (treesit-auto-install . 'prompt))

(leaf uniquify
  :custom
  (uniquify-buffer-name-style . 'reverse))

(leaf vertico
  :straight (vertico :files (:defaults "extensions/vertico-repeat.el"))
  :global-minor-mode t
  :bind
  ("C-c r" . vertico-repeat)
  :hook
  (minibuffer-setup-hook . vertico-repeat-save)
  :config
  (setq completion-ignore-case t)) ; not a customization variable

(leaf vterm
  :straight t
  :custom
  (vterm-buffer-name-string . "vterm: %s")
  (vterm-kill-buffer-on-exit . t)
  (vterm-module-cmake-args . "-DCMAKE_PREFIX_PATH=~/.nix-profile"))

(leaf vterm-toggle
  :straight t
  :bind
  ("C-c v" . vterm-toggle)
  :custom
  (vterm-toggle-scope . 'project)
  :push
  ((shackle-rules . '(vterm-mode :align t :size 0.5))))

(leaf vue-mode
  :straight t)

(leaf vundo
  :straight t
  :bind
  ("C-c u" . vundo))

(leaf web-mode
  :straight t
  :custom
  (web-mode-css-indent-offset . 2)
  (web-mode-markup-indent-offset . 2))

(leaf which-key
  :global-minor-mode t
  :blackout t)

(leaf whitespace
  :global-minor-mode global-whitespace-mode
  :blackout global-whitespace-mode
  :config
  (dolist (style '(newline-mark lines tabs empty)) (delete style whitespace-style))
  (customize-set-variable 'whitespace-display-mappings
                          (cons '(space-mark ?\u3000 [?\u25a1])
                                (seq-remove (lambda (x) (equal (seq-take x 2) '(space-mark ?\ ))) whitespace-display-mappings)))
  :custom
  (whitespace-global-modes . '(not vterm-mode magit-log-mode magit-status-mode)))

(leaf windmove
  :leaf-defer nil
  :config
  (windmove-default-keybindings)
  :hook
  ;; Make windmove work in org-mode
  (org-shiftup-final-hook . windmove-up)
  (org-shiftleft-final-hook . windmove-left)
  (org-shiftdown-final-hook . windmove-down)
  (org-shiftright-final-hook . windmove-right))

(leaf window
  :config
  (dolist (f '(split-window-below split-window-right delete-window))
    (advice-add f :after (lambda (&rest _) (balance-windows)))))

(leaf winner
  :global-minor-mode t)

(leaf yaml-mode
  :straight t)

(leaf yasnippet
  :straight t
  :global-minor-mode yas-global-mode
  :blackout yas-minor-mode)

(provide 'init)
;;; init.el ends here
