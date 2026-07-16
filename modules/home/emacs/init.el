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
  (leaf-keywords-init)
  :custom
  (leaf-alias-keyword-alist . '((:ensure . :straight))))

;; for leaf's blackout keyword
(leaf blackout
  :ensure t)

;; make an effect early
(leaf gcmh
  :ensure t
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
    (ansi-color-apply-on-region (point-min) (point-max)))
  :hook
  (compilation-filter-hook . ansi-color-compilation-filter))

(leaf apib-mode
  :ensure t
  :mode "\\.apib\\'")

(leaf auto-side-windows
  :ensure (auto-side-windows :host github :repo "MArpogaus/auto-side-windows")
  :hook
  (after-init-hook . auto-side-windows-mode)
  :bind
  ("C-c q" . window-toggle-side-windows)
  (window-prefix-map
   ("b" . auto-side-windows-switch-to-buffer))
  :init
  (defun my/side-window-indicator ()
    (if (window-parameter (selected-window) 'window-side) "[⊞] " ""))
  (defun my/side-windows-resync-preserve (&rest _)
    (dolist (window (window-list))
      (pcase (window-parameter window 'window-side)
        ((or 'left 'right) (window-preserve-size window t t))
        ((or 'top 'bottom) (window-preserve-size window nil t)))))
  :config
  (add-to-list 'mode-line-misc-info '(:eval (my/side-window-indicator)) t)
  (add-to-list 'window-persistent-parameters '(window-preserved-size . t))
  (advice-add 'window-toggle-side-windows :after #'my/side-windows-resync-preserve)
  :custom
  (auto-side-windows-common-alist . '((preserve-size . (t . t))))
  (auto-side-windows-common-window-parameters . '((no-other-window . t)))
  (auto-side-windows-bottom-alist
   . '((window-height . (lambda (window) (fit-window-to-buffer window 0.5 0.25)))))
  (auto-side-windows-bottom-buffer-modes . '(ghostel-mode grep-mode magit-status-mode))
  (auto-side-windows-bottom-buffer-names . '("\\*eldoc\\*" "\\*scratch\\*" "\\*Warnings\\*"))
  (auto-side-windows-right-buffer-modes . '(help-mode helpful-mode))
  (switch-to-buffer-obey-display-actions . t)
  (windmove-allow-all-windows . t)
  (window-sides-slots . '(1 1 1 2))
  (window-sides-vertical . t))

(leaf autorevert
  :blackout auto-revert-mode)

(leaf avy
  :ensure t
  :config
  (avy-setup-default))

(leaf balanced-windows
  :ensure (balanced-windows :host github :repo "elp-revive/balanced-windows")
  :require t
  :config
  (add-to-list 'balanced-windows-commands #'split-window-below)
  (add-to-list 'balanced-windows-commands #'split-window-right)
  (balanced-windows-mode))

(leaf browse-at-remote
  :ensure t)

(leaf cabal-mode
  :ensure t)

(leaf cape
  :ensure t
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
  (java-ts-mode-hook . my/indent-by-two))

(leaf claude-code-ide
  :ensure (claude-code-ide :host github :repo "manzaltu/claude-code-ide.el")
  :bind ("C-c C-'" . claude-code-ide-menu)
  :config
  (claude-code-ide-emacs-tools-setup)
  :custom
  (claude-code-ide-terminal-backend . 'ghostel))

(leaf coffee-mode
  :ensure t
  :custom
  (coffee-tab-width . 2))

(leaf comp-run
  :custom
  (native-comp-async-report-warnings-errors . nil))

(leaf compile
  :custom
  (compilation-scroll-output . t))

(leaf completion-preview
  :global-minor-mode global-completion-preview-mode
  :blackout t
  :bind
  (completion-preview-active-mode-map
   ("<tab>" . completion-preview-complete)
   ("<return>" . completion-preview-insert))
  :custom
  (completion-preview-minimum-symbol-length . 2)
  :custom-face
  ;; Solarized green
  (completion-preview-exact . '((t :underline "#859900" :inherit completion-preview-common))))

(leaf consult
  :ensure t
  :bind
  ("C-c C-r" . consult-recent-file)
  ([remap goto-line] . consult-goto-line)
  ([remap imenu] . consult-imenu)
  ([remap project-find-regexp] . consult-git-grep)
  ([remap switch-to-buffer] . consult-buffer)
  ([remap yank-pop] . consult-yank-pop)
  (flymake-mode-map
   ("C-c !" . consult-flymake))
  :custom
  (xref-show-definitions-function . #'consult-xref)
  (xref-show-xrefs-function . #'consult-xref))

(leaf consult-dir
  :ensure t
  :bind
  ("C-x C-d" . consult-dir)
  (vertico-map
   ("C-x C-d" . consult-dir)
   ("C-x C-j" . consult-dir-jump-file)))

(leaf consult-hoogle
  :ensure t)

(leaf corfu
  :ensure t
  :global-minor-mode global-corfu-mode corfu-history-mode corfu-popupinfo-mode)

(leaf css-ts-mode
  :custom
  (css-indent-offset . 2))

(leaf ctrlf
  :ensure t
  :global-minor-mode t
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
  :ensure t)

(leaf dabbrev
  :bind
  (("M-/" . dabbrev-completion)
   ("C-M-/" . dabbrev-expand))
  :custom
  (dabbrev-case-replace . nil))

(leaf delsel
  :global-minor-mode delete-selection-mode)

(leaf desktop
  :global-minor-mode desktop-save-mode
  :custom
  (desktop-load-locked-desktop . t)
  (desktop-restore-eager . 10))

(leaf dhall-mode
  :ensure t)

(leaf diff-hl
  :ensure t
  :global-minor-mode global-diff-hl-mode
  :hook
  (dired-mode-hook . diff-hl-dired-mode))

(leaf dired
  :custom
  (dired-dwim-target . t))

(leaf dmacro
  :ensure t
  :global-minor-mode global-dmacro-mode
  :blackout t
  :custom
  (dmacro-key . `,(kbd "C-c d")))

(leaf dumb-jump
  :ensure t
  :hook
  (xref-backend-functions . dumb-jump-xref-activate))

(leaf ediff
  :custom
  (ediff-split-window-function . 'split-window-horizontally)
  (ediff-window-setup-function . 'ediff-setup-windows-plain))

(leaf eglot
  :ensure (eglot :source gnu-elpa-mirror)
  :hook
  ((haskell-ts-mode-hook js-base-mode-hook nix-ts-mode-hook ruby-base-mode-hook scala-mode-hook sh-mode-hook yaml-ts-mode-hook) . eglot-ensure)
  :bind
  (eglot-mode-map
   ("C-c e" . 'eglot-code-actions)))

(leaf eglot-tempel
  :ensure t
  :global-minor-mode t)

(leaf eldoc
  :custom
  (eldoc-minor-mode-string . nil))

(leaf elec-pair
  :global-minor-mode electric-pair-mode)

(leaf elm-mode
  :ensure t)

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
  :ensure t
  :bind
  ("C-." . embark-act))

(leaf embark-consult
  :ensure t)

(leaf emmet-mode
  :ensure t
  :hook
  (sgml-mode-hook css-base-mode-hook)
  :custom
  (emmet-move-cursor-between-quotes . t))

(leaf envrc
  :ensure t
  :hook
  (after-init-hook . envrc-global-mode))

(leaf exec-path-from-shell
  :ensure t
  :require t
  :config
  (exec-path-from-shell-initialize)
  :push
  ((exec-path-from-shell-variables . "NIX_SSL_CERT_FILE")))

(leaf executable
  :hook
  (after-save-hook . executable-make-buffer-file-executable-if-script-p))

(leaf expreg
  :ensure t
  :setq
  (expreg-restore-point-on-quit . t)
  :bind
  ("C-=" . expreg-expand)
  :config
  (defvar-keymap expreg-repeat-map
    :repeat (:hints ((expreg-expand . "expand") (expreg-contract . "contract")))
    "=" #'expreg-expand
    "-" #'expreg-contract))

(leaf files
  :config
  (add-to-list 'backup-directory-alist `("\\.*$" . ,(expand-file-name "backup" user-emacs-directory)))
  (defun my/reload-init-file ()
    "Reload user's initialization file."
    (interactive)
    (load-file user-init-file))
  :custom
  (backup-by-copying . t)
  (confirm-kill-emacs . 'y-or-n-p)
  (remote-file-name-inhibit-cache . 600)
  (require-final-newline . 'visit))

(leaf fish-mode
  :ensure t)

(leaf flymake
  :hook prog-mode-hook
  :custom
  (flymake-show-diagnostics-at-end-of-line . 'fancy))

(leaf frame
  :global-minor-mode blink-cursor-mode
  :custom
  (blink-cursor-blinks . 0)
  (default-frame-alist . '((fullscreen . fullboth)
                           (font . "Cica-14")
                           (vertical-scroll-bars . nil))))

(leaf free-keys
  :ensure t)

(leaf ghostel
  :ensure t
  :bind
  ("C-c v" . ghostel-project)
  :config
  (add-to-list 'project-switch-commands '(ghostel-project "Ghostel" "v") t)
  (add-to-list 'project-switch-commands '(ghostel-project-list-buffers "Ghostel buffers" "V") t))

(leaf git-timemachine
  :ensure t)

(leaf google-this
  :ensure t
  :global-minor-mode t
  :blackout t)

(leaf goto-addr
  :hook
  (prog-mode-hook . goto-address-prog-mode)
  (text-mode-hook . goto-address-mode))

(leaf groovy-mode
  :ensure t)

(leaf haml-mode
  :ensure t
  :mode "\\.hamlc\\'")

(leaf haskell-ts-mode
  :ensure t
  :blackout "Haskell"
  :bind
  (haskell-ts-mode-map
   ;; Move off the default `C-c C-c` to leave it for `duplicate-dwim`
   ("C-c C-c" . duplicate-dwim)
   ("C-c C-m" . haskell-ts-compile-region-and-go)))

(leaf helpful
  :ensure t
  :bind
  ([remap describe-function] . helpful-callable)
  ([remap describe-key] . helpful-key)
  ([remap describe-variable] . helpful-variable)
  (help-map
   :package emacs
   ("C-." . helpful-at-point)))

(leaf highlight-indent-guides
  :ensure t
  :blackout t
  :hook prog-mode-hook yaml-ts-mode-hook
  :custom
  (highlight-indent-guides-method . 'bitmap)
  (highlight-indent-guides-bitmap-function . 'highlight-indent-guides--bitmap-line)
  (highlight-indent-guides-auto-character-face-perc . 100))

(leaf hledger-mode
  :ensure t
  :mode "\\.journal\\'"
  :config
  (defun my/hledger-set-tab-width ()
    (setq tab-width 4))
  :hook
  (hledger-mode-hook . my/hledger-set-tab-width))

(leaf json-ts-mode
  :mode ("\\.json\\'" "\\.jsonc\\'"))

(leaf magit
  :ensure t
  :global-minor-mode global-git-commit-mode
  :bind
  (project-prefix-map
   ("m" . magit-status-here))
  :custom
  (magit-diff-refine-hunk . 'all))

(leaf markdown-mode
  :ensure t
  :mode
  ("README\\.md\\'" . gfm-mode)
  :custom
  (markdown-fontify-code-blocks-natively . t))

(leaf marginalia
  :ensure t
  :global-minor-mode t
  :bind
  (minibuffer-local-map
   ("M-A" . marginalia-cycle)))

(leaf mb-depth
  :global-minor-mode minibuffer-depth-indicate-mode)

(leaf midnight
  :global-minor-mode t)

(leaf misc
  :bind
  ("C-c C-c" . duplicate-dwim))

(leaf multiple-cursors
  :ensure t
  :bind
  ("C-c m e" . mc/edit-lines)
  ("C-c m n" . mc/mark-next-like-this))

(leaf mwim
  :ensure t
  :bind
  ([remap move-beginning-of-line] . mwim-beginning-of-line-or-code)
  ([rempa move-end-of-line] . mwim-end-of-line-or-code))

(leaf nael
  :ensure t
  :hook
  (nael-mode-hook . abbrev-mode)
  (nael-mode-hook . eglot-ensure))

(leaf nix-ts-mode
  :ensure t
  :mode "\\.nix\\'")

(leaf open-junk-file
  :ensure t
  :commands open-junk-file
  :custom
  (open-junk-file-format . `,(expand-file-name "junk/%Y/%m/%d-%H%M%S." user-emacs-directory))
  (open-junk-file-find-file-function . 'find-file))

(leaf orderless
  :ensure t
  :custom
  (completion-styles . '(orderless basic))
  (completion-category-overrides . '((file (styles basic partial-completion)))))

(leaf paren
  :custom
  (show-paren-mode . nil))

(leaf peep-dired
  :ensure t
  :bind
  (dired-mode-map
   ("C-x x" . peep-dired))
  (peep-dired-mode-map
   ("C-x x" . peep-dired)))

(leaf pulsar
  :ensure t
  :global-minor-mode pulsar-global-mode
  :custom
  (pulsar-pulse-region-functions . '(duplicate-dwim undo yank yank-rectangle)))

(leaf purescript-mode
  :ensure t
  :hook
  (purescript-mode-hook . turn-on-purescript-indentation))

(leaf rainbow-delimiters
  :ensure t
  :hook prog-mode-hook)

(leaf rainbow-mode
  :ensure t
  :hook
  (css-base-mode-hook html-mode-hook lisp-mode-hook web-mode-hook))

(leaf recentf
  :global-minor-mode t
  :custom
  (recentf-max-saved-items . 100)
  (recentf-save-file . `,(expand-file-name "recentf" user-emacs-directory)))

(leaf repeat-mode
  :global-minor-mode t)

(leaf ruby-ts-mode
  :mode "\\.cap\\'" "\\.Brewfile\\'"
  :custom
  (ruby-insert-encoding-magic-comment . nil))

(leaf ruby-end
  :ensure t
  :blackout t)

(leaf ruby-hash-syntax
  :ensure t)

(leaf ruby-interpolation
  :ensure t
  :blackout t
  :hook
  ruby-base-mode-hook)

(leaf savehist
  :global-minor-mode t)

(leaf scala-mode
  :ensure t)

(leaf selected
  :ensure t
  :blackout selected-minor-mode
  :hook
  ((prog-mode-hook text-mode-hook) . selected-minor-mode)
  :setq
  (selected-minor-mode-override . t)
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
  :global-minor-mode server-mode)

(leaf shrink-whitespace
  :ensure t
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
  :ensure t
  :config
  (load-theme 'solarized-dark t))

(leaf subword
  :hook
  (haskell-ts-mode-hook js-base-mode-hook nix-ts-mode-hook ruby-base-mode-hook))

(leaf string-inflection
  :ensure t
  :config
  (defun my/string-inflection-for-ruby ()
    (keymap-local-set "C-c C-u" 'string-inflection-ruby-style-cycle))
  (defun my/string-inflection-for-java ()
    (keymap-local-set "C-c C-u" 'string-inflection-java-style-cycle))
  :bind
  ("C-c C-u" . string-inflection-all-cycle)
  :hook
  (ruby-base-mode-hook . my/string-inflection-for-ruby)
  (java-ts-mode-hook . my/string-inflection-for-java))

(leaf tempel
  :ensure t
  :init
  :hook
  ((conf-mode-hook prog-mode-hook text-mode-hook)
   . (lambda () (add-hook 'completion-at-point-functions #'tempel-complete -10 t))))

(leaf tempel-collection
  :ensure t)

(leaf terminal
  :custom
  (ring-bell-function . (lambda () (princ "[RING] "))))

(leaf terraform-mode
  :ensure t)

(leaf textile-mode
  :ensure t
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

(leaf treesit
  :custom
  (treesit-font-lock-level . 4))

(leaf treesit-fold
  :ensure t
  :global-minor-mode global-treesit-fold-mode
  :blackout t
  :bind
  ("C-c f" . treesit-fold-toggle)
  :hook
  (emacs-lisp-mode-hook . (lambda () (treesit-parser-create 'elisp))))

(leaf uniquify
  :custom
  (uniquify-buffer-name-style . 'reverse))

(leaf vc-jj
  :ensure t)

(leaf vertico
  :ensure
  (vertico
   :files
   (:defaults
    "extensions/vertico-directory.el"
    "extensions/vertico-repeat.el"
    "extensions/vertico-sort.el"))
  :global-minor-mode t
  :bind
  ("C-c r" . vertico-repeat)
  (vertico-map
   ("DEL" . #'vertico-directory-delete-char))
  :hook
  (minibuffer-setup-hook . vertico-repeat-save)
  :config
  (setq completion-ignore-case t)) ; not a customization variable

(leaf visual-fill-column
  :ensure t)

(leaf vue-mode
  :ensure t)

(leaf vundo
  :ensure t
  :bind
  ("C-c u" . vundo))

(leaf web-mode
  :ensure t
  :custom
  (web-mode-css-indent-offset . 2)
  (web-mode-markup-indent-offset . 2))

(leaf which-key
  :global-minor-mode t
  :blackout t)

(leaf whitespace
  :global-minor-mode global-whitespace-mode
  :blackout t
  :config
  (dolist (style '(newline-mark lines tabs empty)) (delete style whitespace-style))
  (customize-set-variable 'whitespace-display-mappings
                          (cons '(space-mark ?\u3000 [?\u25a1])
                                (seq-remove (lambda (x) (equal (seq-take x 2) '(space-mark ?\ ))) whitespace-display-mappings)))
  :custom
  (whitespace-global-modes . '(not magit-log-mode magit-status-mode)))

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

(leaf winner
  :global-minor-mode t)

(provide 'init)
;;; init.el ends here
