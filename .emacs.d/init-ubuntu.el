;;; init-ubuntu --- emacs config file for ubuntu

;;; Commentary:
;;; aaaa

;;; Code:

(let ((home (getenv "HOME")))
  (load (concat home "/dotfiles/.emacs.d/init-common.el")))

(server-start)

;; language settings
(set-language-environment 'Japanese)
(prefer-coding-system 'utf-8)

;; add the directories under site-lisp to load path
(let ((default-directory "~/.emacs.d/site-lisp/"))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

;; requireの前にライブラリの存在検査を追加するアドバイス
(defadvice require (around require-if-exists)
  "Check if library file exists before require."
  (when (locate-library (symbol-name (ad-get-arg 0)))
    ad-do-it))
(ad-enable-advice 'require 'around 'require-if-exists)
(ad-activate 'require)

;; add repositories
(require 'package)
(add-to-list 'package-archives (cons "melpa" "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives (cons "marmalade" "http://marmalade-repo.org/packages/"))
(package-initialize)

;; exec-path の設定
(exec-path-from-shell-initialize)

;; global keybinds

; want to use C-h for DEL
(global-set-key (kbd "C-x ?") 'help-command)
(global-set-key (kbd "C-h") 'delete-backward-char)

(global-set-key (kbd "C-x a r") 'align-regexp)


;; move backup files to .emacs.d/backup/
(setq make-backup-files t)
(add-to-list 'backup-directory-alist (cons "\\.*$" (expand-file-name "~/.emacs.d/backup")))

;; window settings
(setq scroll-conservatively 35
      scroll-margin 0
      scroll-step 1)

(tool-bar-mode 0)

;; show completion candidates when choose one from buffers
(iswitchb-mode t)
;(iswitchb-default-keybindings)

;; kill whole line if killed at top of line
(setq kill-whole-line t)

;; don't use tab
(setq-default indent-tabs-mode nil)

;; delete trailing whitespaces when saved
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; show trailing whitespaces
(setq-default show-trailing-whitespace t)
(add-hook 'term-mode-hook
          (lambda () (setq show-trailing-whitespace nil)))
(add-hook 'undo-tree-visualizer-mode-hook
          (lambda () (setq show-trailing-whitespace nil)))
(add-hook 'Buffer-menu-mode-hook
          (lambda () (setq show-trailing-whitespace nil)))

;; if the file begins with #!, chmod +x after saving.
(add-hook 'after-save-hook
	  'executable-make-buffer-file-executable-if-script-p)

;; add final newline when saved (t)
(setq require-final-newline t)

; yes/no to y/n
(fset 'yes-or-no-p 'y-or-n-p)

; show column number
(column-number-mode t)

; 同一ファイル名のバッファの区別
(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)

;--------------------------------------------------------------------------------
;; maxframe

(when (memq window-system '(ns x)) ; cocoa, carbon -> mac, terminal -> nil, X -> x
  (require 'maxframe)
  (add-hook 'window-setup-hook          'maximize-and-split t)  ; when startup
  (add-hook 'after-make-frame-functions 'maximize-and-split t)) ; when make-frame

(defun maximize-and-split (&optional frame)
  "Maximize the window and split it horizontally into two buffers.
Optionally takes FRAME for its target and works on current frame if nothing given."
  (maximize-frame frame)
  (if frame
      (select-window (frame-root-window frame)))
  (split-window-horizontally))

;--------------------------------------------------------------------------------
;; AUCTeX

;(load "preview-latex" nil t t)
(eval-after-load "tex-mode"
  '(progn
     (setq TeX-engine-alist
           '((ptex "pTeX" "eptex -kanji=utf8 -guess-input-enc"
                   "platex -kanji=utf8 -guess-input-enc" "eptex")
             (jtex "jTeX" "jtex" "jlatex" nil)
             (uptex "upTeX" "euptex -kanji=utf8 -no-guess-input-enc"
                    "uplatex -kanji=utf8 -no-guess-input-enc" "euptex")))
     (setq TeX-engine 'ptex)))

;; zencoding-mode
;(require 'zencoding-mode)
(add-hook 'sgml-mode-hook 'zencoding-mode)
(eval-after-load "zencoding-mode"
  '(progn
     (define-key zencoding-mode-keymap (kbd "C-c C-m") 'zencoding-expand-line)
     (define-key zencoding-preview-keymap (kbd "C-c C-m") 'zencoding-preview-accept)))

;; popwin
(when (require 'popwin)
  (popwin-mode 1))
;(require 'popwin-term)

(eval-after-load "popwin"
  '(progn
     (push '("\\*magit" :regexp t :height 0.5) popwin:special-display-config)
     (push '("\\*helm" :regexp t) popwin:special-display-config)
     (push '("*GHC Info*" :height 10) popwin:special-display-config)
     (push '(" *undo-tree*" :width 0.1 :position right) popwin:special-display-config)
     (push '(term-mode :position :top :height 16 :stick t) popwin:special-display-config)))


;; rails
;(global-rinari-mode)
(require 'rhtml-mode nil t)
(add-hook 'rhtml-mode-hook '(lambda () (rinari-launch)))

;; ruby
(let* ((ruby-files
        '(".jbuilder" ".rake" ".thor" "Gemfile" "Rakefile" "Crushfile" "Capfile" "Gemfile" "Guardfile"))
       (ruby-regexp (concat (regexp-opt ruby-files t) "\\'")))
  (add-to-list 'auto-mode-alist (cons ruby-regexp 'ruby-mode)))
(autoload 'inf-ruby "inf-ruby" "Run an inferior Ruby process" t)
;(autoload 'inf-ruby-setup-keybindings "inf-ruby" "" t)

(defun ruby-mode-hookee ()
  "Hookee for 'ruby-mode'."
  (inf-ruby-minor-mode)
  (ruby-end-mode)
  (ruby-interpolation-mode)
  ;(inf-ruby-setup-keybindings)
  ;(abbrev-mode 1)
  ;(electric-pair-mode t)
  (electric-indent-mode t)
  (electric-layout-mode t))

(eval-after-load "ruby-mode"
  '(progn
     (setq ruby-insert-encoding-magic-comment nil)
     (setq ruby-deep-indent-paren nil)
     (add-hook 'ruby-mode-hook 'ruby-mode-hookee)))

(autoload 'rubydb "rubydb3x" "ruby debug" t)

;; helm
;(helm-mode 1)
(global-set-key (kbd "C-c h") 'helm-mini)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-c r") 'helm-projectile)

; helm-c-source-files-in-current-dir
; helm-mini は buffer と recentf と not-found
; helm-imenu
;; (let ((key-and-func
;;        `((,(kbd "C-r")   helm-for-files)
;;          (,(kbd "C-^")   helm-c-apropos)
;;          (,(kbd "C-;")   helm-resume)
;;          (,(kbd "M-s")   helm-occur)
;;          (,(kbd "M-x")   helm-M-x)
;;          (,(kbd "M-y")   helm-show-kill-ring)
;;          (,(kbd "M-z")   helm-do-grep)
;;          (,(kbd "C-S-h") helm-descbinds)
;;         )))
;;   (loop for (key func) in key-and-func
;;         do (global-set-key key func)))


;; auto-complete
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/dict")
(ac-config-default)
(global-auto-complete-mode t)
(setq ac-ignore-case nil)

;; color-theme
(when (require 'color-theme)
  (color-theme-initialize)
  (when (require 'color-theme-solarized)
    (cond
     ((eq system-type 'darwin)
      (color-theme-solarized-dark))
     (t
      (color-theme-solarized-light)))))

;; move along windows
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;; rainbow mode
(let ((hooks '(css-mode-hook scss-mode-hook html-mode-hook lisp-mode-hook)))
  (dolist (hook hooks)
    (add-hook hook (lambda () (rainbow-mode 1)))))

;; coffee-mode
(defun coffee-mode-hookee ()
  "Hookee for coffee-mode."
  (and (setq tab-width 2) (setq coffee-tab-width 2))
  (auto-complete-mode))
(add-hook 'coffee-mode-hook 'coffee-mode-hookee)

;; scss
(defun scss-sass-mode-hookee ()
  "Hookee for scss-mode and sass-mode."
  (setq css-indent-offset 2))

(dolist (hook '(sass-mode-hook scss-mode-hook))
  (add-hook hook 'scss-sass-mode-hookee))

;; flymake
;; (defun flymake-mode-hooks ()
;;   (global-set-key (kbd "M-n") 'flymake-goto-next-error)
;;   (global-set-key (kbd "M-p") 'flymake-goto-prev-error)
;;   (global-set-key (kbd "M-?") 'flymake-display-err-menu-for-current-line))
;; (add-hook 'flymake-mode-hook 'flymake-mode-hooks)

;; flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)
(eval-after-load 'flycheck
  '(define-key flycheck-mode-map (kbd "C-c !") 'helm-flycheck))

;; flycheck-tip
;(eval-after-load "flycheck-tip"
(global-set-key (kbd "M-n") 'flycheck-tip-cycle)


;; js2 mode
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(setq ac-js2-evaluate-calls t)

;; yasnippet
;(yas-global-mode 1)

;; haskell
(autoload 'ghc-init "ghc" nil t)

(add-hook 'haskell-mode 'haskell-mode-hooks)

(defun haskell-mode-hooks ()
  "Hookee for haskell-mode."
  (haskell-indentation-mode 1)
  (flycheck-mode -1) ; to cancel global flycheck mode
  (ghc-init))

(add-to-list 'ac-sources 'ac-source-ghc-mod)

(eval-after-load "haskell-mode"
  '(progn
     (define-key haskell-mode-map (kbd "C-x C-d") nil)
     (define-key haskell-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
     (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-file)
     (define-key haskell-mode-map (kbd "C-c C-b") 'haskell-interactive-switch)
     ;(define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
     ;(define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)
     (define-key haskell-mode-map (kbd "C-c M-.") nil)
     (define-key haskell-mode-map (kbd "C-c C-d") nil)))


;; markdown
(autoload 'markdown-mode "markdown-mode.el" "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown" . markdown-mode))


;; scala
;; load the ensime lisp code...
(require 'ensime)

;; This step causes the ensime-mode to be started whenever
;; scala-mode is started for a buffer. You may have to customize this step
;; if you're not using the standard scala mode.
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)

;; undo tree
(require 'undo-tree)
(global-undo-tree-mode)

;; malabar-mode
(setq semantic-default-submodes '(global-semantic-idle-scheduler-mode
                                  global-semanticdb-minor-mode
                                  global-semantic-idle-summary-mode
                                  global-semantic-mru-bookmark-mode))
(semantic-mode 1)
(require 'malabar-mode)
(setq malabar-groovy-lib-dir "/path/to/malabar/lib")
(add-to-list 'auto-mode-alist '("\\.java\\'" . malabar-mode))

;; cacoo mode
(require 'cacoo)
(require 'cacoo-plugins)      ; option
;(setq cacoo:api-key "APIKEY") ; option
(global-set-key (kbd "M--") 'toggle-cacoo-minor-mode) ; key bind example

;; open junk file
(require 'open-junk-file)
(setq open-junk-file-format "~/.emacs.d/junk/%Y/%m/%d-%H%M%S.")


;; raibow delimiters
(if (require 'rainbow-delimiters)
    (progn
      ; 文字列の色と被るため変更
      (custom-set-faces
       ;; custom-set-faces was added by Custom.
       ;; If you edit it by hand, you could mess it up, so be careful.
       ;; Your init file should contain only one such instance.
       ;; If there is more than one, they won't work right.
       '(rainbow-delimiters-depth-1-face ((t (:foreground "#7f8c8d")))))
      (global-rainbow-delimiters-mode)))

;; projectile
(eval-after-load 'projectile
  '(projectile-global-mode))

;; magit
(global-set-key (kbd "C-c g") 'magit-status)



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(haskell-mode-hook (quote haskell-mode-hooks))
 '(haskell-process-type (quote cabal-repl))
 '(open-junk-file-find-file-function (quote find-file))
 '(ruby-deep-indent-paren nil)
 '(scss-compile-at-save nil)
 '(setq recentf-max-saved-items)
 '(use-dialog-box nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(provide 'init-ubuntu)
;;; init-ubuntu.el ends here
