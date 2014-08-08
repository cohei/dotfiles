(let ((home (getenv "HOME")))
  (load (concat home "/dotemacs/init-common.el")))


;; パス関係
; set load-path to all subdirectoies under ~/.emacs.d/site-lisp

; なんでかだめ
; (load (expand-file-name "~/.emacs.d/site-lisp/subdirs.el"))

(let ((default-directory (expand-file-name "~/.emacs.d/site-lisp")))
  (if (file-exists-p default-directory)
      (progn
        (add-to-list 'load-path default-directory)
        (load (expand-file-name "~/.emacs.d/site-lisp/subdirs.el")))))

;; (let ((default-directory (expand-file-name "~/.emacs.d/site-lisp")))
;;   (add-to-list 'load-path default-directory)
;;   (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
;;       (normal-top-level-add-subdirs-to-load-path)))

; .emacs.d/infoをC-h iで表示されるInfoに追加
(add-to-list 'Info-default-directory-list (expand-file-name "~/.emacs.d/info"))

(require 'exec-path-from-shell) ;; if not using the ELPA package
(exec-path-from-shell-initialize)



; requireの前にライブラリの存在検査を追加するアドバイス
(defadvice require (around require-if-exists)
  "Check if library file exists before require."
  (when (locate-library (symbol-name (ad-get-arg 0)))
    ad-do-it))
(ad-enable-advice 'require 'around 'require-if-exists)

; 言語関係
(set-language-environment 'Japanese)
(prefer-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)

; リージョンに色付け
(setq-default transient-mark-mode t)

; 使えるモード全てでfont-lockを有効に
(global-font-lock-mode t)

; if the file begins with #!, chmod +x after saving.
(add-hook 'after-save-hook
	  'executable-make-buffer-file-executable-if-script-p)

; 行ががはみ出したときに折り返す
(setq truncate-partial-width-windows nil)

; 一行ずつスクロール
(setq scroll-step 1)

; 行すべてをkill-lineするときに改行も含める
(setq kill-whole-line t)

; ファイル末尾に改行がないとき、つける
(setq require-final-newline t)

; yes/no to y/n
(fset 'yes-or-no-p 'y-or-n-p)

;; move along windows
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

; 同一ファイル名のバッファの区別
(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)

; キーバインドのある関数を使ったときに教えてくれる
(setq suggest-key-bindings t)



(when window-system
  (setq ring-bell-function 'ignore)
  ;(set-frame-parameter nil 'alpha 95)
  (set-background-color "Black")
  (set-foreground-color "White")
  (set-cursor-color "Gray")

  (require 'color-theme)
  (color-theme-initialize)
  (load "color-theme-solarized")
  (color-theme-solarized-dark))


;; aquaSKKで漢字が入力できるように
(when (boundp 'mac-input-method-parameters)
  (mac-input-method-mode 1))

;--------------------------------------------------------------------------------
;; Multi term

(require 'multi-term)
(setq multi-term-program "/bin/bash")

; すでにterminalがあるときはそれを開く
(defun start-bash-multi-term ()
  (interactive)
  (if (get-buffer "*terminal<1>*")
      (switch-to-buffer "*terminal<1>*")
      (multi-term)))

(global-set-key (kbd "C-c t") 'start-bash-multi-term)

;; term の ls の日本語が化けないように
(if (or (eq window-system 'mac) (eq window-system 'ns))
    ;; Mac OS X の HFS+ ファイルフォーマットではファイル名は
    ;; NFD (の様な物)で扱うため以下の設定をする必要がある
    (progn
      (require 'ucs-normalize)
      (setq file-name-coding-system 'utf-8-hfs)
      (setq locale-coding-system 'utf-8-hfs))
    (progn
      (setq file-name-coding-system 'utf-8)
      (setq locale-coding-system 'utf-8)))

;--------------------------------------------------------------------------------
;; maxframe

(when (eq window-system 'ns) ; cocoa, carbon -> mac, terminal -> nil, X -> x
  (require 'maxframe)
  (add-hook 'window-setup-hook 'maximize-frame t)
  (split-window-horizontally))

;--------------------------------------------------------------------------------
;; for haskell-mode

(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
; (add-hook 'haskell-mode-hook 'turn-on-haskell-hugs)
(add-hook 'haskell-mode-hook 'turn-on-haskell-ghci)
(add-hook 'haskell-mode-hook 'turn-on-haskell-decl-scan)
(add-hook 'haskell-mode-hook 'turn-on-haskell-font-lock)
; (add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)
; (add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
; (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation) ; 2.7.0 only

;; for ghc-mod

(autoload 'ghc-init "ghc" nil t)
(add-hook 'haskell-mode-hook (lambda () (ghc-init) (flymake-mode)))

;; offside trap
(autoload 'offside-trap-mode "offside-trap.el")

(add-hook
 'haskell-mode-hook
 '(lambda ()

    ;; Load one of you like.
    ; (turn-on-haskell-indentation)
    (turn-on-haskell-indent)

    ;; (offside-trap-mode) must be called after turn-on-haskell-indentation/turn-on-haskell-indent,
    ;; so that offside-trap-mode-map overrides haskell-indentation/indent-mode-map
    (offside-trap-mode)
    ))

;--------------------------------------------------------------------------------
;; backspace and help key bind

(global-set-key "\C-h" 'delete-backward-char)
(global-set-key "\C-x?" 'help-command)

;--------------------------------------------------------------------------------
;; coq-mode

(add-to-list 'auto-mode-alist (cons "\\.v$" 'coq-mode))
(autoload 'coq-mode "coq" "Major mode for editing Coq vernacular." t)
(autoload 'run-coq "coq-inferior" "Run an inferior Coq process." t)
(autoload 'run-coq-other-window "coq-inferior"
  "Run an inferior Coq process in a new window." t)
(autoload 'run-coq-other-frame "coq-inferior"
  "Run an inferior Coq process in a new frame." t)

;--------------------------------------------------------------------------------
;; php-mode

(when (locate-library "php-mode")
  (autoload 'php-mode "php-mode" nil t))

;(load-library "php-mode")
;(require 'php-mode)

;--------------------------------------------------------------------------------
;; dired

(eval-after-load "dired"
  '(progn
     (setq dired-recursive-copies 'always)
     (setq dired-recursive-deletes 'always)))

;--------------------------------------------------------------------------------
;; save backup files in a specific directory

(add-to-list 'backup-directory-alist (cons "." (expand-file-name "~/.emacs.d/bak/")))

;--------------------------------------------------------------------------------

(autoload 'ruby-mode "ruby-mode" "Major mode for ruby files" t)
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))

(autoload 'inf-ruby "inf-ruby" "Run an inferior Ruby process" t)
(autoload 'inf-ruby-setup-keybindings "inf-ruby" "" t)
(eval-after-load 'ruby-mode
  '(add-hook 'ruby-mode-hook 'inf-ruby-setup-keybindings))

(require 'flymake-ruby)
(add-hook 'ruby-mode-hook 'flymake-ruby-load)

(require 'ruby-end)

;--------------------------------------------------------------------------------
;; scheme-mode

(add-to-list 'process-coding-system-alist '("gosh" utf-8 . utf-8))
(setq scheme-program-name "/opt/local/bin/gosh -i")
(autoload 'scheme-mode "cmuscheme" "Major mode for Scheme." t)
(autoload 'run-scheme "cmuscheme" "Run an inferior Scheme process." t)
(defun scheme-other-window ()
  "Run scheme on other window"
  (interactive)
  (switch-to-buffer-other-window
   (get-buffer-create "*scheme*"))
  (run-scheme scheme-program-name))
(define-key global-map
  "\C-cS" 'scheme-other-window)

(put 'and-let* 'scheme-indent-function 1)
(put 'begin0 'scheme-indent-function 0)
(put 'call-with-client-socket 'scheme-indent-function 1)
(put 'call-with-input-conversion 'scheme-indent-function 1)
(put 'call-with-input-file 'scheme-indent-function 1)
(put 'call-with-input-process 'scheme-indent-function 1)
(put 'call-with-input-string 'scheme-indent-function 1)
(put 'call-with-iterator 'scheme-indent-function 1)
(put 'call-with-output-conversion 'scheme-indent-function 1)
(put 'call-with-output-file 'scheme-indent-function 1)
(put 'call-with-output-string 'scheme-indent-function 0)
(put 'call-with-temporary-file 'scheme-indent-function 1)
(put 'call-with-values 'scheme-indent-function 1)
(put 'dolist 'scheme-indent-function 1)
(put 'dotimes 'scheme-indent-function 1)
(put 'if-match 'scheme-indent-function 2)
(put 'let*-values 'scheme-indent-function 1)
(put 'let-args 'scheme-indent-function 2)
(put 'let-keywords* 'scheme-indent-function 2)
(put 'let-match 'scheme-indent-function 2)
(put 'let-optionals* 'scheme-indent-function 2)
(put 'let-syntax 'scheme-indent-function 1)
(put 'let-values 'scheme-indent-function 1)
(put 'let/cc 'scheme-indent-function 1)
(put 'let1 'scheme-indent-function 2)
(put 'letrec-syntax 'scheme-indent-function 1)
(put 'make 'scheme-indent-function 1)
(put 'multiple-value-bind 'scheme-indent-function 2)
(put 'match 'scheme-indent-function 1)
(put 'parameterize 'scheme-indent-function 1)
(put 'parse-options 'scheme-indent-function 1)
(put 'receive 'scheme-indent-function 2)
(put 'rxmatch-case 'scheme-indent-function 1)
(put 'rxmatch-cond 'scheme-indent-function 0)
(put 'rxmatch-if  'scheme-indent-function 2)
(put 'rxmatch-let 'scheme-indent-function 2)
(put 'syntax-rules 'scheme-indent-function 1)
(put 'unless 'scheme-indent-function 1)
(put 'until 'scheme-indent-function 1)
(put 'when 'scheme-indent-function 1)
(put 'while 'scheme-indent-function 1)
(put 'with-builder 'scheme-indent-function 1)
(put 'with-error-handler 'scheme-indent-function 0)
(put 'with-error-to-port 'scheme-indent-function 1)
(put 'with-input-conversion 'scheme-indent-function 1)
(put 'with-input-from-port 'scheme-indent-function 1)
(put 'with-input-from-process 'scheme-indent-function 1)
(put 'with-input-from-string 'scheme-indent-function 1)
(put 'with-iterator 'scheme-indent-function 1)
(put 'with-module 'scheme-indent-function 1)
(put 'with-output-conversion 'scheme-indent-function 1)
(put 'with-output-to-port 'scheme-indent-function 1)
(put 'with-output-to-process 'scheme-indent-function 1)
(put 'with-output-to-string 'scheme-indent-function 1)
(put 'with-port-locking 'scheme-indent-function 1)
(put 'with-string-io 'scheme-indent-function 1)
(put 'with-time-counter 'scheme-indent-function 1)
(put 'with-signal-handlers 'scheme-indent-function 1)
(put 'with-locking-mutex 'scheme-indent-function 1)
(put 'guard 'scheme-indent-function 1)

;--------------------------------------------------------------------------------
; smalltalk-mode

(add-to-list 'auto-mode-alist (cons "\\.st$" 'smalltalk-mode))
(autoload 'smalltalk-mode "smalltalk-mode" "" t)

;--------------------------------------------------------------------------------
; prolog-mode

(autoload 'run-prolog "prolog" "Start a Prolog sub-process." t)
(autoload 'prolog-mode "prolog" "Major mode for editing Prolog programs." t)
(autoload 'mercury-mode "prolog" "Major mode for editing Mercury programs." t)
(setq prolog-system 'swi)
(add-to-list 'auto-mode-alist (cons "\\.pl$" 'prolog-mode))
(add-to-list 'auto-mode-alist (cons "\\.m$"  'mercury-mode))

;--------------------------------------------------------------------------------
;; erlang-mode

(require 'erlang-start)
(add-hook 'erlang-mode-hook '(lambda () (setq indent-tabs-mode nil)))

;--------------------------------------------------------------------------------
;; ProofGeneral

;(add-to-list 'load-path
;	     (expand-file-name "~/.emacs.d/site-lisp/ProofGeneral-4.1pre110505"))
(require 'proof-site)

;--------------------------------------------------------------------------------
;; drill instructor

;(require 'drill-instructor)
;(setq drill-instructor-global t)
;(add-to-list 'drill-instructor-unset-major-mode-list '***-mode)

;--------------------------------------------------------------------------------
;; font setting

(when (and (>= emacs-major-version 23) window-system)
 (set-face-attribute 'default nil
                     :family "monaco"
                     :height 120)
 (set-fontset-font
  (frame-parameter nil 'font)
  'japanese-jisx0208
  '("Hiragino Kaku Gothic Pro" . "iso10646-1"))
 (set-fontset-font
  (frame-parameter nil 'font)
  'japanese-jisx0212
  '("Hiragino Kaku Gothic Pro" . "iso10646-1"))
 (set-fontset-font
  (frame-parameter nil 'font)
  'mule-unicode-0100-24ff
  '("monaco" . "iso10646-1"))
 (setq face-font-rescale-alist
      '(("^-apple-hiragino.*" . 1.2)
        (".*osaka-bold.*" . 1.2)
        (".*osaka-medium.*" . 1.2)
        (".*courier-bold-.*-mac-roman" . 1.0)
        (".*monaco cy-bold-.*-mac-cyrillic" . 0.9)
        (".*monaco-bold-.*-mac-roman" . 0.9)
        ("-cdac$" . 1.3))))


;--------------------------------------------------------------------------------
;; auto complete

(require 'auto-complete)
(global-auto-complete-mode t)

;--------------------------------------------------------------------------------
;; anything

(require 'anything)
(require 'anything-config)
(add-to-list 'anything-sources 'anything-c-source-emacs-commands)
(define-key global-map (kbd "C-c a") 'anything)

;--------------------------------------------------------------------------------
;; yatex

;; (autoload 'yatex-mode "yatex" "Yet Another LaTeX mode" t)
;; (add-to-list 'auto-mode-alist (cons "\\.tex$" 'yatex-mode))
;; (setq tex-command "platex")

;--------------------------------------------------------------------------------
;; AUCTeX

(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq TeX-default-mode 'japanese-latex-mode)
(setq-default TeX-master nil)

;--------------------------------------------------------------------------------
;; popwin

(require 'popwin)
(setq display-buffer-function 'popwin:display-buffer)

; for ghc flymake
(push '("GHC Info" :noselect t) popwin:special-display-config)

;--------------------------------------------------------------------------------
;; yasnippet

(require 'yasnippet)
(setq yas-snippet-dirs
  '("~/.emacs.d/snippets" ;; 作成するスニペットはここに入る
    "~/.emacs.d/site-lisp/yasnippet/snippets" ;; 最初から入っていたスニペット(省略可能)
    ))
(yas-global-mode 1)
(custom-set-variables '(yas-trigger-key "TAB"))

;--------------------------------------------------------------------------------
;; ajc-java-complete

(require 'ajc-java-complete-config)
;(setq ajc-tag-file "~/.java_base.tag")
(add-hook 'java-mode-hook 'ajc-java-complete-mode)


;--------------------------------------------------------------------------------
;; python
(require 'python-mode)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))

(require 'flymake-python-pyflakes)
(add-hook 'python-mode-hook 'flymake-python-pyflakes-load)

;--------------------------------------------------------------------------------
;; scala ensime
(require 'ensime)
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)
