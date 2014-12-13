;;; init-home --- emacs home config

;;; Commentary:
;;; aaaa

;;; Code:


; 行ががはみ出したときに折り返す
(setq truncate-partial-width-windows nil)


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
;; coq-mode

(add-to-list 'auto-mode-alist (cons "\\.v$" 'coq-mode))
(autoload 'coq-mode "coq" "Major mode for editing Coq vernacular." t)
(autoload 'run-coq "coq-inferior" "Run an inferior Coq process." t)
(autoload 'run-coq-other-window "coq-inferior"
  "Run an inferior Coq process in a new window." t)
(autoload 'run-coq-other-frame "coq-inferior"
  "Run an inferior Coq process in a new frame." t)


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
