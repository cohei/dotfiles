;;; init-home --- emacs home config

;;; Commentary:
;;; aaaa

;;; Code:


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
;; yasnippet

(require 'yasnippet)
(setq yas-snippet-dirs
  '("~/.emacs.d/snippets" ;; 作成するスニペットはここに入る
    "~/.emacs.d/site-lisp/yasnippet/snippets" ;; 最初から入っていたスニペット(省略可能)
    ))
(yas-global-mode 1)
(custom-set-variables '(yas-trigger-key "TAB"))
