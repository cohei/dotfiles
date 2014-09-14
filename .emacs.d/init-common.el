;;; init-common --- emacs common config

;;; Commentary:
;;; aaaa

;;; Code:

; Emacsを起動したときのメッセージを省略
(setq inhibit-startup-message t)

; パッケージリポジトリの追加
(defvar package-list
  '(auctex auto-complete cacoo coffee-mode ensime exec-path-from-shell flycheck flycheck-tip
    ghc haml-mode haskell-mode helm helm-flycheck helm-projectile magit markdown-mode maxframe
    open-junk-file popwin projectile rainbow-delimiters rainbow-mode ruby-end ruby-hash-syntax
    ruby-interpolation solarized-theme scss-mode undo-tree yaml-mode yasnippet zencoding-mode)
  "A list of packages to ensure are installed at launch.")

(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
  (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
  (package-initialize)
  (dolist (p package-list)
    (when (and (not (package-installed-p p))
               (y-or-n-p (format "Package %s is missing. Install it? " p)))
      (package-install p))))
