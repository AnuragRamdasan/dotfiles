;; hasnt been used in a long time
(defvar clojure-packages '(
			   clojure-mode clojure-cheatsheet cider
					rainbow-delimiters paredit))
(dolist (p clojure-packages)
  (package-install p))

;; --------------------------------------------------------------------
;; EMACS PACKAGES
;; --------------------------------------------------------------------
(require 'clojure-mode)
;(require 'cider)
;(require 'rainbow-delimiters)
;(require  'paredit)

;; --------------------------------------------------------------------
;; EMACS CONFIGS
;; --------------------------------------------------------------------
(add-hook 'clojure-mode-hook 'paredit-mode)
'(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
(setq nrepl-hide-special-buffers t)
(add-hook 'clojure-mode-hook 'turn-on-eldoc-mode)
(setq cider-repl-tab-command 'indent-for-tab-command)
(setq nrepl-buffer-name-separator "-")
(setq nrepl-buffer-name-show-port t)
(setq cider-repl-use-clojure-font-lock t)
(add-hook 'cider-repl-mode-hook 'subword-mode)
(add-hook 'cider-repl-mode-hook 'paredit-mode)
(add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode)
(add-to-list 'auto-mode-alist '("\\.cljs$" . clojure-mode))
