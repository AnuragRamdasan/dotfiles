;;(unless (file-exists-p "emacs-for-python")
;;  (magit-clone "https://github.com/gabrielelanaro/emacs-for-python" "."))

(add-to-list 'load-path "~/.emacs.d/languages/emacs-for-python/")

;; --------------------------------------------------------------------
;; EMACS PACKAGES
;; --------------------------------------------------------------------

(require 'epy-setup)      ;; It will setup other loads, it is required!
(require 'epy-python)     ;; If you want the python facilities [optional]
(require 'epy-completion) ;; If you want the autocompletion settings [optional]
(require 'epy-editing)    ;; For configurations related to editing [optional]

;; --------------------------------------------------------------------
;; EMACS CONFIGS
;; --------------------------------------------------------------------

(setq epy-enable-ropemacs nil)
(setq skeleton-pair nil)
(add-to-list 'ac-modes 'web-mode)

;; Highlight indentation coz python is painful otherwise
(require 'highlight-indentation)
(add-hook 'python-mode-hook 'highlight-indentation)
