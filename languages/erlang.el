(defvar erlang-packages '(erlang edts))
(dolist (p erlang-packages)
  (package-install p))

;; -----------------------------------------------------------------------------
;; EMACS PACKAGES
;; -----------------------------------------------------------------------------

;; --------------------------------------------------------------------
;; EMACS CONFIGS
;; --------------------------------------------------------------------
(add-to-list 'auto-mode-alist '("\\.erl\\'" . erlang-mode))
(add-to-list 'ac-modes 'erlang-mode)
