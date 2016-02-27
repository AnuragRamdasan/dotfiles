(defvar elixir-packages '(elixir-mode elixir-mix flymake-elixir alchemist))
(dolist (p elixir-packages)
  (package-install p))

;; -----------------------------------------------------------------------------
;; EMACS PACKAGES
;; -----------------------------------------------------------------------------

;; --------------------------------------------------------------------
;; EMACS CONFIGS
;; --------------------------------------------------------------------
;; (alchemist-mode 1)
(add-to-list 'ac-modes 'elixir-mode)
(add-hook 'elixir-mode-hook '(lambda ()
			       (electric-indent-mode)))
(add-hook 'elixir-mode-hook '(lambda ()
			       (ruby-block-mode)))
