(defvar docker-packages '(dockerfile-mode))
(dolist (p docker-packages)
  (package-install p))

;; -----------------------------------------------------------------------------
;; EMACS PACKAGES
;; -----------------------------------------------------------------------------
(require 'dockerfile-mode)

;; --------------------------------------------------------------------
;; EMACS CONFIGS
;; --------------------------------------------------------------------
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))
