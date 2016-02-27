(when (not package-archive-contents)
  (package-refresh-contents))

(defvar js-packages '(
		      js2-mode
		      ))

(dolist (p js-packages)
  (package-install p))

;; --------------------------------------------------------------------
;; EMACS PACKAGES
;; --------------------------------------------------------------------
;;(add-to-list 'auto-mode-alist '(".js" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist `(,(rx ".js" string-end) . js2-mode))
;; --------------------------------------------------------------------
;; EMACS CONFIGS
;; --------------------------------------------------------------------

;; --------------------------------------------------------------------
;; CUSTOM FUNCTIONS
;; --------------------------------------------------------------------
