;; load necessary packages if not installed
(when (not package-archive-contents)
  (package-refresh-contents))

(defvar utilities-packages '(
			     expand-region
			     ace-jump-mode
			     projectile helm helm-projectile grizzl
			     powerline
			     smooth-scroll
			     undo-tree
			     flycheck flymake
			     color-theme
			     auto-complete
			     magit
			     ))
(dolist (p utilities-packages)
  (package-install p))

;; -----------------------------------------------------------------------------
;; POWERLINE
;; -----------------------------------------------------------------------------
(require 'powerline)

;; -----------------------------------------------------------------------------
;; EXPAND REGION
;; -----------------------------------------------------------------------------
(require 'expand-region)
(global-set-key (kbd "C-@") 'er/expand-region)


;; -----------------------------------------------------------------------------
;; ACE JUMP MODE
;; -----------------------------------------------------------------------------
; needtodownload
(require 'ace-jump-mode)
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)

;; -----------------------------------------------------------------------------
;; IDO MODE
;; -----------------------------------------------------------------------------
(ido-mode 1)

;; -----------------------------------------------------------------------------
;; PROJECTILE
;; -----------------------------------------------------------------------------
(projectile-global-mode)
(setq projectile-completion-system 'grizzl)
(global-set-key (kbd "C-x C-h") 'helm-projectile)
(setq projectile-indexing-method 'native)
(setq projectile-enable-caching t)

;; -----------------------------------------------------------------------------
;; AUTOCOMPLETE
;; -----------------------------------------------------------------------------
(global-auto-complete-mode t)
