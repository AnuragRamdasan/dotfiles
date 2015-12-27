(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aquamacs-additional-fontsets nil t)
 '(aquamacs-customization-version-id 307 t)
 '(aquamacs-tool-bar-user-customization nil t)
 '(cursor-type (quote (bar . 60)))
 '(set-cursor-color "#000000")
 '(default-frame-alist
    (quote
     ((menu-bar-lines . 1)
      (tool-bar-lines . 0)
      (fringe)
      (right-fringe)
      (left-fringe . 1)
      (internal-border-width . 0)
      (vertical-scroll-bars . right)
      (background-color . "#000000")
      (border-color . "#232323")
      (background-mode . dark)
      (mouse-color . "sienna1")
      (foreground-color . "#E6E1DC"))))
 '(global-hl-line-mode t)
 '(menu-bar-mode t)
 '(ns-tool-bar-display-mode (quote both) t)
 '(ns-tool-bar-size-mode (quote regular) t)
 '(visual-line-mode nil t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(mode-line ((t (:background "#FFFFFF" :foreground "grey75" :box (:line-width -1 :style released-button)))))
 '(mode-line-inactive ((t (:inherit mode-line :background "#171717" :foreground "grey75" :box (:line-width -1 :color "grey40") :weight light)))))
(load-file "~/Desktop/emacs.d/color-theme-myrailscasts.el")
(color-theme-myrailscasts)

;; -----------------------------------------------------------------------------
;; EMACS TWEAKS
;; -----------------------------------------------------------------------------

(delete-selection-mode t) ;; highlight a word and start typing, and it will delete the word and put your typed characters in it's place. highly annoying if not there.
(setq make-backup-files nil) ;; disable backup files

(require 'font-lock)
(setq font-lock-maximum-decoration t)
(global-font-lock-mode t)
(global-hi-lock-mode nil)
(setq jit-lock-contextually t)
(setq jit-lock-stealth-verbose t)

(tool-bar-mode -1)
(scroll-bar-mode -1)

(line-number-mode t) ;; show the current line and column numbers in the stats bar as well
(column-number-mode t)

(global-hl-line-mode t)

(global-auto-revert-mode t) ;; reload pages once changed on disk
(global-undo-tree-mode t)
(add-hook 'before-save-hook 'delete-trailing-whitespace) ;; deletes all whitespace that isn't needed.
;; -----------------------------------------------------------------------------
;; REMAPPED KEYS
;; -----------------------------------------------------------------------------
;;resize windows easily
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)

;;easy navigation through the buffers
(global-set-key (kbd "C-x C-<up>") 'windmove-up)
(global-set-key (kbd "C-x C-<down>") 'windmove-down)
(global-set-key (kbd "C-x C-<right>") 'windmove-right)
(global-set-key (kbd "C-x C-<left>") 'windmove-left)

(define-key global-map (kbd "RET") 'newline-and-indent)

(setq frame-title-format
      '((:eval (if (buffer-file-name)
		   (abbreviate-file-name buffer-file-name)
		 "%b"))))


(global-set-key (kbd "M-<up>") 'move-line-up)
(global-set-key (kbd "M-<down>") 'move-line-down)

(defun move-line-up ()
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

(defun move-line-down ()
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))
