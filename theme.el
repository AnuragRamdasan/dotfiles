(require 'color-theme)
(color-theme-initialize)
(setq color-theme-is-global t)
(load-file "~/.emacs.d/color-theme-myrailscasts.el")
(color-theme-myrailscasts)
(blink-cursor-mode -1)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(mode-line ((t
	       (:background "#171717" :foreground "grey75" :box
			    (:line-width -1 :style released-button)))))
 '(mode-line-inactive ((t
			(:inherit mode-line :background "#171717" :foreground
				  "grey75" :box
				  (:line-width -1 :color "grey40") :weight light)))))
