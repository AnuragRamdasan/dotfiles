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

(defvar my-linum-format-string "%3d")

(add-hook 'linum-before-numbering-hook 'my-linum-get-format-string)

(defun my-linum-get-format-string ()
					; The + 2 defines the leeway we have in the sidebar
  (let* ((width (+ 3 (length (number-to-string
			      (count-lines
			       (point-min)
			       (point-max))))))
	 (format (concat "%" (number-to-string width) "d")))
    (setq my-linum-format-string format)))

(defvar my-linum-current-line-number 0)

(setq linum-format 'my-linum-relative-line-numbers)

					;(propertize (format my-linum-format-string noffset) 'face 'linum)
(defun my-linum-relative-line-numbers (line-number)
  (let ((offset (- line-number my-linum-current-line-number)))
    (if (= offset 0)
	(propertize (format my-linum-format-string
			    line-number) 'face 'linum)
      (propertize (format my-linum-format-string (abs
						  offset))
		  'face 'linum))))

(defadvice linum-update (around my-linum-update)
  (let ((my-linum-current-line-number (line-number-at-pos)))
    ad-do-it))
(ad-activate 'linum-update)

(provide 'relative-number)
