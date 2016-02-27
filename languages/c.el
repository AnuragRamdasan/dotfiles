;; ;; HASNT BEEN TESTED IN A WHILE

;; ;; -----------------------------------------------------------------------------
;; ;; C PROGRAMMING
;; ;; -----------------------------------------------------------------------------

;; (require 'ctags)
;; (require 'ctags-update)
;; (require 'c-eldoc)
;; ;(require 'disaster)

;; Set C default style
;;(setq c-default-style "linux" c-basic-offset 4)
;; (add-hook 'c-mode 'turn-on-eldoc-mode)
;; (define-key c-mode-base-map (kbd "C-c a") 'disaster)

;; (setq path-to-ctags "/opt/local/bin/ctags") ;; <- ctags path
;; (defun create-tags (dir-name)
;;   "Create tags file."
;;   (interactive "DDirectory: ")
;;   (shell-command
;;    (format "etags -f %s -R %s/*" path-to-ctags (directory-file-name dir-name))))


;; (defun my-move-function-up ()
;;   "Move current function up."
;;   (interactive)
;;   (save-excursion
;;     (c-mark-function)
;;     (let ((fun-beg (point))
;; 	  (fun-end (mark)))
;;       (transpose-regions (progn
;; 			   (c-beginning-of-defun 1)
;; 			   (point))
;; 			 (progn
;; 			   (c-end-of-defun 1)
;; 			   (point))
;; 			 fun-beg fun-end))))

;; (defun my-move-function-down ()
;;   "Move current function down."
;;   (interactive)
;;   (save-excursion
;;     (c-mark-function)
;;     (let ((fun-beg (point))
;; 	  (fun-end (mark)))
;;       (transpose-regions fun-beg fun-end
;; 			 (progn
;; 			   (c-beginning-of-defun -1)
;; 			   (point))
;; 			 (progn
;; 			   (c-end-of-defun 1)
;; 			   (point))))))

;; (add-hook 'c-mode-hook '(lambda ()
;; 			  (local-set-key (kbd "C-c C-f C-u") 'my-move-function-up)
;; 			  (local-set-key (kbd "C-c C-f C-d") 'my-move-function-down)
;; 			  ))

;; ;; (add-hook 'c-mode-hook
;; ;; 	  '(lambda ()
;; ;; 	     (add-to-list
;; ;; 	      'ac-omni-completion-sources
;; ;; 	      (cons "\\." '(ac-source-semantic)))
;; ;; 	     (add-to-list
;; ;; 	      'ac-omni-completion-sources
;; ;; 	      (cons "->" '(ac-source-semantic)))
;; ;; 	     (setq ac-sources
;; ;; 		   '(ac-source-semantic
;; ;; 		     ac-source-yasnippet))))
