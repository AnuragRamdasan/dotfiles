;; Highlight a word and start typing, and it will delete the word
;; and put your typed characters in it's place. highly annoying if not there.
(delete-selection-mode t)

;; Disable backup files
(setq make-backup-files nil) 

;; Text decoration
(require 'font-lock)
(setq font-lock-maximum-decoration t)
(global-font-lock-mode t)
(global-hi-lock-mode nil)
(setq jit-lock-contextually t)
(setq jit-lock-stealth-verbose t)

;; If there is size information associated with text,
;; change the text size to reflect it
(size-indication-mode t)

;; Highlight parentheses when the cursor is next to them
(require 'paren)
(show-paren-mode t)

;; Disable the welcome message
(setq inhibit-startup-message t)

;; Format the title-bar to always include the buffer name
(setq frame-title-format "emacs - %b") 

;; Always end a file with a newline
(setq require-final-newline t)

;; Stop emacs from arbitrarily adding lines to
;; the end of a file when the cursor is moved past the end of it:
(setq next-line-add-newlines nil)

;; No scroll bars
(toggle-scroll-bar -1)

;; No bells
(setq visible-bell nil)

;; Remove icons toolbar
(if (> emacs-major-version 20)
    (tool-bar-mode -1))

;; No menu bars
(menu-bar-mode -1)

 ;; Use y or n instead of yes or not
(fset 'yes-or-no-p 'y-or-n-p)

;; Show the current line and column numbers in the stats bar as well
(line-number-mode t) 
(column-number-mode t)

;; Reload pages once changed on disk
(global-auto-revert-mode t)

;; Undo like a boss
(global-undo-tree-mode t)

;; Deletes all whitespace that isn't needed.
(add-hook 'before-save-hook 'delete-trailing-whitespace) 

(global-hl-line-mode t)
(blink-cursor-mode -1)
(transient-mark-mode t)

(setq max-specpdl-size 1000)
(setq max-lisp-eval-depth 1000)
(setq-default fill-column 80)
(add-hook 'prog-mode-hook (lambda()(auto-fill-mode)))

(global-visual-line-mode t)
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'text-mode-hook
	  '(lambda() (set-fill-column 120)))
