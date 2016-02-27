(when (not package-archive-contents)
  (package-refresh-contents))

(defvar ruby-packages '(
			projectile-rails
			robe
			enh-ruby-mode
			ruby-block
			bundler
			slim-mode))

(dolist (p ruby-packages)
  (package-install p))

;; --------------------------------------------------------------------
;; EMACS PACKAGES
;; --------------------------------------------------------------------
(require 'enh-ruby-mode)
(require 'ruby-block)
(require 'robe)

;; --------------------------------------------------------------------
;; EMACS CONFIGS
;; --------------------------------------------------------------------

(autoload 'enh-ruby-mode "enh-ruby-mode" "Major mode for ruby files" t)
(add-to-list 'auto-mode-alist '("\\.rb$" . enh-ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby" . enh-ruby-mode))
(setq ruby-block-delay 0)
(setq ruby-block-highlight-toggle t)
(ruby-block-mode t)

(add-hook 'enh-ruby-mode-hook 'robe-mode)
(add-hook 'enh-robe-mode-hook 'ac-robe-setup)

;;(add-to-list 'ac-modes 'enh-ruby-mode)
(add-to-list 'auto-mode-alist '("Jbuilder" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("rake" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.slim\\'" . slim-mode))
(add-hook 'enh-ruby-mode-hook '(lambda ()
				 (electric-indent-mode)))
(add-hook 'enh-ruby-mode-hook 'robe-mode)

;; --------------------------------------------------------------------
;; CUSTOM FUNCTIONS
;; --------------------------------------------------------------------

(defun ruby-interpolate ()
  "In double quoted string, convert # to #{}"
  (interactive)
  (insert "#")
  (when (and
	 (looking-back "\".*")
	 (looking-at ".*\""))
    (insert "{}")
    (backward-char 1)))

;; set C-h R to help
(define-key 'help-command "R" 'yari)
