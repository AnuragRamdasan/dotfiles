;; CEDET CUSTOM CONFIGURATION

;; --------------------------------------------------------------------
;; KEYBINDINGS ENABLED BY CEDET
;; --------------------------------------------------------------------

;; show list of symbols beginning at point      C-c ?
;; show symbol names beginning at point         C-c >
;; visit header file                            C-c i
;; jump to original decalaration of tag         C-c j
;; show documentation for the symbol at point   C-c d
;; show summary for symbol at point             C-c s
;; insert context aware snippets                C-c r
;; toggle between prototype and implementation  C-c t
;; access member values of a struct             .
;; switch between header and source             C-c f
;; list all functions in the current file       C-c e
;; find all reference of symbol at point        C-c C-r


(load-file "~/.emacs.d/cedet/cedet-devel-load.el")
;; if this gives error, run make from within contrib
(load-file "~/.emacs.d/cedet/contrib/cedet-contrib-load.el")

(add-to-list  'Info-directory-list "~/.emacs.d/cedet/doc/info")

(add-to-list 'semantic-default-submodes 'global-semanticdb-minor-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-mru-bookmark-mode)
(add-to-list 'semantic-default-submodes 'global-semanticdb-minor-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-idle-scheduler-mode)
(add-to-list 'semantic-default-submodes 'global-cedet-m3-minor-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-highlight-func-mode)
;; I still havent made my mind about this
;;(add-to-list 'semantic-default-submodes 'global-semantic-highlight-edits-mode)

;; Activate semantic
(semantic-mode 1)

(require 'semantic/ia)
(require 'semantic/bovine/gcc)

(defun my-semantic-hook ()
  (imenu-add-to-menubar "TAGS"))
(add-hook 'semantic-init-hooks 'my-semantic-hook)

(defun my-cedet-hook ()
  (local-set-key [(control return)] 'semantic-ia-complete-symbol-menu)
  ;; show list of symbols beginning at point
  (local-set-key "\C-c?" 'semantic-ia-complete-symbol)
  ;; show symbol names beginning at point
  (local-set-key "\C-c>" 'semantic-complete-analyze-inline)
  ;; visit header file
  (local-set-key "\C-ci" 'semantic-decoration-include-visit)
  ;; jump to original declaration of tag
  (local-set-key "\C-cj" 'semantic-ia-fast-jump)
  ;; show document for the tag
  (local-set-key "\C-cd" 'semantic-ia-show-doc)
  ;; show tag summary in modeline
  (local-set-key "\C-cs" 'semantic-ia-show-summary)
  (local-set-key "\C-ct" 'semantic-analyze-proto-impl-toggle))

(require 'auto-complete)
(add-to-list 'ac-sources 'ac-source-semantic-raw)
(add-hook 'c-mode-common-hook 'my-cedet-hook)
(add-hook 'lisp-mode-hook 'my-cedet-hook)
(add-hook 'scheme-mode-hook 'my-cedet-hook)
(add-hook 'emacs-lisp-mode-hook 'my-cedet-hook)
(add-hook 'enh-ruby-mode 'my-cedet-hook)

;; autocomplete struct members in C
(defun my-c-mode-cedet-hook ()
  (local-set-key "." 'semantic-complete-self-insert)
  (local-set-key ">" 'semantic-complete-self-insert))
(add-hook 'c-mode-common-hook 'my-c-mode-cedet-hook)

;; SRecode
;; insert context aware snippets
(global-srecode-minor-mode 1)
(global-set-key (kbd "C-c r") 'srecode-insert)

(require 'eassist)
(defun my-c-mode-cedet-hook ()
  ;; (local-set-key "." 'semantic-complete-self-insert)
  ;; (local-set-key ">" 'semantic-complete-self-insert)
  (local-set-key "\C-cf" 'eassist-switch-h-cpp)
  (local-set-key "\C-ce" 'eassist-list-methods)
  (local-set-key "\C-c\C-r" 'semantic-symref)
  (add-to-list 'ac-sources 'ac-source-gtags)
  )
(add-hook 'c-mode-common-hook 'my-c-mode-cedet-hook)

(when (cedet-gnu-global-version-check t)
  (semanticdb-enable-gnu-global-databases 'c-mode t)
  (semanticdb-enable-gnu-global-databases 'c++-mode t))

(when (cedet-ectag-version-check t)
  (semantic-load-enable-primary-ectags-support))

(global-ede-mode t)
