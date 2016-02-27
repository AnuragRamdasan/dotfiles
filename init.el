;; -----------------------------------------------------------------------------
;; DISPLAY ERROR TRACE IF ANY
;;-----------------------------------------------------------------------------
(setq debug-on-error t)
(setq stack-trace-on-error t)
(load-library "url-handlers")
(global-unset-key (kbd "C-z"))

;; -----------------------------------------------------------------------------
;; ALL THE EMACS REPOSITORITES TO LIST PACKAGES
;; -----------------------------------------------------------------------------
(require 'package)
(add-to-list 'package-archives '("elpa" . "http://tromey.com/elpa/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))

(package-initialize)

;; -----------------------------------------------------------------------------
;; MAIN LOAD PATH
;; -----------------------------------------------------------------------------

(load-file "~/.emacs.d/custom.el")
(load-file "~/.emacs.d/utilities.el")
(load-file "~/.emacs.d/theme.el")
(load-file "~/.emacs.d/emacs.el")

(add-to-list 'load-path "~/.emacs.d/languages")
(load-file "~/.emacs.d/languages/ruby.el")
(load-file "~/.emacs.d/languages/web.el")
(load-file "~/.emacs.d/languages/c.el")
(load-file "~/.emacs.d/languages/js.el")
(load-file "~/.emacs.d/languages/python.el")
(load-file "~/.emacs.d/languages/dockerfile.el")
(load-file "~/.emacs.d/languages/erlang.el")
(load-file "~/.emacs.d/languages/elixir.el")
