;; ---------------------------------------------------
;; Sample emacs config focusing on clojure development
;; ---------------------------------------------------

;; installed packages
;; - exec-path-from-shell (not from stable!)
;; - hl-sexp
;; - paredit
;; - clojure-mode
;; - cider
;; - company
;; - clj-refactor (not from stable!)

;; Add .emacs.d to load-path
(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))
(add-to-list 'load-path dotfiles-dir)

;; don't use tabs for indent
(setq-default indent-tabs-mode nil)

;; emacs package management
;; use MELPA stable
(require 'package)

(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/") t)

(setq package-user-dir (concat user-emacs-directory "elpa"))
(add-to-list 'load-path (concat user-emacs-directory "site-lisp"))

(defun require-package (package &optional min-version no-refresh)
  "Install given PACKAGE, optionally requiring MIN-VERSION.
If NO-REFRESH is non-nil, the available package lists will not be
re-downloaded in order to locate PACKAGE."
  (if (package-installed-p package min-version)
      t
    (if (or (assoc package package-archive-contents) no-refresh)
        (package-install package)
      (progn
        (package-refresh-contents)
        (require-package package min-version t)))))

(setq package-enable-at-startup nil) ; Don't initialize later as well

(package-initialize)

;; show opening, closing parens
(show-paren-mode)

(require-package 'epl)

(require-package 'exec-path-from-shell)
;; Sort out the $PATH for OSX
(require 'exec-path-from-shell)
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

(dolist (file '("cfg-paredit.el"
		"cfg-hlsexp.el"
		"cfg-cider.el"
                "cfg-cljrefactor.el"))
  (load (concat dotfiles-dir file)))


  
;; Custom User configurations:
;; If you wish to add additional functionality to your emacs config beyond what is in this setup,
;; simply add a file called "user-customizations.el" to your .emacs.d/ directory. Within that file,
;; you have access to the (require-package ...) function defined here, so for example, you could have:
;; (require-package 'rainbow-delimiters)
;; This would be all that is needed for emacs to automatically download the Rainbow Delimiters package
;; from Melpa. Additional configs of any kind could be added to this user-customizations.el file.
;; If the file is ommitted, no problem, no customizations are run.

(when (file-exists-p (concat dotfiles-dir "user-customizations.el"))
  (load (concat dotfiles-dir "user-customizations.el")))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(clojure-defun-indents (quote (defn defmacrol)))
 '(clojure-defun-style-default-indent t)
 '(custom-enabled-themes (quote (wombat)))
 '(global-linum-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(clojure-character-face ((t (:foreground "gray80" :family "Mono16"))))
 '(clojure-interop-method-face ((t (:foreground "yellow1" :weight extra-bold))))
 '(clojure-keyword-face ((t (:foreground "DarkOrange1" :slant italic))))
 '(font-lock-constant-face ((t (:foreground "#e5786d"))))
 '(font-lock-string-face ((t (:foreground "turquoise2" :slant italic))))
 '(rainbow-delimiters-depth-1-face ((t (:foreground "Black" :background "White"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "OrangeRed1"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "light green"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "magenta2"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "gold2"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "SkyBlue2"))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "wheat2"))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "spring green"))))
 '(rainbow-delimiters-depth-9-face ((t (:foreground "red1"))))
 '(rainbow-delimiters-mismatched-face ((t (:inherit default :background "gray100"))))
 '(rainbow-delimiters-unmatched-face ((t (:foreground "White")))))

