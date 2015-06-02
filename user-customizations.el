;This is the custom emacs config for user hellofunk, which adds features not included in the example-config from clojure-emacs.
;info: https://github.com/hellofunk/example-config

;anything you see below that uses require-package is downloaded automatically from Melpa and requires no manual installation of any .el files

;----------------------------------------------------------------------------

;turn off emacs alarms (those annoying beeps)
(setq ring-bell-function 'ignore)

;expand-region functionality is really great for lisp/clojure editing
;from https://github.com/magnars/expand-region.el
(require-package 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;sets the auto-save directory to be other than project directories, so you aren't inundated with tons of backup files
(setq backup-directory-alist `(("." . "~/.saves")))

;winner mode
;from: http://www.emacswiki.org/emacs/WinnerMode
(when (fboundp 'winner-mode)
      (winner-mode 1))

;adds color matching for open and closed parenthesis
;from https://github.com/Fanael/rainbow-delimiters
(require-package 'rainbow-delimiters)
;turns on this for default with clojure files
(add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
(add-hook 'cider-repl-mode-hook #'rainbow-delimiters-mode)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;be kind to your eyes, solarize! hey, that rhymes!
;from https://github.com/bbatsov/solarized-emacs
; (require-package 'solarized-theme)
; (require 'solarized-light-theme)
; (provide 'solarized-theme)

;use ido-mode, a must-have for quick emacs navigation
;from http://emacswiki.org/emacs/InteractivelyDoThings
;more about it: http://www.masteringemacs.org/article/introduction-to-ido-mode
(require 'ido)
(ido-mode t)
(ido-everywhere 1)
(require-package 'flx-ido)
(flx-ido-mode 1)

;smex brings the power of ido mode to your M-x mini-buffer, another way to supercharge your emacs navigation
;from https://github.com/nonsequitur/smex
(require-package 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

; because M-S-6 is awkward
(global-set-key (kbd "C-c q") 'join-line)

;use I-Buffer
(global-set-key (kbd "C-x C-b") 'ibuffer)

;shortcut to launch a shell
(global-set-key (kbd "C-x C-m") 'shell)

;useful little tweak. press C-space once to start a mark, then C-space to end a mark. If you do this in succession, it will simply mark a particular point in a file. Then use C-u and repeat C-space to move back through your saved points.
(setq set-mark-command-repeat-pop t)

;turn off the emacs toolbar; we're coders, after all. when do we use a mouse?
(tool-bar-mode -1)

;turn off the scroll bars
(scroll-bar-mode -1)

;turn off the cursor blinking -- personally i find it distracting
(blink-cursor-mode 0)

;turn off text wrapping in the middle of a word
(global-visual-line-mode 1)

;set font size on startup
;my eyes are kinda bad so i prefer a larger font size
; (set-face-attribute 'default nil :height 135)

;; Font size change
; (define-key global-map (kbd "C-c C-=") 'text-scale-increase)
; (define-key global-map (kbd "C-c C--") 'text-scale-decrease)
(define-key global-map (kbd "C-+") 'text-scale-increase)
(define-key global-map (kbd "C--") 'text-scale-decrease)

;use shift + arrow keys to jump between windows
; (windmove-default-keybindings)

;;these are simple keybindings i've always like to quickly make an emacs window taller, shorter, narrower or wider, without using the mouse
; (defalias 'hellofunk/windowtaller  (read-kbd-macro "C-u 2 C-x ^"))
; (global-set-key (kbd "C-q") 'hellofunk/windowtaller)
; (defalias 'hellofunk/windowshorter (read-kbd-macro "C-u - 2 C-x ^"))
; (global-set-key (kbd "C-z") 'hellofunk/windowshorter)
; (defalias 'hellofunk/windowwider (read-kbd-macro "C-u - 2 C-x }"))
; (global-set-key (kbd "C-\\") 'hellofunk/windowwider)
; (defalias 'hellofunk/windownarrower (read-kbd-macro "C-u - 2 C-x {"))
; (global-set-key (kbd "C-]") 'hellofunk/windownarrower)

;;ORG MODE
;;org mode is a great emacs feature for organization notes, outlines, and more. plenty of docs online about it
; (require 'org)
; (define-key global-map "\C-cl" 'org-store-link)
; (define-key global-map "\C-ca" 'org-agenda)
; (setq org-log-done t)


;erc configurations, for using irc. if using this, change the username from to whatever you use
; (setq erc-nick "your-irc-username")
;http://www.emacswiki.org/emacs/ErcLogging
;choose any directory on your computer for placing log files
;be mindful that some channels to do not promote irc logging, therefore don't post logs publicly without some knowledge about the manners of a channel
; (setq erc-log-channels-directory "~/your-irc-saves/")
;
(setq erc-save-buffer-on-part t
      erc-save-queries-on-quit t
      erc-log-write-after-send t
      erc-log-mode t
      erc-autojoin-mode t
      erc-log-write-after-insert t
      erc-log-file-coding-system 'utf-8)
; (setq erc-log-insert-log-on-open nil)
;choose your desired channels to automatically join here; anything can be added after the "#freenode.net" element
; (setq erc-autojoin-channels-alist '(("freenode.net"  "#emacs")))
;hides annoying irc messages unrelated to actual chats
; (setq erc-hide-list (quote ("JOIN" "PART" "QUIT" "NICK" "AWAY")))
; (setq erc-modules (quote (autojoin button completion fill irccontrols list log match menu move-to-prompt netsplit networks noncommands readonly ring stamp track)))
(defun erc-columns ()
  (erc-fill-disable))
; (add-hook 'erc-mode-hook 'erc-columns )

;adds a custom directory where some .el scripts are placed used in this config file. Note that if you have .el scripts that exist on Melpa, you don't need this directory. But I have an undo-tree script (configured below) not on Melpa, so this is how you do it:
(add-to-list 'load-path (concat user-emacs-directory "hellofunk-config"))

;undo-tree
;from http://www.emacswiki.org/emacs/UndoTree
;script must exist in the hellofunk-config directory before proceeding (see above paragraph)
(require 'undo-tree)
(global-undo-tree-mode)

;see: http://www.emacswiki.org/emacs/EmacsForMacOS#toc26
(when (eq system-type 'darwin) ;; mac specific settings
  ;(setq mac-option-modifier 'alt)
  (setq mac-command-modifier 'meta)
  (global-set-key [kp-delete] 'delete-char) ;; sets fn-delete to be right-delete
)

;; Complementary configuration
; (add-hook 'cider-mode-hook #'eldoc-mode)
(setq nrepl-log-messages t)
(setq nrepl-hide-special-buffers t)
(setq cider-auto-select-error-buffer nil)
(setq cider-repl-pop-to-buffer-on-connect t)
(setq nrepl-buffer-name-separator "-")
(setq nrepl-buffer-name-show-port t)
(setq cider-repl-display-in-current-window t)
(setq cider-prompt-save-file-on-load nil)
(setq cider-repl-result-prefix ";; => ")
(setq cider-interactive-eval-result-prefix ";; => ")
(setq cider-test-show-report-on-success t)
; (setq cider-repl-wrap-history t)
; (setq cider-repl-history-size 1000)
; (setq cider-repl-history-file "~/.replhistory")
; (add-hook 'cider-repl-mode-hook #'subword-mode)

(require-package 'smartparens)

;; paredit chacracter change
(eval-after-load 'paredit
  '(progn
    (define-key paredit-mode-map (kbd "M-<right>")  #'paredit-forward-slurp-sexp)
    (define-key paredit-mode-map (kbd "C-<left>") #'paredit-backward-slurp-sexp)
    (define-key paredit-mode-map (kbd "C-<right>")  #'paredit-backward-barf-sexp)
    (define-key paredit-mode-map (kbd "M-<left>") #'paredit-forward-barf-sexp)
	
    (define-key paredit-mode-map (kbd "(")   #'paredit-open-round)
    (define-key paredit-mode-map (kbd "{")   #'paredit-open-curly)
    (define-key paredit-mode-map (kbd "[")   #'paredit-open-square)
    (define-key paredit-mode-map (kbd "}")   #'paredit-close-curly)
    ;; (define-key paredit-mode-map (kbd "M-(") nil)
    (define-key paredit-mode-map (kbd "M-(") #'paredit-wrap-round)
    (define-key paredit-mode-map (kbd "M-8") #'paredit-wrap-curly)
    (define-key paredit-mode-map (kbd "M-5") #'paredit-wrap-square)

    (define-key paredit-mode-map (kbd "M-<delete>")   #'paredit-kill)
    (define-key paredit-mode-map (kbd "C-<delete>")   #'paredit-forward-kill-word)
    ;; (define-key paredit-mode-map (kbd "M-DEL") nil)   ; #'paredit-kill)
    (define-key paredit-mode-map (kbd "C-DEL")        #'paredit-backward-kill-word)))

(require-package 'linum)

(eval-after-load 'linum
  '(progn
     (defface linum-leading-zero
       `((t :inherit 'linum
            :foreground ,(face-attribute 'linum :background nil t)))
       "Face for displaying leading zeroes for line numbers in display margin."
       :group 'linum)

     (defun linum-format-func (line)
       (let ((w (length (number-to-string (count-lines (point-min) (point-max))))))
	 (propertize (format (format "%%%dd" w) line) 'face 'linum)))
     (setq linum-format 'linum-format-func)))


;; projectile setup
(require-package 'projectile)
(projectile-global-mode)

;; (add-hook 'clojure-mode-hook 'projectile-mode) ;; projectile for Clojure
(setq projectile-indexing-method 'alien) ;; don't use EMACS indexing
(setq projectile-enable-caching t) ;; cache project files
(setq projectile-switch-project-action 'projectile-find-dir) ;; keep current project directory
(setq projectile-find-dir-includes-top-level t) ;; enable up directory to top level
;; (setq projectile-keymap-prefix (kbd "C-c C-p")) ;; command is C-c C-p
