(custom-set-variables
 '(cua-mode t nil (cua-base))
 '(menu-bar-mode t)
 '(tool-bar-mode nil)
 '(scroll-bar-mode nil))

;;; General 
;(desktop-save-mode t)
(cua-mode t)

;;; This was installed by package-install.el.
;;; This provides support for the package system and
;;; interfacing with ELPA, the package archive.
;;; Move this code earlier if you want to reference
;;; packages in your .emacs.
(when
    (load
     (expand-file-name "~/.emacs.d/elpa/package.el"))
  (package-initialize))


;;; Drag stuff
(require 'drag-stuff)
(drag-stuff-global-mode t)

;;; Color theme
(require 'color-theme)
(color-theme-initialize)
(color-theme-billw)

;;; Font
(set-default-font "-unknown-DejaVu Sans Mono-normal-normal-normal-*-16-*-*-*-m-0-iso10646-1")

(setq browse-url-generic-program (executable-find "chromium-browser")
      browse-url-browser-function 'browse-url-generic)
(put 'narrow-to-region 'disabled nil)

;;; Smart beginning of line
(defun smart-beginning-of-line ()
  "Move point to first non-whitespace character or beginning-of-line.

Move point to the first non-whitespace character on this line.
If point was already at that position, move point to beginning of line."
  (interactive)
  (let ((oldpos (point)))
    (back-to-indentation)
    (and (= oldpos (point))
         (beginning-of-line))))

(put 'smart-beginning-of-line 'CUA 'move)
(global-set-key [home] 'smart-beginning-of-line)

;;; Word completion
(global-set-key "\C-b" 'dabbrev-expand)

;;; Other window
(global-set-key (kbd "C--") 'other-window)

;;; IDo mode
(ido-mode t)

;;; Markdown mode 
(setq auto-mode-alist
      (cons '("\\.\\(markdown\\|md\\)" . markdown-mode) auto-mode-alist))

;;; Alternative yas expand
(global-set-key (kbd "M-b") 'yas/expand)

;;; Git grep 
;; There's something similar (but fancier) in vc-git.el: vc-git-grep
;; -I means don't search through binary files
(defcustom git-grep-switches "--extended-regexp -I -n --ignore-case"
  "Switches to pass to `git grep'."
  :type 'string)

(defun git-grep (command-args)
  (interactive
   (list (read-shell-command "Run git-grep (like this): "
                             (format "git grep %s -e "
                                     git-grep-switches)
                             'git-grep-history)))
  (let ((grep-use-null-device nil))
    (grep command-args)))

;;; Regexps
(global-set-key (kbd "M-s") 'isearch-forward-regexp)
(global-set-key (kbd "M-r") 'isearch-backward-regexp)

(defalias 'qrr 'query-replace-regexp)

;;; Macros
(global-set-key [f8] 'call-last-kbd-macro)

;;; Open line above and below
(defun open-line-above ()
  "Open a line above the line the point is at.
Then move to that line and indent accordning to mode"
  (interactive)
  (move-beginning-of-line 1)
  (newline)
  (previous-line)
  (indent-according-to-mode))

(defun open-line-below ()
  "Open a line below the line the point is at.
Then move to that line and indent accordning to mode"
  (interactive)
  (move-end-of-line 1)
  (newline)
  (indent-according-to-mode))

(global-set-key (kbd "M-o") 'open-line-above)
(global-set-key (kbd "C-o") 'open-line-below)