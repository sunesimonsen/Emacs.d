;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

(custom-set-variables
 '(cua-mode t nil (cua-base))
 '(menu-bar-mode t)
 '(tool-bar-mode nil)
 '(scroll-bar-mode nil)
 '(truncate-lines t))

;;; General 
;(desktop-save-mode t)
(setq inhibit-startup-message t)

(cua-mode t)
(setq cua-enable-cua-keys nil)
(global-set-key (kbd "C-z") 'undo)

; stop leaving backup~ turds scattered everywhere
(setq backup-directory-alist '(("." . "~/.emacs-backups"))) 
; stop forcing me to spell out "yes"
(fset 'yes-or-no-p 'y-or-n-p)

;;; This was installed by package-install.el.
;;; This provides support for the package system and
;;; interfacing with ELPA, the package archive.
;;; Move this code earlier if you want to reference
;;; packages in your .emacs.
(when
    (load
     (expand-file-name "~/.emacs.d/elpa/package.el"))
  (package-initialize))


;;; Scrolling
(setq scroll-margin 4)
(setq scroll-step 1)


;;; Drag stuff
(require 'drag-stuff)
(drag-stuff-global-mode t)

;;; Color theme
(require 'color-theme)
(color-theme-initialize)
(color-theme-billw)

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

;;; Windows
(global-set-key (kbd "C--") 'other-window)
(global-set-key (kbd "M-0") 'delete-window)
(global-set-key (kbd "M-1") 'delete-other-windows)
(global-set-key (kbd "M-2") 'split-window-vertically)
(global-set-key (kbd "M-3") 'split-window-horizontally)

(global-set-key (kbd "M-å") 'enlarge-window-horizontally)
(global-set-key (kbd "M-ø") 'shrink-window-horizontally)
(global-set-key (kbd "C-å") 'enlarge-window)
(global-set-key (kbd "C-ø") 'shrink-window)

;; Buffers
(global-set-key [XF86Back] 'previous-buffer)
(global-set-key [XF86Forward] 'next-buffer)

;;; IDo mode
(ido-mode t)
(setq ido-enable-flex-matching t)
(setq ido-enable-last-directory-history nil)

;;; Markdown mode 
(setq auto-mode-alist
      (cons '("\\.\\(markdown\\|md\\)" . markdown-mode) auto-mode-alist))

;;; YASnippet
(setq yas/root-directory "~/.emacs.d/snippets")
(yas/load-directory yas/root-directory)
(yas/global-mode t)
;; Alternative yas expand
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

;;; Join 
(global-set-key (kbd "C-x C-j") 'join-line)

;;; ETags
(defun ido-find-file-in-tag-files ()
  (interactive)
  (save-excursion
    (let ((enable-recursive-minibuffers t)) (visit-tags-table-buffer))
    (find-file (expand-file-name
		(ido-completing-read "Project file: "
				     (tags-table-files) nil t)))))

(defvar af-ido-flex-fuzzy-limit (* 2000 5))
(defadvice ido-set-matches-1 (around my-ido-set-matches-1 activate)
  "Conditionally disable flex matching if the list is huge.

This is useful because when the ido list is huge, ido flex matching
spends an eternity in a regex if you make a typo."
  (let ((ido-enable-flex-matching (< (* (length (ad-get-arg 0)) (length ido-text))
                                     af-ido-flex-fuzzy-limit)))
    ad-do-it))

(global-set-key (kbd "C-x p") 'ido-find-file-in-tag-files)

;;;; Highlight-Symbol
(require 'highlight-symbol)
(global-set-key (kbd "<f12>") 'highlight-symbol-at-point)
(global-set-key (kbd "C-.") 'highlight-symbol-next)
(global-set-key (kbd "C-,") 'highlight-symbol-prev)

;;; Registers
;TODO make an append with separator where the 
;default separator can be changed or specified 
  
(global-set-key (kbd "C-x r a") 'append-to-register)

(defun append-to-register-with-newline 
  (register start end &optional delete-flag)
  "Append region to text register REGISTER with a new line as separator.
With prefix arg, delete as well.
Called from program, takes four args: REGISTER, START, END and DELETE-FLAG.
START and END are buffer positions indicating what to append."
  (interactive "cAppend to register: \nr\nP")
  (let ((separator "\n")
	(value (get-register register)))
    (or 
     (not value)
     (stringp value)
     (error "Register does not contain text"))
    (set-register register 
		  (concat (if value (concat value separator) "")
			  (buffer-substring start end)))
    (if delete-flag (delete-region start end))))

(defun clear-register (register) 
  "Sets the specified register to nil."
  (interactive "cRegiszer to clear: ")
  (set-register register nil))

(global-set-key (kbd "M-æ M-a") 'append-to-register)
(global-set-key (kbd "M-æ a") 'append-to-register-with-newline)
(global-set-key (kbd "M-æ i") 'insert-register)
(global-set-key (kbd "M-æ s") 'copy-to-register)
(global-set-key (kbd "M-æ v") 'view-register)
(global-set-key (kbd "M-æ SPC") 'point-to-register)
(global-set-key (kbd "M-æ C-SPC") 'point-to-register)
(global-set-key (kbd "M-æ j") 'jump-to-register)
(global-set-key (kbd "M-æ c") 'clear-register)



;;; Bookmarks 
(global-set-key (kbd "C-æ l") 'bookmark-bmenu-list)
(global-set-key (kbd "C-æ j") 'bookmark-jump)
(global-set-key (kbd "C-æ b") 'bookmark-set)

;;; Replace 
(global-set-key (kbd "C-f") 'query-replace)
(global-set-key (kbd "M-f") 'query-replace-regexp)


;;; Extend selection 
;; by Nikolaj Schumacher, 2008-10-20. Released under GPL.
(defun semnav-up (arg)
  (interactive "p")
  (when (nth 3 (syntax-ppss))
    (if (> arg 0)
        (progn
          (skip-syntax-forward "^\"")
          (goto-char (1+ (point)))
          (decf arg))
      (skip-syntax-backward "^\"")
      (goto-char (1- (point)))
      (incf arg)))
  (up-list arg))

;; by Nikolaj Schumacher, 2008-10-20. Released under GPL.
(defun extend-selection (arg &optional incremental)
  "Select the current word.
Subsequent calls expands the selection to larger semantic unit."
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     (or (and transient-mark-mode mark-active)
                         (eq last-command this-command))))
  (if incremental
      (progn
        (semnav-up (- arg))
        (forward-sexp)
        (mark-sexp -1))
    (if (> arg 1)
        (extend-selection (1- arg) t)
      (if (looking-at "\\=\\(\\s_\\|\\sw\\)*\\_>")
          (goto-char (match-end 0))
        (unless (memq (char-before) '(?\) ?\"))
          (forward-sexp)))
      (mark-sexp -1))))

(global-set-key (kbd "M-8") 'extend-selection)


(global-set-key [(meta menu)] 'imenu)