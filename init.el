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

;;; helper functions
(defun find-by-string-key (key seq)
  (assoc-if (lambda (x) (string-equal x key)) seq))

(defun cdr* (seq)
  (map 'list 'cdr seq))

(defun car* (seq)
  (map 'list 'car seq))

;;; General 
;(desktop-save-mode t)
(setq inhibit-startup-message t)

;(cua-mode t)
;(setq cua-enable-cua-keys nil)
(global-set-key (kbd "C-z") 'undo)

; stop leaving backup~ turds scattered everywhere
(setq backup-directory-alist '(("." . "~/.emacs-backups"))) 
; stop forcing me to spell out "yes"
(fset 'yes-or-no-p 'y-or-n-p)

;; I hate tabs!
(setq-default indent-tabs-mode nil)
(setq c-basic-indent 2)
(setq tab-width 4)

;;; This was installed by package-install.el.
;;; This provides support for the package system and
;;; interfacing with ELPA, the package archive.
;;; Move this code earlier if you want to reference
;;; packages in your .emacs.
(when
    (load
     (expand-file-name "~/.emacs.d/elpa/package.el"))
  (package-initialize))

(add-to-list 'load-path (expand-file-name "~/.emacs.d/scala-mode/"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/moz/"))

(autoload 'moz-minor-mode "moz" "Mozilla Minor and Inferior Mozilla Modes" t)
(autoload 'inferior-moz-mode "moz" "MozRepl Inferior Mode" t)

(load (expand-file-name "~/.emacs.d/js2-mode-indentation.el"))

;;; Drag stuff
;(require 'drag-stuff)
;(drag-stuff-global-mode t)

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

;;; Buffers
(global-set-key [XF86Back] 'previous-buffer)
(global-set-key [XF86Forward] 'next-buffer)

;;; IDo mode
(ido-mode t)
(setq ido-enable-flex-matching t)
(setq ido-enable-last-directory-history nil)

(defun lazy-load-smex ()
  (interactive)
  (or (boundp 'smex-cache)
      (smex-initialize))
  (global-set-key [(meta x)] 'smex)
  (global-set-key [menu] 'smex)
  (smex))

;;; SMEX
(global-set-key [(meta x)] 'lazy-load-smex)
(global-set-key [menu] 'lazy-load-smex)

(global-set-key 
 [(shift meta x)] 
 (lambda ()
   (interactive)
   (or (boundp 'smex-cache)
       (smex-initialize))
   (global-set-key [(shift meta x)] 'smex-major-mode-commands)
   (smex-major-mode-commands)))

(defun smex-update-after-load (unused)
  (when (boundp 'smex-cache)
    (smex-update)))
(add-hook 'after-load-functions 'smex-update-after-load)

;;; Markdown mode 
(setq auto-mode-alist
      (cons '("\\.\\(markdown\\|md\\)" . markdown-mode) auto-mode-alist))

;;; YASnippet
;(setq yas/root-directory "~/.emacs.d/snippets")
;(yas/load-directory yas/root-directory)
(add-to-list 'load-path "~/.emacs.d/yasnippet")
(require 'yasnippet) ;; not yasnippet-bundle
(yas/global-mode t)

;; Alternative yas expand
(global-set-key (kbd "M-b") 'yas/expand)

;;; Git grep 
;; There's something similar (but fancier) in vc-git.el: vc-git-grep
;; -It means don't search through binary files
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
  (let ((ido-enable-flex-matching 
         (< (* (length (ad-get-arg 0)) (length ido-text))
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
  (interactive "cRegister to clear: ")
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

;;; Delete 
;(global-set-key (kbd "C-d") 'kill-whole-line)
;(global-set-key [delete] 'delete-char)

;;; Command on region
(global-set-key (kbd "C-' C-'") 'shell-command-on-region)
 
;;; Navigatio
(global-set-key (kbd "C-<") 'end-of-buffer)
(global-set-key (kbd "M-<") 'beginning-of-buffer)

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
;;todo rebind
(global-set-key (kbd "M-8") 'extend-selection)

(defun prefix-suffix-region (prefix suffix start end) 
  "Add a prefix and a suffix string to each line between mark and point." 
  (interactive "sPrefix string: \nsSuffix string: \nr") 
  (replace-regexp 
   (rx (submatch line-start (* blank)) (submatch (* not-newline) (not blank)) (submatch (* blank) line-end)) 
   (concat "\\1" prefix "\\2" suffix "\\3") nil start end))
  
(defun prefix-suffix-region-preserving-whitespace (prefix suffix start end) 
  "Add a prefix and a suffix string to each line between mark and point." 
  (interactive "sPrefix string: \nsSuffix string: \nr") 
  (if prefix 
      (let ((p (point))
	    (count (count-lines start end))) 
	(goto-char start) 
	(while (> count 0) 
	  (setq count (1- count)) 
	  (beginning-of-line 1) 
	  (insert prefix) 
	  (end-of-line 1) 
	  (insert suffix) 
	  (forward-char 1))
	(goto-char p))))

(defun insert-numbers (offset start end)
  "Inserts increasing number for each line in the region starting from OFFSET"
  (interactive "nOffset: \nr")
  (save-excursion
    (widen)
    (goto-char start)
    (let ((i offset))
      (while (< (point) end)
        (insert (format "%d " i))
        (forward-line)
        (setq i (1+ i))))))

(load (expand-file-name "~/.emacs.d/powerbuilder-mode.el"))
(autoload 'powerbuilder-mode "powerbuilder-mode" "Power Builder mode." t)
(setq auto-mode-alist (append '(("\\.\\(srw\\|sru\\|srm\\)$" . powerbuilder-mode)) auto-mode-alist))

;;; Rectangles
(global-set-key (kbd "C-' i") 'string-insert-rectangle)
(global-set-key (kbd "C-' r") 'string-rectangle)
(global-set-key (kbd "C-' o") 'open-rectangle)
(global-set-key (kbd "C-' c") 'clear-rectangle)
(global-set-key (kbd "C-' k") 'kill-rectangle)
(global-set-key (kbd "C-' y") 'yank-rectangle)
(global-set-key (kbd "C-' d") 'delete-rectangle)

;;; imenu
(global-set-key [(meta menu)] 'imenu)

;;; Marks
(defun jump-to-mark ()
  (interactive) 
  (set-mark-command 0))
(global-set-key [(meta left)] 'jump-to-mark)

(load (expand-file-name "~/.emacs.d/visible-mark.el"))

;;; X Maximized
(defun maximize-window (&optional f)
       (interactive)
       (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
	    		 '(2 "_NET_WM_STATE_MAXIMIZED_VERT" 0))
       (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
	    		 '(2 "_NET_WM_STATE_MAXIMIZED_HORZ" 0)))

;;; DOS 
(load (expand-file-name "~/.emacs.d/dos.el"))
(autoload 'dos-mode "dos" "Edit Dos scripts." t)
(add-to-list 'auto-mode-alist '("\\.bat$" . dos-mode))

;;; Customize
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;;; File management
(put 'dired-find-alternate-file 'disabled nil)

;;; ELPA
(setq package-archives '(("ELPA" . "http://tromey.com/elpa/") 
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("SC"  . "http://joseito.republika.pl/sunrise-commander/")
                         ("MM" . "http://marmalade-repo.org/packages/")))

(defun package-change-archive ()
  "Changes the ELPA package archive"
  (interactive)
  (save-excursion
    (let* ((enable-recursive-minibuffers t)
          (keys (car* package-archives))
          (key (ido-completing-read "Archive: " keys nil t))
          (archive (find-by-string-key key package-archives))
          (url (cdr archive)))
      (setq package-archive-base url)
      (message "Changes package archive to %s" url))))