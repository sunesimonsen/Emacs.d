;;; sunrise-commander-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (sr-dired sunrise-cd sunrise sr-virtual-mode sr-mode)
;;;;;;  "sunrise-commander" "sunrise-commander.el" (20199 49415))
;;; Generated autoloads from sunrise-commander.el

(autoload 'sr-mode "sunrise-commander" "\
Two-pane file manager for Emacs based on Dired and inspired by MC.
The following keybindings are available:

        /, j .......... go to directory
        p, n .......... move cursor up/down
        M-p, M-n ...... move cursor up/down in passive pane
        ^, J .......... go to parent directory
        M-^, M-J ...... go to parent directory in passive pane
        Tab ........... switch to other pane
        C-Tab.......... switch to viewer window
        C-c Tab ....... switch to viewer window (console compatible)
        RET, f ........ visit selected file/directory
        M-RET, M-f .... visit selected file/directory in passive pane
        C-c RET ....... visit selected in passive pane (console compatible)
        b ............. visit selected file/directory in default browser
        F ............. visit all marked files, each in its own window
        C-u F ......... visit all marked files in the background
        o,v ........... quick visit selected file (scroll with C-M-v, C-M-S-v)
        C-u o, C-u v .. kill quick-visited buffer (restores normal scrolling)
        X ............. execute selected file
        C-u X.......... execute selected file with arguments

        + ............. create new directory
        M-+ ........... create new empty file(s)
        C ............. copy marked (or current) files and directories
        R ............. rename marked (or current) files and directories
        D ............. delete marked (or current) files and directories
        S ............. soft-link selected file/directory to passive pane
        Y ............. do relative soft-link of selected file in passive pane
        H ............. hard-link selected file to passive pane
        K ............. clone selected files and directories into passive pane
        M-C ........... copy (using traditional dired-do-copy)
        M-R ........... rename (using traditional dired-do-rename)
        M-D ........... delete (using traditional dired-do-delete)
        M-S............ soft-link (using traditional dired-do-symlink)
        M-Y............ do relative soft-link (with traditional dired-do-relsymlink)
        M-H............ hard-link selected file/directory (with dired-do-hardlink)
        A ............. search marked files for regular expression
        Q ............. perform query-replace-regexp on marked files
        C-c s ......... start a \"sticky\" interactive search in the current pane

        M-a ........... move to beginning of current directory
        M-e ........... move to end of current directory
        M-y ........... go to previous directory in history
        M-u ........... go to next directory in history
        C-M-y ......... go to previous directory in history on passive pane
        C-M-u ......... go to next directory in history on passive pane

        g, C-c C-c .... refresh pane
        s ............. sort entries (by name, number, size, time or extension)
        r ............. reverse the order of entries in the active pane (sticky)
        C-o ........... show/hide hidden files (requires dired-omit-mode)
        C-Backspace ... hide/show file attributes in pane
        C-c Backspace . hide/show file attributes in pane (console compatible)
        y ............. show file type / size of selected files and directories.
        M-l ........... truncate/continue long lines in pane
        C-c v ......... put current panel in VIRTUAL mode
        C-c C-v ....... create new pure VIRTUAL buffer
        C-c C-w ....... browse directory tree using w3m

        M-t ........... transpose panes
        M-o ........... synchronize panes
        C-c C-s ....... change panes layout (vertical/horizontal/top-only)
        [ ............. enlarges the right pane by 5 columns
        ] ............. enlarges the left pane by 5 columns
        } ............. enlarges both panes vertically by 1 row
        C-} ........... enlarges both panes vertically as much as it can
        C-c } ......... enlarges both panes vertically as much as it can
        { ............. shrinks both panes vertically by 1 row
        C-{ ........... shrinks both panes vertically as much as it can
        C-c { ......... shrinks both panes vertically as much as it can
        \\ ............. sets size of all windows back to «normal»
        C-c C-z ....... enable/disable synchronized navigation

        C-= ........... smart compare files (ediff)
        C-c = ......... smart compare files (console compatible)
        = ............. fast smart compare files (plain diff)
        C-M-= ......... compare panes
        C-x = ......... compare panes (console compatible)

        C-c C-f ....... execute Find-dired in Sunrise VIRTUAL mode
        C-c C-n ....... execute find-Name-dired in Sunrise VIRTUAL mode
        C-c C-g ....... execute find-Grep-dired in Sunrise VIRTUAL mode
        C-c C-l ....... execute Locate in Sunrise VIRTUAL mode
        C-c C-r ....... browse list of Recently visited files (requires recentf)
        C-c C-c ....... [after find, locate or recent] dismiss virtual buffer
        C-c / ......... narrow the contents of the current pane using fuzzy matching
        C-c b ......... partial Branch view of selected items in the current pane
        C-c p ......... Prune paths matching regular expression from current pane
        ; ............. follow file (go to same directory as selected file)
        M-; ........... follow file in passive pane
        C-M-o ......... follow a projection of current directory in passive pane

        C-> ........... save named checkpoint (a.k.a. \"bookmark panes\")
        C-c > ......... save named checkpoint (console compatible)
        C-.    ........ restore named checkpoint
        C-c .  ........ restore named checkpoint

        C-x C-q ....... put pane in Editable Dired mode (commit with C-c C-c)
        @! ............ fast backup files (but not dirs!), each to [filename].bak

        C-c t ......... open new terminal or switch to already open one
        C-c T ......... open terminal AND/OR change directory to current
        C-c C-t ....... open always a new terminal in current directory
        C-c M-t ....... open a new terminal using an alternative shell program
        q, C-x k ...... quit Sunrise Commander, restore previous window setup
        M-q ........... quit Sunrise Commander, don't restore previous windows

Additionally, the following traditional commander-style keybindings are provided
\(these may be disabled by customizing the `sr-use-commander-keys' option):

        F2 ............ go to directory
        F3 ............ quick visit selected file
        F4 ............ visit selected file
        F5 ............ copy marked (or current) files and directories
        F6 ............ rename marked (or current) files and directories
        F7 ............ create new directory
        F8 ............ delete marked (or current) files and directories
        F10 ........... quit Sunrise Commander
        C-F3 .......... sort contents of current pane by name
        C-F4 .......... sort contents of current pane by extension
        C-F5 .......... sort contents of current pane by time
        C-F6 .......... sort contents of current pane by size
        C-F7 .......... sort contents of current pane numerically
        S-F7 .......... soft-link selected file/directory to passive pane
        Insert ........ mark file
        C-PgUp ........ go to parent directory

Any other dired keybinding (not overridden by any of the above) can be used in
Sunrise, like G for changing group, M for changing mode and so on.

Some more bindings are provided for terminals in line mode, most useful after
opening a terminal in the viewer window (with C-c t):

       (these two are only for external shells - bash, ksh, etc. not for eshell)
        C-c C-j ....... put terminal in line mode
        C-c C-k ....... put terminal back in char mode

        M-<up>, M-P ... move cursor up in active pane
        M-<down>, M-N . move cursor down in active pane
        M-Return ...... visit selected file/directory in active pane
        M-J ........... go to parent directory in active pane
        M-M ........... mark selected file/directory in active pane
        M-Backspace ... unmark previous file/directory in active pane
        M-U ........... remove all marks from active pane
        C-Tab ......... switch focus to active pane

In a terminal in line mode the following substitutions are also performed
automatically:

       %f - expands to the currently selected file in the left pane
       %F - expands to the currently selected file in the right pane
       %m - expands to the list of paths of all marked files in the left pane
       %M - expands to the list of paths of all marked files in the right pane
       %n - expands to the list of names of all marked files in the left pane
       %N - expands to the list of names of all marked files in the right pane
       %d - expands to the current directory in the left pane
       %D - expands to the current directory in the right pane
       %a - expands to the list of paths of all marked files in the active pane
       %A - expands to the current directory in the active pane
       %p - expands to the list of paths of all marked files in the passive pane
       %P - expands to the current directory in the passive pane
       %% - inserts a single % sign.

\(fn)" t nil)

(autoload 'sr-virtual-mode "sunrise-commander" "\
Sunrise Commander Virtual Mode. Useful for reusing find and locate results.

\(fn)" t nil)

(autoload 'sunrise "sunrise-commander" "\
Start the Sunrise Commander.
If LEFT-DIRECTORY is given, the left window will display that
directory (same for RIGHT-DIRECTORY). Specifying nil for any of
these values uses the default, ie. $HOME.

\(fn &optional LEFT-DIRECTORY RIGHT-DIRECTORY FILENAME)" t nil)

(autoload 'sunrise-cd "sunrise-commander" "\
Run Sunrise but give it the current directory to use.

\(fn)" t nil)

(autoload 'sr-dired "sunrise-commander" "\
Visit the given directory in `sr-mode'.

\(fn DIRECTORY &optional SWITCHES)" t nil)

;;;***

;;;### (autoloads nil nil ("sunrise-commander-pkg.el") (20199 49415
;;;;;;  642526))

;;;***

(provide 'sunrise-commander-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; sunrise-commander-autoloads.el ends here
