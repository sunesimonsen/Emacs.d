Emacs Setup
===========

Prerequisite

* Emacs 23
* emacs-color-theme

Run the following commands from bash:

    cd
    mv .emacs.d .emacs.d.bak
    git clone http://github.com/sunesimonsen/Emacs.d.git
    mv Emacs.d .emacs.d

Add the following command to your ~/.emacs file.

    (load (expand-file-name "~/.emacs.d/init.el"))
