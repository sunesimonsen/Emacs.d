Emacs Setup
===========

Instalation
-----------

Run the following commands from bash:

    cd 
    mkdir -p .emacs.d
    cd .emacs.d
    git clone git@github.com:sunesimonsen/Emacs.d.git

Add the following command to your ~/.emacs file.

    (load (expand-file-name "~/.emacs.d/custom.el"))
