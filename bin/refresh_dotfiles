#!/bin/bash
# Get or update dotfiles.
if [ -d ~/my/dotfiles ]; then
    cd ~/my/dotfiles
    git diff --exit-code
    if [ "$?" -ne "0" ]; then
        read -p "stash local changes and get latest? " yn
        case $yn in
            [Nn]* ) exit 1;;
        esac
        echo stashing local changes
        git stash
    else
        echo "(no local changes)"
    fi
    echo getting latest...
    git pull
else
    echo setting up your dotfiles...
    mkdir -p ~/my
    cd ~/my
    git clone https://github.com/sri/dotfiles.git
    ln -s ~/my/dotfiles/emacs ~/.emacs
    ln -s ~/my/dotfiles/emacs.d ~/.emacs.d
    if [ -e ~/.bashrc ]; then
        mv ~/.bashrc ~/.bashrc.old
    fi
    ln -s ~/my/dotfiles/bashrc ~/.bashrc
fi
