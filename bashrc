. ~/my/dotfiles/bash/aliases
. ~/my/dotfiles/bash/thirdparty/git-completions.bash

export PATH=~/my/dotfiles/bin:~/my/dotfiles/bin/thirdparty:$PATH

#export LESS='-i-P%f (%i/%m) Line %lt/%L'
export EDITOR=emacs
export VISUAL=emacs

# don't put duplicate lines in the history. See bash(1) for more options
# ... or force ignoredups and ignorespace
HISTCONTROL=ignoredups:ignorespace

# append to the history file, don't overwrite it
shopt -s histappend
# Number of lines of ~/.bash_history
HISTSIZE=10000
# Number of commands Bash will keep in memory
HISTFILESIZE=10000

# Write to ~/.bash_history immediately after each command is typed in.
export PROMPT_COMMAND='history -a'

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

if [ -e ~/.bashrc_private ];
then
  . ~/.bashrc_private
fi
