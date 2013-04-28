# Git #################################################################
if [ -e ~/.git-completions.bash ];
then
  . ~/.git-completions.bash
fi

alias g=git
alias ga='git add'
alias gb='git branch'
alias gc='git checkout'
alias gd='git diff'
alias gf='git fetch'
alias gl='git log --date-order --pretty="format:%C(yellow)%h%Cblue%d%Creset %s %C(white) %an, %ar%Creset" --graph'
alias gs='git status'

#######################################################################

alias rm_rf='command rm -rf'
alias ..='c ..'
alias ...='c ../..'
alias r=irb
alias x='chmod +x'
alias rr='. ~/.bashrc'
alias brc='vim ~/.bashrc; . ~/.bashrc'
alias topcmds='cat ~/.bash_history | cut -d " " -f1 | sort | uniq -c | sort -nr | head -15'

alias l='my_ls'
function my_ls() {
  if [ $# = 1 ]; then
    if [ -f $1 ]; then
      less $1;
      return
    fi
  fi
  ls -G $*;
}

alias c="cd_ls"
function cd_ls () {
   if [ $# = 0 ]; then
      cd && ls -G
   else
      cd "$*" && ls -G
   fi
}

alias d='my_dir'
function my_dir() {
  mkdir $1 && c $1
}

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
