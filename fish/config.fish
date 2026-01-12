# -*- mode: fish -*-

if status is-interactive
    # Commands to run in interactive sessions can go here
end


### PATH
set -U fish_user_paths ~/my/dotfiles/bin $fish_user_paths

fish_add_path /opt/homebrew/bin



### Aliases
alias sub "/Applications/Sublime Text.app/Contents/SharedSupport/bin/subl"

### asdf
# asdf has native fish support
if test -f ~/.asdf/asdf.fish
    source ~/.asdf/asdf.fish
end

### rbenv
#if test -x ~/.rbenv/bin/rbenv
#    status --is-interactive; and source (rbenv init - fish | psub)
#end
#
### History (Fish already does most of this by default)
set -U fish_history main
set -g fish_history_limit 10000

### Environment variables
set -x PYTHONSTARTUP ~/.pythonrc.py
set -x RIPGREP_CONFIG_PATH ~/.ripgreprc

### Cargo
if test -f ~/.cargo/env.fish
    source ~/.cargo/env.fish
end

############################################################
# Aliases & Functions
############################################################

alias tab 'open . -a iterm'

alias d 'git diff'
alias gd 'git diff'
alias s 'git status --short --branch'
alias gs 'git status'
alias gg 'git grep'
alias ggi 'git grep -i'

function gc
    if test (count $argv) -eq 0
        set url (pbpaste)
        if string match -r '^http' -- $url
            git clone $url
        else
            echo "Error: Clipboard content does not start with 'http'."
        end
    else
        git clone $argv[1]
    end
end

function g
    set search $argv[1]
    set downcased (string lower $search)

    if test "$search" = "$downcased"
        git grep --color --break -n -i $search
    else
        git grep --color --break -n $search
    end
end

alias gcontains 'git branch -a --contains'
alias gwhich 'git branch -a | grep -i'
alias gss 'git show -p'
alias gds 'git diff --staged'

alias gl 'git log --graph --all --decorate --oneline -5'
alias gall 'git log --graph --all --decorate --no-merges'
alias glp 'gl -p'
alias gp 'git pull'

alias cdr 'cd (git rev-parse --show-toplevel)'

alias ip 'dig +short myip.opendns.com @resolver1.opendns.com'

alias .. 'c ..'
alias dt 'c ~/Desktop'
alias cddotfiles 'c ~/my/dotfiles'

alias x 'chmod +x'

alias rr 'source ~/.config/fish/config.fish'
alias brc 'emacsclient ~/.zshrc; source ~/.config/fish/config.fish'

alias df 'df -h'
alias du 'du -c -h'
alias du1 'du --max-depth=1'

alias topcmds 'cat ~/.local/share/fish/fish_history | grep -oE "cmd=.*" | sed "s/cmd=//" | sort | uniq -c | sort -nr | head -15'


# ls
alias ll 'eza --long --sort modified --total-size --almost-all --group-directories-first'
alias llsz 'eza --long --sort size --total-size --almost-all --group-directories-first'
alias llt 'eza --tree'

alias curl_info 'curl -so /dev/null -w "dnslookup: %{time_namelookup} | connect: %{time_connect} | appconnect: %{time_appconnect} | pretransfer: %{time_pretransfer} | starttransfer: %{time_starttransfer} | total: %{time_total} | size: %{size_download}\n"'

alias r my_ruby
function my_ruby
    if test -f ./Gemfile
        if set -q INSIDE_EMACS
            rails console -- --inf-ruby-mode
        else
            rails console
        end
    else
        if set -q INSIDE_EMACS
            irb --inf-ruby-mode
        else
            irb
        end
    end
end

alias p my_python
function my_python
    if command -sq ipython
        if set -q INSIDE_EMACS
            ipython --simple-prompt $argv
        else
            ipython $argv
        end
    else if command -sq python3
        python3 $argv
    else
        python $argv
    end
end

alias l my_ls
function my_ls
    if test (count $argv) -eq 1; and test -f $argv[1]
        less $argv[1]
    else
        ll $argv
    end
end

alias c cd_ls
function cd_ls
    if test (count $argv) -eq 0
        cd; and ls -G
    else
        cd $argv; and ls -G
    end
end

function e
    ~/progs/emacs/bin/emacs &
end

function ev
    set var (string upper $argv[1])
    eval echo "$var=\$$var"
end

function evall
    set var (string upper $argv[1])
    env | grep $var | sort
end

function find_broken_symlinks
    find . -type l ! -exec test -e {} \; -print
end

if type -q oh-my-posh
    oh-my-posh init fish | source
end
/Users/sri/.local/bin/mise activate fish | source
