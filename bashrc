#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

export EDITOR=emacs

export TERM=xterm-256color

PS1='[\u@\h \W]\$ '
alias ls='ls --color=auto'
alias vi='vim'
alias ㅊㅇ='cd'
alias 니='ls'
# When you run ssh-agent for the first time, you need to add
# your private key(s) with 'ssh-add /path/to/priv/key'
alias genpw='date +%s | shasum -a 256 | base64 | head -c 32 ; echo'

# vim: ts=2 sw=2 et :
