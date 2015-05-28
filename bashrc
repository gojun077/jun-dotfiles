#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

export EDITOR=emacs
export GTK_IM_MODULE=ibus
export XMODIFIERS=@im=ibus
export QT_IM_MODULE=ibus


archey3
PS1='[\u@\h \W]\$ '
alias ls='ls --color=auto'
alias pxe='cd /usr/local/tftpboot/pxe'
alias meditation='cd /usr/local/apache2/htdocs/cam1'
alias vi='vim'
