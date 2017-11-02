#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

export BROWSER=firefox
export EDITOR=emacs

##### ibus IME settings #####
export GTK_IM_MODULE=ibus
export XMODIFIERS=@im=ibus
export QT_IM_MODULE=ibus
export CLUTTER_IM_MODULE=ibus
export ECORE_IMF_MODULE=xim
# Note: for ibus to work in non-GTK/QT apps, the ibus-daemon
# must be launched with the following options:
#env IBUS_ENABLE_SYNC_MODE=0 ibus-daemon -rdx &
#############################

export TERM=xterm-256color
export WINEPREFIX=/MULTIMEDIA/wine

if [ -f  /usr/sbin/pacman ]; then
  archey3 # Archlinux only
fi

PS1='[\u@\h \W]\$ '
alias ls='ls --color=auto'
alias pxe='cd /usr/local/tftpboot/pxelinux'
alias meditation='cd /usr/local/apache2/htdocs/cam1'
alias vi='vim'
#alias fbterm='fbterm -n "WenQuanYi Micro Hei" --font-width=11 -s 14'
alias ㅊㅇ='cd'
alias 니='ls'
alias wanderer='cd /MULTIMEDIA/vagrant_boxen'
alias startxfce4='ssh-agent startxfce4'
# When you run ssh-agent for the first time, you need to add
# your private key(s) with 'ssh-add /path/to/priv/key'
alias genpw='date +%s | sha256sum | base64 | head -c 32 ; echo'
alias wine='WINEARCH=win32 WINEPREFIX=/MULTIMEDIA/wine /usr/bin/wine'
alias winecfg='WINEARCH=win32 WINEPREFIX=/MULTIMEDIA/wine winecfg'

# vim: ts=2 sw=2 et :
export PATH="$HOME/.cargo/bin:$PATH"
