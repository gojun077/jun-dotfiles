#!/bin/bash
# xinitrc
# Launch an X11 Desktop Environment
# The original path for this file is: ~/.xinitrc
# Last updated Sun 29 Jun 2025
# Peter Jun Go  gopeterjun@naver.com

# USAGE:
# startx ~/.xinitrc <DE_name>
# To launch DE_name

DEFAULT_SESSION=startxfce4

case $1 in
  i3)
    exec ssh-agent i3
    ;;
  lxde)
    exec ssh-agent startlxde
    ;;
  openbox)
    exec ssh-agent openbox-session
    ;;
  gnome)
    exec ssh-agent gnome-session
    ;;
  kde)
    exec ssh-agent startplasma-x11
    ;;
  xfce4)
    exec ssh-agent startxfce4
    ;;
  *)
    exec $DEFAULT_SESSION
    ;;
esac
