#!/bin/bash
# xinitrc
# Launch an X11 Desktop Environment
# The original path for this file is: ~/.xinitrc
# Last updated 2019.03.26
# Jun Go gojun077@gmail.com

# USAGE:
# startx ~/.xinitrc <DE_name>
# To launch DE_name

DEFAULT_SESSION=xfce4

case $1 in
  i3)
    exec ssh-agent i3
    ;;
  openbox)
    exec ssh-agent openbox-session
    ;;
  xfce4)
    exec ssh-agent startxfce4
    ;;
  *)
    exec $DEFAULT_SESSION
    ;;
esac

