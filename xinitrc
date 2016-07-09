#!/bin/bash
# xinitrc
# Launch an X11 Desktop Environment
# The original path for this file is: ~/.xinitrc
# Last updated 2016-7-9
# Jun Go gojun077@gmail.com

# USAGE:
# xinit <DE_name>
# To launch DE_name

DEFAULT_SESSION=xfce4

case $1 in
  xmonad)
    exec ssh-agent monad
    ;;
  openbox)
    exec ssh-agent openbox-session
    ;;
  xfce4)
    exec ssh-agent startxfce4
    ;;
  awesome)
    exec ssh-agent awesome
    ;;
  *)
    exec $DEFAULT_SESSION
    ;;
esac

