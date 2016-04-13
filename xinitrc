#!/bin/bash
# The original path for this file is: ~/.xinitrc
# To launch different DE's, 'startx foo' where 'foo' is
# the name of the DE

export BROWSER=chromium

DEFAULT_SESSION=xfce4

case $1 in
  xmonad)
    exec xmonad
    ;;
  openbox)
    exec openbox-session
    ;;
  xfce4)
    exec startxfce4
    ;;
  awesome)
    exec awesome
    ;;
  *)
    exec $DEFAULT_SESSION
    ;;
esac
 
