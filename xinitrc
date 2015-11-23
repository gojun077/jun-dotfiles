#!/bin/bash
# The original path and name of this file are: ~/.xinitrc
# The following settings will launch openbox when 'startx' is invoked

export BROWSER=chromium

DEFAULT_SESSION=xmonad

case $1 in
  xmonad)
    exec xmonad
    ;;
  openbox)
    exec dbus-launch openbox-session
    ;;
  xfce4)
    exec startxfce4
  *)
    exec $DEFAULT_SESSION
    ;;
esac
 
