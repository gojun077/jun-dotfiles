#!/bin/sh
xrandr --output LVDS-1-1 --off \
       --output DP-1-1 --off \
       --output VGA-1-1 --off \
       --output HDMI-1-1 --off \
       --output HDMI-0 --primary --mode 1920x1080 --pos 0x0 --rotate normal

# --output LVDS-1-1 --mode 1366x768 --pos 1920x312 --rotate normal
