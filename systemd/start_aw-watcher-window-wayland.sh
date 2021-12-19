#!/bin/bash
# start_aw-watcher-window-wayland.sh
#
# This script is intended to be run by a systemd service
# and launches an Activity Watch client for watching Wayland
# windows. This only works for Sway WM >= v1.5

RUST_BACKTRACE=full aw-watcher-window-wayland /home/junbuntu/.cache/activitywatch/log/aw-watcher-window-wayland/$(date -Iseconds).log 2>&1 &
if $? -eq 0; then
  printf "%s\n" "aw-watcher-window-wayland launch SUCCESS"
else
  printf "%s\n" "aw-watcher-window-wayland launch FAILURE"
fi

