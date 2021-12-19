#!/bin/bash
# start_aw-watcher-window-wayland.sh
#
# This script is intended to be run by a systemd service
# and launches an Activity Watch client for watching Wayland
# windows. This only works for Sway WM >= v1.5

RUST_BACKTRACE=full XDG_RUNTIME_DIR=/run/junbuntu/1000 \
  WAYLAND_DISPLAY=wayland-0 aw-watcher-window-wayland > \
  ~/.cache/activitywatch/log/aw-watcher-window-wayland/$(date -Iseconds).log \
  2>&1
