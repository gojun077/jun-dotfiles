#!/bin/bash
# start_aw-watcher-window-wayland.sh
#
# This script is intended to be run by a systemd service
# and launches an Activity Watch client for watching Wayland
# windows. This only works for Sway WM >= v1.5

RUST_BACKTRACE=1 aw-watcher-window-wayland
