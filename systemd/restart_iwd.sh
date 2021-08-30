#!/bin/bash
# restart_iwd.sh

# This is intended to be run by a custom systemd service/timer

/usr/bin/systemctl restart iwd
