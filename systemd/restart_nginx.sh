#!/bin/bash
# restart_nginx.sh

# This is intended to be run by a custom systemd service/timer
# on a Debian/Ubuntu distro (note path to 'systemctl').

/bin/systemctl restart nginx
