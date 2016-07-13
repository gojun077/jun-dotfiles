#!/bin/bash
# setACL_symlinks.sh
# Add $USER to Access Control List (ACL) so that create_symlinks.sh
# can create symlinks for conf files in ~/dotfiles to system
# directories like /root, /etc, ...
# This script should be executed as root

# Last updated 2016-07-08
# Jun Go gojun077@gmail.com

DIRS=(/root
      /etc
      /etc/ansible
      /etc/bitlbee
      /etc/motion
      /etc/libvirt
      /etc/pacman.d
      /etc/ssh
)

# Make sure to change username $localuser to
# something appropriate for your machine
# Simply using $USER won't work because this script
# is executed as 'root'

localuser=archjun

for i in ${DIRS[*]}; do
  if ! [ -d "$i" ]; then
    mkdir -p "$i"
  fi
  setfacl -R -m "u:$localuser:rwx" "$i"
  getfacl "$i"
done
