#!/bin/bash
# setACL_symlinks.sh
# Add $USER to Access Control List (ACL) so that create_symlinks.sh
# can create symlinks for conf files in ~/dotfiles to system
# directories like /root, /etc, ...
# This script should be executed as root

# Last updated 2016-07-26
# Jun Go gojun077@gmail.com

# USAGE ./setACL_symlinks.sh <username>
# example: ./setACL_symlinks archjun

DIRS=(/root
      /etc
      /etc/ansible
      /etc/bitlbee
      /etc/motion
      /etc/libvirt
      /etc/pacman.d
      /etc/ssh
)

FILES=(/var/log/motion.log
)

# Make sure to change username $localuser to
# something appropriate for your machine
# Simply using $USER won't work because this script
# is executed as 'root'

if [ -z "$1" ]; then
  echo "Must enter name of local user for setting ACL's"
  exit 1
fi

for i in ${DIRS[*]}; do
  if ! [ -d "$i" ]; then
    mkdir -p "$i"
  fi
  setfacl -R -m "u:$1:rwx" "$i"
  getfacl "$i"
done

for j in ${FILES[*]}; do
  if [ -f "$j" ]; then
    setfacl -m "u:$1:rwx" "$j"
    getfacl "$j"
  fi
done

