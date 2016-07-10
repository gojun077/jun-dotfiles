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
)


for i in ${DIRS[*]}; do
  if ! [ -d "$i" ]; then
    mkdir -p "$i"
  fi
  setfacl -R -m "u:$USER:rwx" "$i"
done
