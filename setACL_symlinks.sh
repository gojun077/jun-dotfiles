#!/bin/bash
# setACL_symlinks.sh
# Add $USER to Access Control List (ACL) so that create_symlinks.sh
# can create symlinks from ~/dotfiles to system directories like /root,
# /etc, ...
# This script should be executed as root

# Last updated 2016-08-01
# Jun Go gojun077@gmail.com

# USAGE ./setACL_symlinks.sh <username>
# example: sudo ./setACL_symlinks archjun


USER=$1

DIRS=(/root
      /etc
      /etc/ansible
      /etc/bitlbee
      /etc/motion
      /etc/libvirt
      /etc/pacman.d
      /etc/ssh
      /etc/xdg/menus
      /var/run/motion
)

FILES=(/var/log/motion.log
      )

DSA="/etc/ssh_host_dsa_key"
ECDSA="/etc/ssh_host_ecdsa_key"
ED25519="/etc/ssh_host_ed25519_key"
RSA="/etc/ssh_host_rsa_key"

HOSTPRIV=($DSA
          $ECDSA
          $ED25519
          $RSA
)

HOSTPUB=($DSA.pub
         $ECDSA.pub
         $ED25519.pub
         $RSA
)


if [ -z "$USER" ]; then
  echo "Must enter name of local user for setting ACL's"
  exit 1
fi

for i in ${DIRS[*]}; do
  if ! [ -d "$i" ]; then
    mkdir -p "$i"
  fi
  printf "%s\n" "### Give $USER access perm's for $i ###"
  setfacl -R -m "u:$USER:rwx" "$i"
  getfacl "$i"
done

for j in ${FILES[*]}; do
  if [ -f "$j" ]; then
    printf "%s\n" "### Give $USER access perm's for file $j ###"
    setfacl -m "u:$USER:rwx" "$j"
    getfacl "$j"
  fi
done

printf "%s\n" "### Set Proper Perm's on SSH Host Keys  ###"
for k in ${HOSTPRIV[*]}; do
  printf "%s\n" "Set $k to rw-------"
  chmod 600 "$k"
done

for h in ${HOSTPUB[*]}; do
  printf "%s\n" "Set $h to rw-r--r--"
  chmod 644 "$k"
done
