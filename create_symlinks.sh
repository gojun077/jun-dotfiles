#!/bin/bash
# create symlinks for config files mostly under ~/ and other directories
# Do not run as root, as this will cause the $HOME variable to default
# to '/root' instead of ~/username
#
# Last updated 2016-07-06
# Jun Go gojun077@gmail.com


create_sym()
{
  # this function takes 2 string arguments:
  # (1) path and filename of original file
  # (2) path and filename of the replacement file
  #
  # Given (1), the function will check if the file exists and is a symlink.
  # Depending on what it finds it will do the following
  # + File exists and is not a symlink
  #   - file will be renamed to file.old
  #   - a symlink from (2) will be created in place of (1)
  # + File exists and is a symlink
  #   - exit the function
  # + File does not exist
  #   - create a symlink from (2) to (1)
  #
  # USAGE:
  # create_sym <path to orig file> <path to new file>

  if [[ -f "$1" && ! -L "$1" ]]; then
    mv "$1" "$1".old
    ln -s "$2" "$1"
  elif [[ -f "$1" && -L "$1" ]]; then
    echo -e "$1 exists and is already a symlink.\n"
  else
    ln -s "$2" "$1"
  fi
}



####################################################
# Create Symlinks to dotfiles directly below ~/
####################################################

DOTFILES="bashrc conkyrc emacs mrconfig screenrc vimrc xinitrc"

for i in $DOTFILES; do
  create_sym "$HOME/.$i" "$HOME/dotfiles/$i"
done


######################################################
# Create Symlinks to files in subdirectories of $HOME
######################################################

#CMUS CONFIG
if [ -f /usr/bin/cmus ]; then
  create_sym "$HOME/.config/cmus/lib.pl" "$HOME/dotfiles/cmus_libpl"
else
  echo "CMUS is not installed on this machine"
fi

#LXTERMINAL CONFIG
if [ -f /usr/bin/lxterminal ]; then
  create_sym "$HOME/.config/lxterminal/lxterminal.conf" \
	     "$HOME/dotfiles/lxterminal"
else
  echo "lxterminal is not installed on this machine"
fi

#MAME CONFIG
if [ -f /usr/bin/sdlmame ]; then
  mkdir -p "$HOME/.mame/ini"
  create_sym "$HOME/.mame/ini/mame.ini" "$HOME/dotfiles/mame.ini"
else
  echo "sdlmame is not installed on this machine"
fi

#OPENBOX CONFIG
if [ -f /usr/sbin/openbox-session ]; then
  create_sym "$HOME/.config/openbox/autostart" "$HOME/dotfiles/openbox-autostart"
else
  echo "openbox DE is not installed on this machine"
fi

#TERMINATOR CONFIG
if [ -f /usr/bin/terminator ]; then
  create_sym "$HOME/.config/terminator/config" "$HOME/dotfiles/terminator"
else
  echo "terminator is not installed on this machine"
fi

#XFCE TERMINAL CONFIG
if [ -f /usr/bin/xfce-terminal ]; then
  create_sym "$HOME/.config/xfce4/terminal/terminalrc" "$HOME/dotfiles/xfceTerm"
else
  echo "xfce-terminal is not installed on this machine"
fi

#IRSSI CONFIG
if [ -f /usr/bin/irssi ]; then
  create_sym "$HOME/.irssi/config" "$HOME/dotfiles/irssi-config"
else
  echo "irssi is not installed on this machine"
fi

#QUODLIBET CONFIG
if [ -f /usr/bin/quodlibet ]; then
  create_sym "$HOME/.quodlibet/stations" "$HOME/dotfiles/quod_stations"
else
  echo "quodlibet is not installed on this machine"
fi
# Note: In recent versions of quodlibet, it is now overwriting the
# symlinked 'stations' file and replacing it with a regular file
# with the same content. Workaround?

#SSH CONFIG
if ! [ -d "$HOME/.ssh" ]; then
  mkdir -p "$HOME/.ssh"
  chmod 700 "$HOME/.ssh"
fi

create_sym "$HOME/.ssh/config" "$HOME/dotfiles/ssh-config"
chmod 600 "$HOME/.ssh/config"


#TODO XFCE4 PANEL

#XFCE4 KEYBOARD SHORTCUTS
#if [ -f /usr/bin/startxfce4 ]; then
#  create_sym "$HOME/.config/xfce4/foo" \
#    "$HOME/dotfiles/xfce4-keyboard-shortcuts.xml"
#else
#  echo "xfce4 is not installed on this machine"
#fi

######################################################
# Create Symlinks to files under /root
######################################################

# NOTE: to create symlinks from root_bashrc to /root/.bashrc
# your regular user needs to have rwx permissions on /root
# you can achieve this using Access Control Lists
#
# setfacl -m "u:USER:rwx" /root

create_sym "/root/.bashrc" "$HOME/dotfiles/root_bashrc"
create_sym "/root/.vimrc" "$HOME/dotfiles/vimrc"

######################################################
# Create Symlinks to files in under /etc
######################################################

# NOTE: to create symlinks from ~/dotfiles/anacrontab
# to /etc/anacrontab, for example, the regular user needs
# to have rwx permissions on /etc
# you can achieve this using Access Control Lists
#
# setfacl -m "u:USER:rwx" /etc
# setfacl -m "u:USER:rwx" /etc/bitlbee/
# setfacl -m "u:USER:rwx" /etc/ssh/
# setfacl -m "u:$USER:rwx" /etc/motion/
# setfacl -R -m "u:$USER:rwx" /etc/ansible

if [ -f /etc/redhat-release ]; then
  create_sym "/etc/anacrontab" "$HOME/dotfiles/anacrontab_fedora"
else
  create_sym "/etc/anacrontab" "$HOME/dotfiles/anacrontab"
fi

if [ -f /usr/bin/dnsmasq ]; then
  create_sym "/etc/dnsmasq.conf" "$HOME/dotfiles/dnsmasq"
else
  echo "dnsmasq is not installed on this machine"
fi

if [ -f /usr/sbin/pacman ]; then
  create_sym "/etc/pacman.conf" "$HOME/dotfiles/pacman.conf"
else
  echo "This system is not running Archlinux"
fi

if [ -f /usr/bin/bitlbee ]; then
  create_sym "/etc/bitlbee/bitlbee.conf" "$HOME/dotfiles/bitlbee"
else
  echo "bitlbee is not installed on this machine"
fi
#TODO add permissions changes for bitlbee log in /var/lib/bitlbee
# setfacl -m "u:USERNAME:rwx" /var/lib/bitlbee

if [ -f /usr/bin/vsftpd ]; then
  create_sym "/etc/vsftpd.conf" "$HOME/dotfiles/vsftpd.conf"
else
  echo "vsftpd is not installed on this machine"
fi

if [ -f /usr/bin/motion ]; then
  create_sym "/etc/motion/motion.conf" "$HOME/dotfiles/motion.conf"
else
  echo "motion is not installed on this machine"
fi

create_sym "/etc/ssh/sshd_config" "$HOME/dotfiles/sshd_config"

if [ -f /usr/bin/ansible ]; then
  create_sym "/etc/ansible/ansible.cfg" "$HOME/dotfiles/ansible/ansible.cfg"
  create_sym "/etc/ansible/hosts" "$HOME/dotfiles/ansible/hosts"
  mkdir -p /etc/ansible/group_vars/
  create_sym "/etc/ansible/hosts/group_vars/ubuntu_base" \
	     "$HOME/dotfiles/ansible/group_vars/ubuntu_base"
else
  echo "ansible is not installed on this machine"
fi


######################################################
# Create tmp dir's for vim
######################################################
mkdir "$HOME"/tmp
mkdir /root/tmp
# setfacl for /root should already setup for regular
# user

######################################################
# Setup git user name and email
######################################################
git config --global user.email "gojun077@gmail.com"
git config --global user.name "$USER"
