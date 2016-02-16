#!/bin/bash
# create symlinks for config files mostly under ~/ and other directories
# Do not run as root, as this will cause the $HOME variable to default
# to '/root' instead of ~/username
#
# First created in June 2015
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

DOTFILES="bashrc emacs screenrc vimrc conkyrc git-prompt xinitrc"

for i in $DOTFILES; do
  create_sym "$HOME/.$i" "$HOME/dotfiles/$i"
done


######################################################
# Create Symlinks to files in subdirectories of $HOME
######################################################

#CMUS CONFIG
create_sym "$HOME/.config/cmus/lib.pl" "$HOME/dotfiles/cmus_libpl"
#LXTERMINAL CONFIG
create_sym "$HOME/.config/lxterminal/lxterminal.conf" "$HOME/dotfiles/lxterminal"
#MAME CONFIG
create_sym "$HOME/.mame/mame.ini" "$HOME/dotfiles/mame.ini"
#OPENBOX CONFIG
create_sym "$HOME/.config/openbox/autostart" "$HOME/dotfiles/openbox-autostart"
#TERMINATOR CONFIG
create_sym "$HOME/.config/terminator/config" "$HOME/dotfiles/terminator"
#XFCE CONFIG
create_sym "$HOME/.config/xfce4/terminal/terminalrc" "$HOME/dotfiles/xfceTerm"
#IRSSI CONFIG
create_sym "$HOME/.irssi/config" "$HOME/dotfiles/irssi-config"
#QUODLIBET CONFIG
create_sym "$HOME/.quodlibet/stations" "$HOME/dotfiles/quod_stations"
#TODO XFCE4 PANEL XFCE TIMER CONFIG

#TODO XFCE4 KEYBOARD SHORTCUTS

######################################################
# Create Symlinks to files in under /root
######################################################

# NOTE: to create symlinks from root_bashrc to /root/.bashrc
# your regular user needs to have rwx permissions on /root
# you can achieve this using Access Control Lists
#
# setfacl -m "u:USERNAME:rwx" /root

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
# setfacl -m "u:USERNAME:rwx" /etc
# setfacl -m "u:USERNAME:rwx" /etc/bitlbee/
# setfacl -m "u:USERNAME:rwx" /etc/ssh/

create_sym "/etc/anacrontab" "$HOME/dotfiles/anacrontab"
create_sym "/etc/dnsmasq.conf" "$HOME/dotfiles/dnsmasq"
create_sym "/etc/pacman.conf" "$HOME/dotfiles/pacman.conf"
create_sym "/etc/bitlbee/bitlbee.conf" "$HOME/dotfiles/bitlbee"
#TODO add permissions changes for bitlbee log in /var/lib/bitlbee
# setfacl -m "u:USERNAME:rwx" /var/lib/bitlbee
create_sym "/etc/vsftpd.conf" "$HOME/dotfiles/vsftpd.conf"
create_sym "/etc/ssh/sshd_config" "$HOME/dotfiles/sshd_config"


######################################################
# Create tmp dir's for vim
######################################################
mkdir "$HOME"/tmp
mkdir /root/tmp

######################################################
# Setup git user name and email
######################################################
git config --global user.email "gojun077@gmail.com"
git config --global user.name "$USER"
