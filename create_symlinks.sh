#!/bin/bash
# create symlinks for config files mostly under ~/ and other directories
# Do NOT run as root, as this will cause the $HOME variable to default
# to '/root' instead of ~/username and cause $USER to become 'root'
# instead of local user.
#
# Last updated 2016-08-01
# Jun Go gojun077@gmail.com


create_sym()
{
  # this function takes 2 string arguments:
  # (1) original file with full path
  # (2) replacement file with full path
  #
  # Given (1), the function will check if the file exists and is a symlink.
  # Depending on what it finds it will do the following
  # + File exists and _is not_ a symlink
  #   - file will be renamed to 'file.old'
  #   - create a symlink from (2) to (1)
  # + File exists and _is_ a symlink
  #   - exit the function
  # + File does not exist
  #   - create a symlink from (2) to (1)
  #
  # USAGE:
  # create_sym <orig file w/path> <replacement file w/path>

  ORIG=$1
  REPLACE=$2
  if [[ -f "$ORIG" && ! -L "$ORIG" ]]; then
    mv "$ORIG" "$ORIG".old
    ln -s "$REPLACE" "$ORIG"
  elif [[ -f "$ORIG" && -L "$ORIG" ]]; then
    echo -e "$ORIG exists and is already a symlink.\n"
  else
    ln -s "$REPLACE" "$ORIG"
  fi
}


printf "%s\n" "##################################################"
printf "%s\n" "# Check that the script is not being run as root #"
printf "%s\n" "##################################################"

if [ "$USER" = "root" ]; then
  printf "%s\n" "This script must not be executed as root."
  exit 1
fi


printf "%s\n" "##################################################"
printf "%s\n" "# Create Symlinks to dotfiles directly below ~/  #"
printf "%s\n" "##################################################"

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

#TERMINATOR CONFIG
if [ -f /usr/bin/terminator ]; then
  create_sym "$HOME/.config/terminator/config" "$HOME/dotfiles/terminator"
else
  echo "terminator is not installed on this machine"
fi

#IRSSI CONFIG
if [ -f /usr/bin/irssi ]; then
  if [ ! -d "$HOME/.irssi" ]; then
    mkdir "$HOME/.irssi"
  fi
  create_sym "$HOME/.irssi/config" "$HOME/dotfiles/irssi-config"
  if [ -f "$HOME/SpiderOak Hive/keys/irssi.pem" ]; then
    ln -s "$HOME/SpiderOak Hive/keys/irssi.pem" "$HOME/.irssi/"
  fi
else
  echo "irssi is not installed on this machine"
fi

#QUODLIBET CONFIG
if [ -f /usr/bin/quodlibet ]; then
  create_sym "$HOME/.quodlibet/stations" "$HOME/dotfiles/quod_stations"
else
  echo "quodlibet is not installed on this machine"
fi
# Note: Recent versions of quodlibet overwrite the symlinked
# 'stations' file and replace it with a regular file


#SSH CONFIG
if ! [ -d "$HOME/.ssh" ]; then
  mkdir -p "$HOME/.ssh"
  chmod 700 "$HOME/.ssh"
fi

create_sym "$HOME/.ssh/config" "$HOME/dotfiles/ssh-config"
chmod 600 "$HOME/.ssh/config"


######################################################
# Create Openbox Symlinks
######################################################
#OPENBOX AUTOSTART
if [ -f /usr/sbin/openbox-session ]; then
  create_sym "$HOME/.config/openbox/autostart" \
             "$HOME/dotfiles/openbox/openbox-autostart"
else
  echo "openbox DE is not installed on this machine"
fi

######################################################
# Create XFCE4 Symlinks
######################################################
# XFCE4 TERMINAL CONFIG
if [ -f /usr/bin/xfce4-terminal ]; then
  create_sym "$HOME/.config/xfce4/terminal/terminalrc" \
             "$HOME/dotfiles/xfce4/xfceTerm"
else
  echo "xfce-terminal is not installed on this machine"
fi

# XFCE4 MENU
#if [ -f /usr/bin/startxfce4 ]; then
#  create_sym "" "$HOME/dotfiles/xfce4/"
#fi

# XFCE4 PANEL

# XFCE4 KEYBOARD SHORTCUTS
if [ -f /usr/bin/startxfce4 ]; then
  create_sym "$HOME/.config/xfce4/xfconf/xfce-perchannel-xml/xfce4-keyboard-shortcuts.xml" \
    "$HOME/dotfiles/xfce4-keyboard-shortcuts.xml"
else
  echo "xfce4 is not installed on this machine"
fi

######################################################
# Create Symlinks to files under /root
######################################################

# NOTE: to create symlinks from root_bashrc to /root/.bashrc
# your regular user needs to have rwx permissions on /root
# and subdir's; make sure to run 'setACL_symlinks.sh' first

create_sym "/root/.bashrc" "$HOME/dotfiles/root_bashc"
create_sym "/root/.vimrc" "$HOME/dotfiles/vimrc"

######################################################
# Create Symlinks to files under /etc
######################################################

# NOTE: to create symlinks from ~/dotfiles/anacrontab
# to /etc/anacrontab, for example, the regular user needs
# to have rwx permissions on /etc and subdir's;
# make sure to run 'setACL_symlinks.sh' first


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
# NOTE ABOUT dnsmasq and SELINUX
# When you create a symlink from $HOME/dotfiles/dnsmasq to
# /etc/dnsmasq.conf, the SELINUX context will be
# 'unconfined_u:object_r:etc_t:s0'
# but the context should be
# 'system_u:system_r:dnsmasq_t:s0'
# To set 'dnsmasq_t' to permissive mode, execute the following:
# sudo semanage permissive -a dnsmasq_t
# OR
# You can change the context on ~/dotfiles/dnsmasq which is
# symlinked to /etc/dnsmasq.conf as follows:
# `sudo semanage fcontext -a -t dnsmasq_etc_t /home/fedjun/dotfiles/dnsmasq`
# `sudo restorecon -v /home/fedjun/dotfiles/dnsmasq`


if [ -f /usr/sbin/pacman ]; then
  create_sym "/etc/pacman.conf" "$HOME/dotfiles/pacman.conf"
  create_sym "/etc/pacman.d/mirrorlist" "$HOME/dotfiles/pacman_mirrorlist"
else
  echo "This system is not running Archlinux"
fi

if which bitlbee; then
  if grep "Fedora" /etc/redhat-release; then
    create_sym "/etc/bitlbee/bitlbee.conf" "$HOME/dotfiles/bitlbee_fedora"
  else
    create_sym "/etc/bitlbee/bitlbee.conf" "$HOME/dotfiles/bitlbee"
  fi
else
  echo "bitlbee is not installed on this machine"
fi

if [ -f /usr/bin/vsftpd ]; then
  create_sym "/etc/vsftpd.conf" "$HOME/dotfiles/vsftpd.conf"
else
  echo "vsftpd is not installed on this machine"
fi

if [ -f /usr/bin/motion ]; then
  SYSINFO=$(sudo dmidecode | grep -i "System Information" -A 8)
  # If machine is U36JC, use motion-u36jc.conf
  if [[ "$SYSINFO" = *U36JC* ]]; then
    create_sym "/etc/motion/motion.conf" "$HOME/dotfiles/motion-u36jc.conf"
  # For all other machines use regular motion.conf
  else
    create_sym "/etc/motion/motion.conf" "$HOME/dotfiles/motion.conf"
  fi
else
  echo "motion is not installed on this machine"
fi

# Set config for ssh daemon
create_sym "/etc/ssh/sshd_config" "$HOME/dotfiles/sshd_config"

if [ -f /usr/bin/ansible ]; then
  create_sym "/etc/ansible/ansible.cfg" "$HOME/dotfiles/ansible/ansible.cfg"
  create_sym "/etc/ansible/hosts" "$HOME/dotfiles/ansible/hosts"
  mkdir -p /etc/ansible/group_vars/
  create_sym "/etc/ansible/group_vars/ubuntu_base" \
             "$HOME/dotfiles/ansible/group_vars/ubuntu_base"
else
  echo "ansible is not installed on this machine"
fi

# Note: qemu.conf is Archlinux-specific customized
# TODO: create an alternate version for Fedora and cond'l check
if [ -f /usr/bin/libvirtd ]; then
  create_sym "/etc/libvirt/qemu.conf" "$HOME/dotfiles/qemu.conf"
else
  echo "libvirt is not installed on this machine"
fi


######################################################
# Vim-specific config files etc.
######################################################

# Create tmp dir's for vim
if ! [ -d "$HOME/tmp" ]; then
   mkdir "$HOME"/tmp
fi

if ! [ -d /root/tmp ]; then
  echo "Make sure to create /root/tmp for when vim is run as 'root'"
fi

# Create symlinks for vim syntax files
if ! [ -d "$HOME"/.vim/after/ftplugin ]; then
  mkdir -p "$HOME"/.vim/after/ftplugin
fi

create_sym "$HOME/.vim/after/ftplugin/yaml.vim" "$HOME/dotfiles/yaml.vim"
create_sym "$HOME/.vim/after/ftplugin/sh.vim" "$HOME/dotfiles/sh.vim"


######################################################
# Create symlinks for SSH keys on SpiderOak
######################################################
KEYS=(junAUR
      archjun_rsa
      cloud
      fx8350
     )

KEYLIST=$HOME/keylist.txt

for i in ${KEYS[*]}; do
  find "$HOME/SpiderOak Hive/keys/ssh" -type f -name "${i}*" >> "$KEYLIST"
done


while read -r key; do
  keyname=$(basename "$key")
  if ! [ -f "$HOME/.ssh/$keyname" ]; then
    printf "%s\n" "### Create symlink to ~/.ssh for $keyname ###"
    ln -s "$key" "$HOME/.ssh/"
  fi
done<"$KEYLIST"


printf "%s\n" "####################################################"
printf "%s\n" "#         Setup git user name and email            #"
printf "%s\n" "####################################################"
git config --global user.email "gojun077@gmail.com"
git config --global user.name "$USER"

printf "%s\n" "####################################################"
printf "%s\n" "#                Setup Mnemosyne                   #"
printf "%s\n" "####################################################"
ln -s "$HOME/Dropbox/mnemosyne" "$HOME/.local/share/"
