#!/bin/bash
# create symlinks for the following config files:
# bashrc  emacs  root_bashrc  screenrc
# to ~/.bashrc, ~/.emacs, /root/.bashrc, ~/.screenrc
# respectively

DOTFILES="bashrc emacs screenrc vimrc"
TARGETS=".bashrc .emacs .screenrc .vimrc"

for i in $TARGETS; do
  if [ -f ~/$i ]; then
    mv ~/$i ~/$i_old
  fi
done

for j in $DOTFILES; do
  ln -s ~/dotfiles/$j ~/.$j
done

# Note that to create a symlink from root_bashrc to ~/root/.bashrc
# your regular user needs to have rwx permissions on /root
# you can achieve this using Access Control Lists
# setfacl -m "u:USERNAME:rwx" /root

if [ -f /root/.bashrc ]; then
  mv /root/.bashrc /root/.bashrc_old
fi

ln -s ~/dotfiles/root_bashrc /root/.bashrc

if [ -L /root/.vimrc ]; then
  echo -e "symlink for .vimrc already exists\n"
elif [ -f /root/.vimrc ]; then
  mv /root/.vimrc /root/.vimrc_old
  mkdir /root/tmp #path for vim swap and temp files (root)
fi

ln -s ~/dotfiles/vimrc /root/.vimrc

if [ -f ~/.quodlibet/stations ]; then
  mv ~/.quodlibet/stations ~/.quodlibet/stations_old
fi

ln -s ~/dotfiles/stations ~/.quodlibet/stations
