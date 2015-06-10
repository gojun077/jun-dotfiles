#!/bin/bash
# create symlinks for the following config files:
# bashrc  emacs  root_bashrc  screenrc
# to ~/.bashrc, ~/.emacs, /root/.bashrc, ~/.screenrc
# respectively


DOTFILES="bashrc emacs screenrc vimrc"

for i in $DOTFILES; do
  # if file exists and is not a symlink
  if [[ -f $HOME/.$i  && ! -L $HOME/.$i ]]; then
    # mv original files
    mv $HOME/.$i $HOME/.$i_old
    # create symlinks for dotfiles in ~/
    ln -s $HOME/dotfiles/$i $HOME/.$i
  fi
done


AP_TARGETS="$HOME/.quodlibet/stations $HOME/.config/terminator/config \
$HOME/.config/lxterminal/lxterminal.conf $HOME/.config/xfce4/terminal/terminalrc"

for i in $AP_TARGETS; do
  if [[ -f $i && ! -L $i ]]; then
    mv $i $i.old
  fi
done

ln -s $HOME/dotfiles/stations $HOME/.quodlibet/stations
ln -s $HOME/dotfiles/terminator $HOME/.config/terminator/config
ln -s $HOME/dotfiles/lxterminal $HOME/.config/lxterminal/lxterminal.conf
ln -s $HOME/dotfiles/xfceTerm $HOME/.config/xfce4/terminal/terminalrc


# Note that to create a symlink from root_bashrc to ~/root/.bashrc
# your regular user needs to have rwx permissions on /root
# you can achieve this using Access Control Lists
# setfacl -m "u:USERNAME:rwx" /root

# mv original file so we can create symlink

# if file exists and is not a symlink
if [[ -f /root/.bashrc && ! -L /root/.bashrc ]]; then
  mv /root/.bashrc /root/.bashrc_old
fi

ln -s $HOME/dotfiles/root_bashrc /root/.bashrc

if [[ -f /root/.vimrc && ! -L /root/.vimrc ]]; then
  mv /root/.vimrc /root/.vimrc_old
  mkdir /root/tmp #path for vim swap and temp files (root)
fi

ln -s $HOME/dotfiles/vimrc /root/.vimrc

