#!/bin/bash
# create symlinks for config files mostly under ~/
# First created in June 2015
# Jun Go gojun077@gmail.com

####################################################
# Constant file names in dotfiles
####################################################
DOTFILESP="$HOME/dotfiles"
QUODCONF="quod_stations"


####################################################
# Create Symlinks to files directly below ~/
####################################################

DOTFILES="bashrc emacs screenrc vimrc conkyrc"

for i in $DOTFILES; do
  # if file exists and is not a symlink
  if [[ -f $HOME/.$i  && ! -L $HOME/.$i ]]; then
    # rename original file
    mv $HOME/.$i $HOME/.$i_old
    # create symlink from $HOME/dotfiles to ~/
    ln -s $HOME/dotfiles/$i $HOME/.$i
  fi
done


######################################################
# Create Symlinks to files in subdirectories of $HOME
######################################################

# the 4-letter variable is the original path for the conf file
# the variable with _TGT appended is the original name of the conf
# file in its original path

QUOD="$HOME/.quodlibet"
QUOD_TGT="stations"
TMNTR="$HOME/.config/terminator"
TMNTR_TGT="config"
LXTM="$HOME/.config/lxterminal"
LXTM_TGT="lxterminal.conf"
XFTM="$HOME/.config/xfce4/terminal"
XFTM_TGT="terminalrc"
OPBX="$HOME/.config/openbox"
OPBX_TGT="autostart"

# only rename original file and create symlink from $HOME/dotfiles
# if the config file exists in the given path and is not a symlink

if [[ -f $QUOD/$QUOD_TGT && ! -L $QUOD/$QUOD_TGT ]]; then
  mv $QUOD/$QUOD_TGT $QUOD/$QUOD_TGT.old
  ln -s $DOTFILESP/$QUODCONF $QUOD/$QUOD_TGT
fi

if [[ -f $TMNTR/$TMNTR_TGT && ! -L $TMNTR/$TMNTR_TGT ]]; then
  mv $TMNTR/$TMNTR_TGT $TMNTR/$TMNTR_TGT.old
  ln -s $HOME/dotfiles/terminator $TMNTR/$TMNTR_TGT
fi

if [[ -f $LXTM/$LXTM_TGT.conf && ! -L $LXTM/$LXTM_TGT ]]; then
  mv $LXTM/$LXTM_TGT $LXTM/$LXTM_TGT.old
  ln -s $HOME/dotfiles/lxterminal $LXTM/$LXTM_TGT
fi

if [[ -f $XFTM/$XFTM_TGT.conf && ! -L $XFTM/$XFTM_TGT ]]; then
  mv $XFTM/$XFTM_TGT $XFTM/$XFTM_TGT.old
  ln -s $HOME/dotfiles/xfceTerm $XFTM/$XFTM_TGT
fi

if [[ -f $OPBX/$OPBX_TGT.conf && ! -L $OPBX/$OPBX_TGT ]]; then
  mv $OPBX/$OPBX_TGT $OPBX/$OPBX_TGT.old
  ln -s $HOME/dotfiles/openbox-autostart $OPBX/$OPBX_TGT
fi

# Note that to create a symlink from root_bashrc to ~/root/.bashrc
# your regular user needs to have rwx permissions on /root
# you can achieve this using Access Control Lists
# setfacl -m "u:USERNAME:rwx" /root

# mv original file so we can create symlink

# if file exists and is not a symlink, rename it and create symlink
# from $HOME/dotfiles
if [[ -f /root/.bashrc && ! -L /root/.bashrc ]]; then
  mv /root/.bashrc /root/.bashrc_old
  ln -s $HOME/dotfiles/root_bashrc /root/.bashrc
fi

if [[ -f /root/.vimrc && ! -L /root/.vimrc ]]; then
  mv /root/.vimrc /root/.vimrc_old
  mkdir /root/tmp #path for vim swap and temp files (root)
  ln -s $HOME/dotfiles/vimrc /root/.vimrc
fi

# TODO
# if-stmt blocks are quite similar; create a bash function named
# something like "renameAndSymlink" that accepts the parameters
# (1) original path, (2) original target file, (3) dotfile name
# and that checks if (2) exists and is not a symlink, renames
# (2) if the condition is satisfied, and creates a symlink from
# (3) to (1)'/'(2)
