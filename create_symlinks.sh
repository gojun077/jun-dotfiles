#!/bin/bash
# create symlinks for config files mostly under ~/
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

  if [[ -f $1 && ! -L $1 ]]; then
    mv $1 $1.old
    ln -s $2 $1 
  elif [[ -f $1 && -L $1 ]]; then
    echo -e "$1 exists and is already a symlink.\n"
    exit 0
  else
    ln -s $2 $1
  fi
}


####################################################
# DECLARATIONS
####################################################
DOTFILESP="$HOME/dotfiles"
QUODCONF="quod_stations"


####################################################
# Create Symlinks to dotfiles directly below ~/
####################################################

DOTFILES="bashrc emacs screenrc vimrc conkyrc git-prompt"

for i in $DOTFILES; do
  create_sym "$HOME/.$i" "$HOME/dotfiles/$i"
done


######################################################
# Create Symlinks to files in subdirectories of $HOME
######################################################

# the 4-letter variable is the original path for the conf file
# the variable with '_TGT' appended is the original name of the conf
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
MAME="$HOME/.mame"
MAME_TGT="mame.ini"

# only rename original file and create symlink from $HOME/dotfiles
# if the config file exists in the given path and is not a symlink
# otw just create the symlink

if [[ -f $QUOD/$QUOD_TGT && ! -L $QUOD/$QUOD_TGT ]]; then
  mv $QUOD/$QUOD_TGT $QUOD/$QUOD_TGT.old
  ln -s $DOTFILESP/$QUODCONF $QUOD/$QUOD_TGT
else
  ln -s $DOTFILESP/$QUODCONF $QUOD/$QUOD_TGT
fi

if [[ -f $TMNTR/$TMNTR_TGT && ! -L $TMNTR/$TMNTR_TGT ]]; then
  mv $TMNTR/$TMNTR_TGT $TMNTR/$TMNTR_TGT.old
  ln -s $HOME/dotfiles/terminator $TMNTR/$TMNTR_TGT
else
  ln -s $HOME/dotfiles/terminator $TMNTR/$TMNTR_TGT
fi

if [[ -f $LXTM/$LXTM_TGT.conf && ! -L $LXTM/$LXTM_TGT ]]; then
  mv $LXTM/$LXTM_TGT $LXTM/$LXTM_TGT.old
  ln -s $HOME/dotfiles/lxterminal $LXTM/$LXTM_TGT
else
  ln -s $HOME/dotfiles/lxterminal $LXTM/$LXTM_TGT
fi

if [[ -f $XFTM/$XFTM_TGT.conf && ! -L $XFTM/$XFTM_TGT ]]; then
  mv $XFTM/$XFTM_TGT $XFTM/$XFTM_TGT.old
  ln -s $HOME/dotfiles/xfceTerm $XFTM/$XFTM_TGT
else
  ln -s $HOME/dotfiles/xfceTerm $XFTM/$XFTM_TGT
fi

if [[ -f $OPBX/$OPBX_TGT.conf && ! -L $OPBX/$OPBX_TGT ]]; then
  mv $OPBX/$OPBX_TGT $OPBX/$OPBX_TGT.old
  ln -s $HOME/dotfiles/openbox-autostart $OPBX/$OPBX_TGT
else
  ln -s $HOME/dotfiles/openbox-autostart $OPBX/$OPBX_TGT
fi

if [[ -f $MAME/$MAME_TGT.ini && ! -L $MAME/$MAME_TGT ]]; then
  mv $MAME/$MAME_TGT $MAME/$MAME_TGT.old
  ln -s $HOME/dotfiles/mame.ini $MAME/$MAME_TGT
else
  ln -s $HOME/dotfiles/mame.ini $MAME/$MAME_TGT
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
else
  ln -s $HOME/dotfiles/root_bashrc /root/.bashrc
fi

if [[ -f /root/.vimrc && ! -L /root/.vimrc ]]; then
  mv /root/.vimrc /root/.vimrc_old
  mkdir /root/tmp #path for vim swap and temp files (root)
  ln -s $HOME/dotfiles/vimrc /root/.vimrc
else
  ln -s $HOME/dotfiles/vimrc /root/.vimrc
fi
