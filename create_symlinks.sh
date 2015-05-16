#!/bin/bash
# create symlinks for the following config files:
# bashrc  emacs  root_bashrc  screenrc
# to ~/.bashrc, ~/.emacs, /root/.bashrc, ~/.screenrc
# respectively

ln -s bashrc ~/.bashrc
ln -s emacs ~/.emacs
# Note that to create a symlink from root_bashrc to ~/root/.bashrc
# your regular user needs to have rwx permissions on /root
# you can achieve this using Access Control Lists
# setfacl -m "u:USERNAME:rwx" /root
ln -s root_bashrc ~/root/.bashrc
ln -s screenrc ~/.screenrc
