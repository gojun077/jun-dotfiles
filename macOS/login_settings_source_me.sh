#!/usr/bin/env bash
# login_settings_source_me.sh
#
# Last Updated: Mar 4 2024
#
# This file is for MacOS ONLY, and is intended to be `source`d, not
# executed!
#
# Source this file in a new shell after booting.
# `login_settings_source_me.sh` plays the role of `.bash_profile` in Linux 
# and other non-macOS POSIX environments.
#
# On MacOS, however, `.bash_profile` acts differently because every shell
# in the macOS GUI is a login shell. This means that settings in
# `.bash_profile` will be executed every time a new terminal session is
# started! So if you have some recursive settings like
# `export PATH=/foo/mypath:$PATH`, your PATH var will get hopelessly
# cluttered if you declare it in macOS `~/.bash_profile`
#
# This script contains settings that should only be defined once per
# GUI shell session. The best case is if you are using a terminal
# multiplexer like GNU Screen or `tmux`; before launching the
# multiplexer in a terminal, first `source login_settings_source_me.sh`
# then start the multiplexer session. Then the multiplexer panes will
# inherit all the declarations made below.

#sudo launchctl limit maxfiles 65536 200000
#ulimit -n 65536 200000

if [ -f ~/.bashrc ]; then
  source ~/.bashrc
fi

# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/bin" ] ; then
    PATH="$HOME/bin:$PATH"
fi

if [ -d "$HOME/.local/bin" ] ; then
    PATH="$HOME/.local/bin:$PATH"
fi

# Nix
if [ -e '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh' ]; then
  . '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh'
fi
# End Nix
# The above will add ~/.nix-profile/bin to your PATH start
# nix daemon

export GOROOT=/usr/local/go
export GOPATH="/Users/$USER/go"
export PYENV_ROOT="$HOME/.pyenv"
export PATH="/opt/homebrew/bin:$PYENV_ROOT:$GOROOT/bin:$GOPATH/bin:$PATH"
#export NIX_PATH="$HOME/.nix-defexpr"

# Set PATH variable to enable shims, no shell integration
#eval "$(pyenv init --path)"
# Install pyenv into your shell as shell function, enable shims,
# enable autocomplete
eval "$(pyenv init -)"
ssh-add --apple-load-keychain
