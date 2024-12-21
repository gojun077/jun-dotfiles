#!/usr/bin/env bash
# login_settings_source_me.sh
#
# Created on: Sometime in 2024
# Last Updated: 09 Sep 2024
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

# Nix
# The below script is not fully working on MacOS 15 Beta
# 'export' declarations seem to work, but PATH is not being updated
if [ -e '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh' ]; then
  . '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh'
fi
# End Nix

# The above is supposed to add ~/.nix-profile/bin to your PATH start
# and set other env vars for NIX to function correctly, but on MacOS
# 15 Beta PATH is not being updated so I will do it manually in the
# PATH declaration below:

export GOROOT=/usr/local/go
export GOPATH="/Users/$USER/go"
export HOMEBREW_PREFIX=/opt/homebrew
export HOMEBREW_CELLAR=/opt/homebrew/Cellar
export PYENV_ROOT="$HOME/.pyenv"
#export NIX_PATH="$HOME/.nix-defexpr"
export RVMPATH="$HOME/.rvm/bin"

export PATH="$HOME/bin:$HOME/.nix-profile/bin:/nix/var/nix/profiles/default/bin:$HOME/google-cloud-sdk/bin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/user/local/MacGPG2/bin:/Library/TeX/texbin:/opt/homebrew/bin:$PYENV_ROOT:$GOROOT/bin:$GOPATH/bin:$HOME/.krew/bin"

# Set PATH variable to enable shims, no shell integration
#eval "$(pyenv init --path)"
# Install pyenv into your shell as shell function, enable shims,
# enable autocomplete
eval "$(pyenv init -)"
eval "$(fzf --bash)"
ssh-add --apple-load-keychain
