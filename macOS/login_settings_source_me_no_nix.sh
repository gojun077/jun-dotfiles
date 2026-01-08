#!/usr/bin/env bash
# login_settings_source_me.sh
#
# Created on: Wed 07 Jan 2026
# Last Updated: Thu 08 Jan 2026
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

export GOROOT=/usr/local/go
export GOPATH="/Users/$USER/go"
export HOMEBREW_PREFIX=/opt/homebrew
export HOMEBREW_CELLAR=/opt/homebrew/Cellar
export PYENV_ROOT="$HOME/.pyenv"

export PATH="$HOME/.local/bin:$HOME/bin:/usr/local/bin:/System/Cryptexes/App/usr/bin:/usr/bin:/bin:/usr/sbin:/sbin:/var/run/com.apple.security.cryptexd/codex.system/bootstrap/usr/local/bin:/var/run/com.apple.security.cryptexd/codex.system/bootstrap/usr/bin:/var/run/com.apple.security.cryptexd/codex.system/bootstrap/usr/appleinternal/bin:/opt/pmk/env/global/bin:/user/local/MacGPG2/bin:/Library/TeX/texbin:/opt/homebrew/bin:/Applications/Ghostty.app/Contents/MacOS:$PYENV_ROOT:$GOROOT/bin:$GOPATH/bin:$HOME/.krew/bin:$HOME/bin-pkg"

# enable Node Version Manager
#export NVM_DIR="$HOME/.nvm"
#[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
#[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

# Set PATH variable to enable shims, no shell integration
#eval "$(pyenv init --path)"
# Install pyenv into your shell as shell function, enable shims,
# enable autocomplete
#eval "$(pyenv init -)"
eval "$(fzf --bash)"
ssh-add --apple-load-keychain
