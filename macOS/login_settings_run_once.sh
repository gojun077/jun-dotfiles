#!/usr/bin/env bash
#
# login_settings_run_once.sh
#
# This file serves the purpose of `.bash_profile` as it operates
# on Linux and POSIX environments. `.bash_profile` has quite
# different behavior on MacOS, however, because every shell in the
# GUI is a login shell.
#
# This bash shell file contains settings that should only be run
# once per GUI shell session.

sudo launchctl limit maxfiles 65536 200000
ulimit -n 65536 200000

# The next line updates PATH for the Google Cloud SDK.
if [ -f '/Users/peter.koh/Downloads/google-cloud-sdk/path.bash.inc' ]; then . '/Users/peter.koh/Downloads/google-cloud-sdk/path.bash.inc'; fi

# The next line enables shell command completion for gcloud.
if [ -f '/Users/peter.koh/Downloads/google-cloud-sdk/completion.bash.inc' ]; then . '/Users/peter.koh/Downloads/google-cloud-sdk/completion.bash.inc'; fi

export GOROOT=/usr/local/go
export GOPATH=/Users/peter.koh/go
export PATH="/usr/local/Cellar/sqlite/3.31.1/bin:/usr/local/opt/openjdk@11/bin:/usr/local/go/bin:$GOPATH/bin:/usr/local/opt/mysql-client/bin:$HOME/.pyenv/bin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/MacGPG2/bin:/usr/local/share/dotnet:~/.dotnet/tools:$HOME/Downloads/google-cloud-sdk/bin:/usr/local/opt/libpq/bin:/Library/TeX/texbin"

eval "$(pyenv init --path)"
ssh-add --apple-load-keychain
