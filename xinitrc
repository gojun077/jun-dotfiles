#!/bin/bash
# The original path and name of this file are: ~/.xinitrc
# The following settings will launch openbox when 'startx' is invoked
export BROWSER=chromium
exec dbus-launch openbox-session
