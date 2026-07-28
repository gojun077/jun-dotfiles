#!/bin/bash
# remove_tailscale_app.sh
#
# Removes /Applications/Tailscale.app when it appears. A corporate MDM
# reinstalls the GUI app roughly every 30 minutes; this script keeps it
# gone so the open-source tailscale CLI (e.g. `brew install tailscale`)
# remains the active client.
#
# Meant to be driven by the launchd daemon
#   com.peterjunkoh.remove-tailscale-app
# which runs this script as root every 10 minutes (see the matching plist).
#
# Install (one-time, run with sudo):
#   cp ~/dotfiles/macOS/com.peterjunkoh.remove-tailscale-app.plist /Library/LaunchDaemons/
#   chown root:wheel /Library/LaunchDaemons/com.peterjunkoh.remove-tailscale-app.plist
#   chmod +x ~/dotfiles/macOS/remove_tailscale_app.sh
#   launchctl load -w /Library/LaunchDaemons/com.peterjunkoh.remove-tailscale-app.plist
#
# Uninstall:
#   launchctl unload -w /Library/LaunchDaemons/com.peterjunkoh.remove-tailscale-app.plist
#   rm -f /Library/LaunchDaemons/com.peterjunkoh.remove-tailscale-app.plist

set -u

APP="/Applications/Tailscale.app"
LOG="/var/log/remove-tailscale-app.log"
TAG="remove-tailscale-app"

log() {
  # Append a timestamped line to the log file and to the unified log
  # (inspect with: log show --predicate 'eventMessage CONTAINS "remove-tailscale-app"').
  local msg
  msg="[$(date '+%Y-%m-%dT%H:%M:%S')] $*"
  printf '%s\n' "$msg" >> "$LOG"
  /usr/bin/logger -t "$TAG" -- "$*"
}

if [[ -d "$APP" ]]; then
  # Sanity check: only act on something that looks like an app bundle.
  if [[ -f "$APP/Contents/Info.plist" ]]; then
    log "Found $APP; removing."
    /bin/rm -rf "$APP"
    if [[ ! -e "$APP" ]]; then
      log "Removed $APP successfully."
    else
      log "ERROR: failed to remove $APP."
      exit 1
    fi
  else
    log "WARN: $APP exists but is not an app bundle; leaving it untouched."
  fi
fi

# Silent when Tailscale.app is absent to avoid log spam every 10 minutes.
exit 0
