# Tailscale.app Removal Daemon

A macOS system LaunchDaemon that automatically removes `/Applications/Tailscale.app` whenever it reappears. A corporate MDM reinstalls the GUI app roughly every 30 minutes; this daemon keeps it gone so the open-source `tailscale` CLI (e.g.  `brew install tailscale`) remains the active client.

## Files

| File                                         | Role                                                                 |
|----------------------------------------------|----------------------------------------------------------------------|
| `remove_tailscale_app.sh`                    | Removal script — checks for the app bundle and deletes it if present |
| `com.peterjunkoh.remove-tailscale-app.plist` | System LaunchDaemon definition (runs as root every 10 min)           |

## Why a LaunchDaemon (not a LaunchAgent)?

`Tailscale.app` is MDM-installed and owned by `root`. A user-level `LaunchAgent` cannot delete a root-owned bundle without an interactive password prompt. A system LaunchDaemon runs as root and can remove it cleanly and unattended.

## How it works

- The daemon runs at load and every **600 seconds** (10 minutes).
- The script checks whether `/Applications/Tailscale.app` exists and is a real
  app bundle (`Contents/Info.plist` present).
- If found, it `rm -rf`s the bundle and logs the action.
- If absent, it exits silently to avoid log spam on idle runs.
- Logs are written to `/var/log/remove-tailscale-app.log` and the macOS unified
  log (queryable via `log show --predicate 'eventMessage CONTAINS "remove-tailscale-app"'`).

## Install (one-time, run with sudo)

```bash
sudo cp ~/dotfiles/macOS/com.peterjunkoh.remove-tailscale-app.plist /Library/LaunchDaemons/
sudo chown root:wheel /Library/LaunchDaemons/com.peterjunkoh.remove-tailscale-app.plist
chmod +x ~/dotfiles/macOS/remove_tailscale_app.sh
sudo launchctl load -w /Library/LaunchDaemons/com.peterjunkoh.remove-tailscale-app.plist
```

## Uninstall

```bash
sudo launchctl unload -w /Library/LaunchDaemons/com.peterjunkoh.remove-tailscale-app.plist
sudo rm -f /Library/LaunchDaemons/com.peterjunkoh.remove-tailscale-app.plist
```

## Verifying the daemon

### 1. Confirm the daemon is loaded

```bash
sudo launchctl list | grep remove-tailscale-app
```

You'll see a line like:

```
-  0  com.peterjunkoh.remove-tailscale-app
```

The three columns are PID, last exit status, and label. A PID of `-` means
it's loaded but not currently running (expected between intervals). An exit
status of `0` means the last run succeeded. If you see no output, the daemon
isn't loaded.

For more detail (full plist as loaded, run state, last exit code):

```bash
sudo launchctl print system/com.peterjunkoh.remove-tailscale-app
```

### 2. Check the log

```bash
tail -20 /var/log/remove-tailscale-app.log
```

Each removal logs a timestamped line like:

```
[2026-07-28T12:40:01] Found /Applications/Tailscale.app; removing.
[2026-07-28T12:40:01] Removed /Applications/Tailscale.app successfully.
```

Or via the unified log (last hour):

```bash
log show --predicate 'eventMessage CONTAINS "remove-tailscale-app"' --last 1h
```

### 3. End-to-end test (proves it actually removes the app)

```bash
# Force a manual run right now
sudo /bin/bash ~/dotfiles/macOS/remove_tailscale_app.sh

# Or trigger it via launchctl without waiting for the interval
sudo launchctl kickstart -k system/com.peterjunkoh.remove-tailscale-app
```

Then verify the app is gone and the log shows the removal:

```bash
ls -d /Applications/Tailscale.app 2>/dev/null && echo "STILL PRESENT" || echo "GONE"
tail -5 /var/log/remove-tailscale-app.log
```

The `kickstart -k` flag kills any running instance and starts a fresh one
immediately, so you don't have to wait for the 10-minute interval. The
quickest day-to-day check is step 1 (`launchctl list | grep`) combined with
step 2 (`tail` the log).

## Notes

- The plist invokes the script directly from `~/dotfiles/macOS/`, so edits to the script take effect without re-copying anywhere — single source of truth.
- Ensure `remove_tailscale_app.sh` stays readable and executable by root (`0755`).  If you relocate the dotfiles repo, update the script path in the plist.
- This does not conflict with `create_symlinks.sh`, whose Tailscale symlink is guarded by `[[ -x "$TAILSCALE_APP_BIN" ]]` — once the app is gone, that conditional simply won't fire.
