# Gmail archive: mbsync AppArmor override

On Ubuntu 26.04.1, the packaged `/etc/apparmor.d/mbsync` profile blocks
`/usr/bin/dash` (the target of `/bin/sh` used by `PassCmd`) and the archive's
`/data/mail/gmail` Maildir. The resulting error is misleading:
`Skipping account gmail, PassCmd failed: Permission denied`.

`local/mbsync` supplements the packaged profile via its existing
`include if exists <local/mbsync>` directive. Keep the packaged profile intact.
Install a root-owned copy rather than a symlink into a user-writable checkout.
If a local override already exists, review and merge its rules instead of
overwriting it.

```sh
sudo install -o root -g root -m 0644 ~/dotfiles/apparmor/local/mbsync /etc/apparmor.d/local/mbsync
sudo apparmor_parser --skip-kernel-load --skip-cache /etc/apparmor.d/mbsync
sudo apparmor_parser --replace /etc/apparmor.d/mbsync
```

The shell inherits mbsync confinement. Only `/usr/local/bin/oama` is allowed to
execute unconfined (`Ux`, with unsafe environment variables scrubbed), so its
existing OAuth/GPG setup works without granting mbsync access to token files or
GPG keys. This deliberately trusts that exact root-owned helper and its
subprocesses; do not change the rule to permit arbitrary executable paths.
Mail permissions mirror the packaged Maildir rules under `/data/mail/gmail`,
restricted to entries owned by the invoking user. Custom mail roots or helper
locations require corresponding policy changes.

Verify authentication and mailbox listing without synchronizing or moving mail:

```sh
env PATH="$HOME/.local/bin:/usr/local/bin:/usr/bin:/bin" mbsync --list 'gmail:[Gmail]/All Mail'
sudo journalctl -k -b --no-pager | grep 'profile="mbsync"'
```

Old denials remain in the journal; check their timestamps. Do not use a full
`gmail-archive-sync.service` restart merely as an authentication test: its
`--move-gmail` option can move backed-up messages to Gmail Trash. The existing
timer will retry normally. Neither disabling AppArmor nor putting the profile
in complain mode is necessary.
