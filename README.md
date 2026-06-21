# Jun's public dotfiles

Personal configuration files for the Linux, macOS, WSL, and Termux systems I
use day to day.  The repo is intentionally practical rather than frameworky:
most files are the real configs used on specific hosts, with a shared symlink
script that installs the right set for the current machine.

Clone this repository as `~/dotfiles`:

```sh
git clone https://github.com/gojun077/jun-dotfiles.git ~/dotfiles
```

## What is in here

- **Shell configs** in [`bash/`](bash/) and [`macOS/`](macOS/) for Fedora,
  Asahi Linux, Ubuntu, WSL, Termux, and macOS.
- **Emacs configs** in [`emacs/`](emacs/), with per-platform `.emacs`
  entrypoints plus shared `~/.emacs.d/` modules for Org, GPTel, UI,
  Projectile, and YASnippet.
- **Terminal and editor configs** for Vim, tmux, Ghostty, WezTerm, Alacritty,
  GNU Screen, ShellCheck, and Starship.
- **Desktop and Wayland configs** for Sway, Waybar, AeroSpace, yabai/skhd-era
  macOS workflows, XDG MIME handlers, and screenshot tooling.
- **System/service configs** for tools such as Ansible, dnsmasq, pacman,
  bitlbee, vsftpd, systemd, NetworkManager/iwd, and related host utilities.

This is a **public** dotfiles repo.  Private material such as SSH keys,
GPG keys, authinfo, Git identity, kubeconfigs, WireGuard keys, and AI/API
credentials belongs in the separate encrypted dotfiles repo, not here.

## Installing symlinks

Run the cross-platform symlink script from the repo root:

```sh
cd ~/dotfiles
bash create_symlinks.sh
```

Do **not** run it as root.  The script expects `$HOME` to be your user home and
will stop if `$USER` is `root`.

The script uses a hybrid deployment model:

1. **Shared base symlinks** are installed everywhere, such as `.vimrc`,
   `.screenrc`, `.shellcheckrc`, Vim ftplugins, and XDG MIME desktop files.
2. **Per-hostname blocks** choose host-specific bash, Emacs, tmux, window
   manager, terminal, and prompt configs.  Current examples include Asahi Linux
   on a MacBook Air, Fedora workstations, Ubuntu OCI, Raspberry Pi/Fedora,
   k3s, WSL, Termux, and macOS.
3. **Conditional app symlinks** are only installed when the app exists on the
   machine, for example irssi, cmus, Quod Libet, Terminator, MAME, Midnight
   Commander, and Lynx.
4. **System-level symlinks** are attempted for supported services when the
   target directories/tools exist.  These may require permissions prepared by
   `setACL_symlinks.sh`.

Existing files are moved aside as `.old` backups before new symlinks are
created.  If a `.old` backup already exists, the script adds a timestamped
suffix.

To add a new machine, add a `case` block for its `hostname -s` value in
[`create_symlinks.sh`](create_symlinks.sh), then point each desired dotfile at
the platform-specific file in this repo.

## Emacs layout

The Emacs setup has grown into a small multi-platform configuration:

- `emacs/emacs_asahi` and `emacs/emacs_mac` are the active, modern entrypoints
  for Emacs 30-era systems using `straight.el` and `use-package`.
- `emacs/emacs_fedora`, `emacs/emacs_ubuntu_oci`, `emacs/emacs_wsl`, and
  `emacs/emacs_win11_native` preserve host-specific setups for other systems.
- `emacs/emacs.d/init-org.el` and `init-org-macos.el` hold Org/GTD capture,
  agenda, refile, and export behavior.
- `emacs/emacs.d/init-gptel.el` configures GPTel backends while keeping actual
  credentials outside this public repo.
- `emacs/emacs.d/init-ui*.el`, `init-projectile.el`, and
  `init-yasnippet.el` keep reusable UI, project, and snippet behavior separate
  from the top-level `.emacs` files.
- `emacs/emacs.d/snippets/` is symlinked into `~/.emacs.d/snippets` on systems
  that use the shared YASnippet module.

When editing Emacs Lisp in this repo, the Makefile provides helper targets for
documentation lookup, apropos-style search, and linting:

```sh
make get_elisp_info SYMBOL=use-package
make search_elisp QUERY=gptel
make validate_elisp FILE=emacs/emacs.d/init-yasnippet.el
```

## Platform naming convention

Many files use a suffix to describe the host or platform they belong to:

| Suffix | Platform |
| --- | --- |
| `_asahi` | Asahi Linux / Fedora Remix on Apple Silicon |
| `_fedora`, `_fedora_pi` | Fedora systems |
| `_ubuntu`, `_ubuntu_oci` | Ubuntu systems, including Oracle Cloud |
| `_mac`, `_macbookair`, `_mbp` | macOS variants |
| `_wsl` | Windows Subsystem for Linux |
| `_k3s`, `_rpi` | Kubernetes / Raspberry Pi nodes |
| `_termux`, `_android` | Android / Termux |

Unsuffixed files are shared defaults unless the symlink script overrides them
for a specific platform.

## Related private repo

This public repository is paired with an encrypted dotfiles repository managed
separately by `create_symlinks_enc.sh`.  Keep secrets and identity-bearing
configuration there, not in this repo.
