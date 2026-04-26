# Agent Instructions

Public dotfiles repository for Linux, macOS, and WSL configuration.
Maintained by Peter Jun Koh.

## Repository Layout

Configs are organized as flat files and directories under `~/dotfiles/`.
Deployment is via symlinks managed by `create_symlinks.sh`.

### Platform Naming Convention

Most config files follow a `_<platform>` suffix pattern:

| Suffix | Platform |
|--------|----------|
| `_argo` | Fedora workstation (Argo) |
| `_asahi` | Asahi Linux (aarch64) |
| `_fedora` / `_fedora_pi` | Fedora (generic / Raspberry Pi) |
| `_ubuntu` / `_ubuntu_oci` | Ubuntu (generic / Oracle Cloud) |
| `_mac` / `_macbookair` / `_mbp` | macOS variants |
| `_wsl` | Windows Subsystem for Linux |
| `_k3s` / `_rpi` | Kubernetes / Raspberry Pi nodes |
| `_termux` / `_android` | Android/Termux |

Unsuffixed files (e.g. `vimrc`, `screenrc`) are shared across platforms.

### Key Directories

| Directory | Contents |
|-----------|----------|
| `bash/` | Per-platform bashrc, bash_profile, bash_aliases |
| `emacs/` | Per-platform emacs configs + `emacs.d/` |
| `git/` | Global gitignore, hooks, scripts |
| `tmux/` | Per-platform tmux configs |
| `macOS/` | macOS-specific configs (aerospace, yabai, skhd, etc.) |
| `mcp/` | MCP server definitions |
| `sway/` | Sway Wayland compositor configs |
| `waybar/` | Waybar status bar configs |
| `vim/` | Vim ftplugin syntax files |
| `xdg-mime/` | XDG mimeapps and .desktop files |

## Security Boundaries

This repo is **public**. Never commit:

- SSH private keys
- GPG private keys / secret keyrings
- API tokens, passwords, or credentials
- Git identity configs (`.gitconfig` contains email/name)
- Authinfo / netrc files
- WireGuard keys
- Kubernetes configs
- AI tool credentials

The encrypted dotfiles repo (`dotfiles-enc`) uses git-remote-gcrypt with a VPS
remote. **Two repos, two scripts:**

| Repo | Path | Script | Content |
|------|------|--------|---------|
| `~/dotfiles/` (public) | `~/dotfiles/` | `create_symlinks.sh` | Shell, editor, terminal, WM configs |
| `dotfiles-enc` (encrypted) | `~/Documents/repos/encrypted/dotfiles-enc/` | `create_symlinks_enc.sh` | Git identity, SSH, GPG, auth, WireGuard, kube, AI creds |

## Symlink Deployment

### Public dotfiles: `create_symlinks.sh`

```bash
cd ~/dotfiles && bash create_symlinks.sh
```

The script uses a **hybrid approach**:
1. **Shared base** — symlinks applied on all platforms (vimrc, screenrc, shellcheckrc, vim ftplugins, XDG mime)
2. **Per-hostname blocks** — detected via `hostname -s`, each host gets its bash/emacs/tmux/WM configs
3. **Conditional app symlinks** — only created if the app is installed (irssi, cmus, quodlibet, etc.)
4. **System-level symlinks** — require `setACL_symlinks.sh` to have been run first (ansible, dnsmasq, pacman, etc.)

Existing files are backed up as `<file>.old` before symlinks are created.
Existing symlinks (even broken ones) are left in place.

Adding a new machine: add a `case` block in the per-hostname section.

### Encrypted dotfiles: `create_symlinks_enc.sh`

```bash
cd ~/Documents/repos/encrypted/dotfiles-enc && bash create_symlinks_enc.sh
```

Same hybrid pattern (shared base + per-hostname). Also sets correct
permissions on SSH keys (600) and authinfo (600).

## Git Conventions

- Prefer specific file staging over `git add -A`
- Do not commit secrets — check file contents before staging
- Platform-specific changes should note the platform in the commit message
- The `.gitignore` is in the encrypted repo (contains per-host patterns)

## Non-Interactive Shell Commands

ALWAYS use `-f` flags: `cp -f`, `mv -f`, `rm -f`

## Elisp Tooling for Agents

Modeled after Go's `go doc` (docs lookup) and `gopls` (real-time
diagnostics). Use these when reading or editing any `.el` file in the repo.

| Command | Backend | Purpose |
|---------|---------|---------|
| `make get_elisp_info SYMBOL=<name>` | `emacsclient` → live server | Print signature, source file, and docstring |
| `make search_elisp QUERY=<regexp>`  | `emacsclient` → live server | Apropos search over loaded symbols |
| `make validate_elisp FILE=<path>`   | `emacs --batch -Q`          | Byte-compile lint (warnings = errors) |

The underlying scripts live in `bin/` and can also be invoked directly
(`bin/elisp-doc`, `bin/elisp-search`, `bin/elisp-lint`).

**Why two backends:** docs/search query the running Emacs server so
they see your loaded init and third-party packages. Lint runs in an
isolated batch Emacs (`-Q`, no init) so it never redefines functions in
the live session and is safe to run in CI.

**Caveat:** `validate_elisp` runs against the file in isolation and does
not load your config. Files that `(require ...)` packages from your
init will produce free-variable warnings. That's expected for the MVP;
hardening for full-init validation is a future task.

## MCP Servers

Defined in `mcp/mcpservers.json`. Currently:
- **puppeteer**: headless browser automation (uses `/usr/bin/chromium-browser`)
