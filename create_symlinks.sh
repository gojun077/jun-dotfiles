#!/bin/bash
# create symlinks for public dotfiles from ~/dotfiles
# Do NOT run as root, as this will cause the $HOME variable to default
# to '/root' instead of ~/username and cause $USER to become 'root'
# instead of local user.
#
# Encrypted dotfiles (SSH keys, gitconfig, authinfo, etc.) are managed
# separately by create_symlinks_enc.sh in the dotfiles-enc repo.
#
# Last updated: Sun 19 Apr 2026

set -euo pipefail

DOTFILES="$HOME/dotfiles"

# =========================================================================
# Utility functions
# =========================================================================

create_sym()
{
  # Create a symlink from $1 (link path) -> $2 (target file)
  # - If $1 exists as a non-symlink file, back it up as $1.old
  # - If $1 is already a symlink (even broken), skip
  # - If parent dir of $1 doesn't exist, create it
  #
  # USAGE: create_sym <link_path> <target_path>

  local LINK="$1"
  local TARGET="$2"

  if [[ -L "$LINK" ]]; then
    printf "[skip] %s is already a symlink\n" "$LINK"
    return 0
  fi

  if [[ -f "$LINK" ]]; then
    mv -f "$LINK" "${LINK}.old"
    printf "[backup] %s -> %s.old\n" "$LINK" "$LINK"
  fi

  local LINKDIR
  LINKDIR=$(dirname "$LINK")
  if [[ ! -d "$LINKDIR" ]]; then
    mkdir -p "$LINKDIR"
  fi

  ln -s "$TARGET" "$LINK"
  printf "[link]  %s -> %s\n" "$LINK" "$TARGET"
}


remove_broken_sym()
{
  # Remove broken symlinks at the given path
  local LINK="$1"
  if [[ -L "$LINK" && ! -e "$LINK" ]]; then
    rm -f "$LINK"
    printf "[rm]    %s (broken symlink)\n" "$LINK"
  fi
}


# =========================================================================
# Pre-flight checks
# =========================================================================

if [[ "$USER" = "root" ]]; then
  printf "This script must not be executed as root.\n" >&2
  exit 1
fi

if [[ ! -d "$DOTFILES" ]]; then
  printf "$HOME/dotfiles directory not found at %s\n" "$DOTFILES" >&2
  exit 1
fi

HOSTNAME=$(hostname -s 2>/dev/null || hostname)
printf "=== Creating symlinks for public dotfiles ===\n"
printf "Hostname: %s\n" "$HOSTNAME"
printf "Dotfiles:  %s\n\n" "$DOTFILES"


# =========================================================================
# Shared base symlinks (all platforms)
# =========================================================================

printf "\n--- Shared base symlinks ---\n"

# Home directory dotfiles
create_sym "$HOME/.vimrc"           "$DOTFILES/vimrc"
create_sym "$HOME/.screenrc"        "$DOTFILES/screenrc"
create_sym "$HOME/.shellcheckrc"    "$DOTFILES/shellcheckrc"

# Vim syntax ftplugins
mkdir -p "$HOME/.vim/after/ftplugin"
create_sym "$HOME/.vim/after/ftplugin/yaml.vim" "$DOTFILES/vim/yaml.vim"
create_sym "$HOME/.vim/after/ftplugin/sh.vim"   "$DOTFILES/vim/sh.vim"

# XDG mime/applications
create_sym "$HOME/.config/mimeapps.list" \
           "$DOTFILES/xdg-mime/mimeapps.list"
mkdir -p "$HOME/.local/share/applications"
create_sym "$HOME/.local/share/applications/org-protocol.desktop" \
           "$DOTFILES/xdg-mime/org-protocol.desktop"
create_sym "$HOME/.local/share/applications/emacsclient.desktop" \
           "$DOTFILES/xdg-mime/emacsclient.desktop"

# Vim tmp dir
if [[ ! -d "$HOME/tmp" ]]; then
  mkdir "$HOME/tmp"
fi


# =========================================================================
# Per-hostname symlinks
# =========================================================================

printf "\n--- Per-hostname symlinks (%s) ---\n" "$HOSTNAME"

case "$HOSTNAME" in

  # -----------------------------------------------------------------------
  # Asahi Linux on MacBook Air M2
  # -----------------------------------------------------------------------
  jundora-macbookair)
    # Bash
    create_sym "$HOME/.bashrc"         "$DOTFILES/bash/bashrc_asahi"
    create_sym "$HOME/.bash_profile"   "$DOTFILES/bash/bash_profile_asahi_macbook_air_m2"
    create_sym "$HOME/.bash_aliases"   "$DOTFILES/bash/bash_aliases_asahi"
    create_sym "$HOME/.bash_aliases_macbookair" "$DOTFILES/bash/bash_aliases_macbookair"

    # Emacs
    create_sym "$HOME/.emacs"          "$DOTFILES/emacs/emacs_asahi"
    mkdir -p "$HOME/.emacs.d"
    create_sym "$HOME/.emacs.d/early-init.el" "$DOTFILES/emacs/emacs.d/asahi_early-init.el"
    create_sym "$HOME/.emacs.d/init-gptel.el" "$DOTFILES/emacs/emacs.d/init-gptel.el"
    create_sym "$HOME/.emacs.d/init-org.el"   "$DOTFILES/emacs/emacs.d/init-org.el"

    # Tmux
    create_sym "$HOME/.tmux.conf"      "$DOTFILES/tmux/tmux-argo.conf"

    # Sway (Wayland)
    mkdir -p "$HOME/.config/sway"
    create_sym "$HOME/.config/sway/config" \
               "$DOTFILES/sway/sway_config_asahi_mba-m2"

    # Waybar
    mkdir -p "$HOME/.config/waybar"
    create_sym "$HOME/.config/waybar/config"    "$DOTFILES/waybar/waybar_config_asahi"
    create_sym "$HOME/.config/waybar/style.css"  "$DOTFILES/waybar/style_asahi.css"

    # Terminals
    mkdir -p "$HOME/.config/alacritty"
    create_sym "$HOME/.config/alacritty/alacritty.toml" "$DOTFILES/alacritty.toml"
    mkdir -p "$HOME/.config/ghostty"
    create_sym "$HOME/.config/ghostty/config"   "$DOTFILES/ghostty.config.linux"

    # Starship prompt
    create_sym "$HOME/.config/starship.toml"     "$DOTFILES/starship_macbookair.toml"

    # Fastfetch
    mkdir -p "$HOME/.config/fastfetch"
    create_sym "$HOME/.config/fastfetch/config.jsonc" "$DOTFILES/fastfetch-config.jsonc"

    # Swappy (screenshot editor)
    mkdir -p "$HOME/.config/swappy"
    create_sym "$HOME/.config/swappy/config"    "$DOTFILES/swappy_config"

    # bin-pkg config
    mkdir -p "$HOME/.config/bin"
    create_sym "$HOME/.config/bin/config.json"  "$DOTFILES/bin-pkg/mbair-asahi-config.json"
    ;;

  # -----------------------------------------------------------------------
  # Argo (Fedora x86_64 workstation)
  # -----------------------------------------------------------------------
  argo)
    # Bash
    create_sym "$HOME/.bashrc"         "$DOTFILES/bash/bashrc_fedora_pi"
    create_sym "$HOME/.bash_profile"   "$DOTFILES/bash/bash_profile_fedora"
    create_sym "$HOME/.bash_aliases"   "$DOTFILES/bash/bash_aliases_fedora_pi"

    # Emacs
    create_sym "$HOME/.emacs"          "$DOTFILES/emacs/emacs_fedora"

    # Tmux
    create_sym "$HOME/.tmux.conf"      "$DOTFILES/tmux/tmux-argo.conf"
    ;;

  # -----------------------------------------------------------------------
  # Ubuntu OCI (Oracle Cloud Infrastructure)
  # -----------------------------------------------------------------------
  ubuntu-oci|ubuntu_oci)
    # Bash
    create_sym "$HOME/.bashrc"         "$DOTFILES/bash/bashrc_ubuntu_oci"
    create_sym "$HOME/.bash_profile"   "$DOTFILES/bash/bash_profile_ubuntu_oci"
    create_sym "$HOME/.bash_aliases"   "$DOTFILES/bash/bash_aliases_ubuntu_oci"

    # Emacs
    create_sym "$HOME/.emacs"          "$DOTFILES/emacs/emacs_ubuntu_oci"
    ;;

  # -----------------------------------------------------------------------
  # Raspberry Pi (Fedora aarch64)
  # -----------------------------------------------------------------------
  fedora-pi|rpi3b)
    # Bash
    create_sym "$HOME/.bashrc"         "$DOTFILES/bash/bashrc_fedora_pi"
    create_sym "$HOME/.bash_profile"   "$DOTFILES/bash/bash_profile_fedora_pi"
    create_sym "$HOME/.bash_aliases"   "$DOTFILES/bash/bash_aliases_fedora_pi"

    # Emacs
    create_sym "$HOME/.emacs"          "$DOTFILES/emacs/emacs_fedora"

    # Tmux
    create_sym "$HOME/.tmux.conf"      "$DOTFILES/tmux/tmux-rpi.conf"
    ;;

  # -----------------------------------------------------------------------
  # k3s node
  # -----------------------------------------------------------------------
  k3s)
    # Bash
    create_sym "$HOME/.bashrc"         "$DOTFILES/bash/bashrc_k3s"
    create_sym "$HOME/.bash_aliases"   "$DOTFILES/bash/bash_aliases_k3s"
    ;;

  # -----------------------------------------------------------------------
  # WSL (Windows Subsystem for Linux)
  # -----------------------------------------------------------------------
  wsl*)
    # Bash
    create_sym "$HOME/.bashrc"         "$DOTFILES/bash/bashrc_ubuntu_wsl"
    create_sym "$HOME/.bash_aliases"   "$DOTFILES/bash/bash_aliases_wsl"

    # Emacs
    create_sym "$HOME/.emacs"          "$DOTFILES/emacs/emacs_wsl"
    ;;

  # -----------------------------------------------------------------------
  # Termux (Android)
  # -----------------------------------------------------------------------
  localhost|termux)
    create_sym "$HOME/.bashrc"         "$DOTFILES/bash/bashrc_termux_poco_x3"

    # Tmux
    create_sym "$HOME/.tmux.conf"      "$DOTFILES/tmux/tmux-termux.conf"
    ;;

  # -----------------------------------------------------------------------
  # macOS (detected by uname, not hostname)
  # -----------------------------------------------------------------------
  *)
    if [[ "$(uname -s)" = "Darwin" ]]; then
      # Bash
      create_sym "$HOME/.bashrc"       "$DOTFILES/macOS/bashrc_mac"
      create_sym "$HOME/.bash_profile" "$DOTFILES/macOS/bash_profile_mac"
      create_sym "$HOME/.bash_aliases" "$DOTFILES/macOS/bash_aliases_mac"

      # Emacs
      create_sym "$HOME/.emacs"        "$DOTFILES/emacs/emacs_mac"

      # Tmux
      create_sym "$HOME/.tmux.conf"    "$DOTFILES/tmux/tmux-macos.conf"

      # Starship prompt
      create_sym "$HOME/.config/starship.toml" "$DOTFILES/starship_macbookpro.toml"
    else
      printf "[warn] Unknown hostname '%s':  no per-host symlinks configured.\n" "$HOSTNAME"
      printf "       Add a case block for this host in create_symlinks.sh\n"
    fi
    ;;
esac


# =========================================================================
# Conditional app symlinks (only if the app is installed)
# =========================================================================

printf "\n--- Conditional app symlinks ---\n"

# Irssi
if command -v irssi &>/dev/null; then
  mkdir -p "$HOME/.irssi"
  if [[ -f "$DOTFILES/irssi-config" ]]; then
    create_sym "$HOME/.irssi/config" "$DOTFILES/irssi-config"
  elif [[ -f /etc/redhat-release && -f "$DOTFILES/irssi-config.fedora" ]]; then
    create_sym "$HOME/.irssi/config" "$DOTFILES/irssi-config.fedora"
  elif [[ -f "$DOTFILES/irssi-config-ubuntu" ]]; then
    create_sym "$HOME/.irssi/config" "$DOTFILES/irssi-config-ubuntu"
  fi
fi

# CMUS
if command -v cmus &>/dev/null && [[ -f "$DOTFILES/cmus_libpl" ]]; then
  mkdir -p "$HOME/.config/cmus"
  create_sym "$HOME/.config/cmus/lib.pl" "$DOTFILES/cmus_libpl"
fi

# Quod Libet
if command -v quodlibet &>/dev/null && [[ -f "$DOTFILES/quod_stations" ]]; then
  create_sym "$HOME/.quodlibet/stations" "$DOTFILES/quod_stations"
fi

# Terminator
if command -v terminator &>/dev/null && [[ -f "$DOTFILES/terminator" ]]; then
  mkdir -p "$HOME/.config/terminator"
  create_sym "$HOME/.config/terminator/config" "$DOTFILES/terminator"
fi

# lxterminal
if command -v lxterminal &>/dev/null && [[ -f "$DOTFILES/lxterminal" ]]; then
  mkdir -p "$HOME/.config/lxterminal"
  create_sym "$HOME/.config/lxterminal/lxterminal.conf" "$DOTFILES/lxterminal"
fi

# MAME
if command -v sdlmame &>/dev/null && [[ -f "$DOTFILES/mame.ini" ]]; then
  mkdir -p "$HOME/.mame/ini"
  create_sym "$HOME/.mame/ini/mame.ini" "$DOTFILES/mame.ini"
fi

# Midnight Commander
if command -v mc &>/dev/null && [[ -f "$DOTFILES/mc.ini" ]]; then
  mkdir -p "$HOME/.config/mc"
  create_sym "$HOME/.config/mc/ini" "$DOTFILES/mc.ini"
fi

# Lynx
if command -v lynx &>/dev/null && [[ -f "$DOTFILES/.lynxrc" ]]; then
  create_sym "$HOME/.lynxrc" "$DOTFILES/.lynxrc"
fi


# =========================================================================
# System-level symlinks (require elevated permissions)
# =========================================================================

printf "\n--- System-level symlinks (may require sudo) ---\n"
printf "NOTE: System-level symlinks require 'setACL_symlinks.sh' to have been\n"
printf "      run first. Skipping if not configured.\n"

# Ansible
if command -v ansible &>/dev/null; then
  if [[ -d /etc/ansible ]]; then
    if [[ -f "$DOTFILES/ansible/ansible.cfg" ]]; then
      create_sym "/etc/ansible/ansible.cfg" "$DOTFILES/ansible/ansible.cfg"
    fi
    if [[ -f "$DOTFILES/ansible/hosts" ]]; then
      create_sym "/etc/ansible/hosts" "$DOTFILES/ansible/hosts"
    fi
    if [[ -f "$DOTFILES/ansible/group_vars/ubuntu_base" ]]; then
      mkdir -p /etc/ansible/group_vars/ 2>/dev/null || true
      create_sym "/etc/ansible/group_vars/ubuntu_base" \
                 "$DOTFILES/ansible/group_vars/ubuntu_base"
    fi
  fi
fi

# dnsmasq
if command -v dnsmasq &>/dev/null && [[ -f "$DOTFILES/dnsmasq" ]]; then
  create_sym "/etc/dnsmasq.conf" "$DOTFILES/dnsmasq"
fi

# pacman (Arch Linux)
if command -v pacman &>/dev/null; then
  if [[ -f "$DOTFILES/pacman.conf" ]]; then
    create_sym "/etc/pacman.conf" "$DOTFILES/pacman.conf"
  fi
  if [[ -f "$DOTFILES/pacman_mirrorlist" ]]; then
    create_sym "/etc/pacman.d/mirrorlist" "$DOTFILES/pacman_mirrorlist"
  fi
fi

# bitlbee
if command -v bitlbee &>/dev/null; then
  if [[ -f /etc/redhat-release && -f "$DOTFILES/bitlbee_fedora" ]]; then
    create_sym "/etc/bitlbee/bitlbee.conf" "$DOTFILES/bitlbee_fedora"
  elif [[ -f "$DOTFILES/bitlbee" ]]; then
    create_sym "/etc/bitlbee/bitlbee.conf" "$DOTFILES/bitlbee"
  fi
fi

# vsftpd
if command -v vsftpd &>/dev/null && [[ -f "$DOTFILES/vsftpd.conf" ]]; then
  create_sym "/etc/vsftpd.conf" "$DOTFILES/vsftpd.conf"
fi


# =========================================================================
# Summary
# =========================================================================

printf "\n=== Done ===\n"
printf "Public dotfiles symlinked from %s\n" "$DOTFILES"
printf "Encrypted dotfiles are managed separately by create_symlinks_enc.sh\n"
printf "  in ~/Documents/repos/encrypted/dotfiles-enc/\n"
