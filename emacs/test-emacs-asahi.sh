#!/usr/bin/env bash
#
# Batch harness for smoke-testing the Asahi Emacs configuration.
# Usage:
#   ./emacs/test-emacs-asahi.sh
#
# Optional environment variables:
#   EMACS_INIT_FILE        Override the init file to load (default: emacs/emacs_asahi)
#   EMACS_EARLY_INIT_FILE  Override the early-init file (default: emacs/emacs.d/asahi_early-init.el)
#   EMACS_DOTFILES_ROOT    Explicitly set the dotfiles repository root
#
# Exit codes:
#   0   success
#   1   Emacs reported an error (details printed to stderr)
#   125 emacs executable not found

set -euo pipefail

repo_root="${EMACS_DOTFILES_ROOT:-$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)}"

log_file="${TMPDIR:-/tmp}/emacs_asahi_test.$$"
trap 'rm -f "$log_file"' EXIT

printf 'Checking emacs_asahi in %s\n' "$repo_root"

if ! command -v emacs >/dev/null 2>&1; then
  echo "emacs executable not found" >&2
  exit 125
fi

elisp=$(cat <<'EOL'
(let* ((repo-root (or (getenv "EMACS_DOTFILES_ROOT")
                      (error "EMACS_DOTFILES_ROOT not set")))
       (user-emacs-directory (expand-file-name "~/.emacs.d/"))
       (early-init (or (getenv "EMACS_EARLY_INIT_FILE")
                       (expand-file-name "emacs/emacs.d/asahi_early-init.el"
                                         repo-root)))
       (init-file (or (getenv "EMACS_INIT_FILE")
                      (expand-file-name "emacs/emacs_asahi" repo-root))))
  (condition-case err
      (progn
        (when (file-readable-p early-init)
          (load early-init nil 'nomessage))
        (load init-file nil 'nomessage)
        (message "emacs_asahi loaded successfully")
        (kill-emacs 0))
    (error
     (princ (format "ERROR: %S\n" err))
     (kill-emacs 1))))
EOL
)

EMACS_DOTFILES_ROOT="$repo_root" \
emacs -Q --batch --eval "$elisp" >"$log_file" 2>&1 || {
  cat "$log_file" >&2
  exit 1
}

cat "$log_file"
