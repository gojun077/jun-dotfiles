#!/usr/bin/env bash
# hooks/pre-commit.d/lint_posix_shell.sh
#
# Created on: Nov 24 2022
# Last Updated: Apr 2 2026
#
# Lints staged .sh and .bash files using shellcheck.

# 1. Find staged shell scripts, excluding deletions
gitfiles=$(git diff --cached --name-only --diff-filter=ACMR | grep -E '\.(sh|bash)$')

# 2. Fast exit if no relevant files are staged
if [ -z "$gitfiles" ]; then
    exit 0
fi

if ! command -v shellcheck &> /dev/null; then
    printf "⚠️ 'shellcheck' not found in PATH; please install shellcheck to lint shell scripts.\n"
    exit 0
fi

EXIT_CODE=0
for f in $gitfiles; do
    if [ -f "$f" ]; then
        printf "Linting shell script with shellcheck: %s\n" "$f"
        if ! shellcheck "$f"; then
            EXIT_CODE=1
        fi
    fi
done

exit $EXIT_CODE
