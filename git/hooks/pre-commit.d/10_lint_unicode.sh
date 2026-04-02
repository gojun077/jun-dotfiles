#!/usr/bin/env bash
# hooks/pre-commit.d/lint_unicode.sh
#
# Created on: Sun 14 Sep 2025
# Last Updated: Apr 2 2026
#
# Checks staged files for illegal/invisible Unicode characters.

REPO_ROOT=$(git rev-parse --show-toplevel)
CHECKER_SCRIPT="$REPO_ROOT/.git/scripts/check_illegal_chars.py"

# 1. Find staged files, excluding deletions
STAGED_FILES=$(git diff --cached --name-only --diff-filter=ACMR)

# 2. Fast exit if no relevant files are staged
if [ -z "$STAGED_FILES" ]; then
    exit 0
fi

if [ ! -x "$CHECKER_SCRIPT" ]; then
    printf "⚠️ '%s' not found or not executable; skipping Unicode check.\n" "$CHECKER_SCRIPT"
    exit 0
fi

printf "Checking staged files for illegal Unicode characters...\n"
echo "$STAGED_FILES" | xargs "$CHECKER_SCRIPT"
exit $?
