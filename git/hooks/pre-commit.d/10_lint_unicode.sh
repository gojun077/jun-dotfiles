#!/usr/bin/env bash
# hooks/pre-commit.d/lint_unicode.sh
#
# Created on: Sun 14 Sep 2025
# Last Updated: Apr 2 2026
#
# Checks staged files for illegal/invisible Unicode characters.
# Skips document formats where Unicode chars (em-dash, smart quotes, etc.)
# are expected and valid.

REPO_ROOT=$(git rev-parse --show-toplevel)
CHECKER_SCRIPT="$REPO_ROOT/.git/scripts/check_illegal_chars.py"

# File extensions to skip (documents where Unicode is intentional)
SKIP_EXTENSIONS='\.md$\.org$\.rst$\.txt$\.adoc$\.tex$\.lyx$\.html$\.xml$\.svg$'

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

# 3. Filter out document file types
CODE_FILES=""
for f in $STAGED_FILES; do
    skip=false
    for ext_pat in $SKIP_EXTENSIONS; do
        if echo "$f" | grep -qE "$ext_pat"; then
            skip=true
            break
        fi
    done
    if [ "$skip" = false ]; then
        CODE_FILES="$CODE_FILES $f"
    fi
done

# 4. Fast exit if no code files remain after filtering
if [ -z "$CODE_FILES" ]; then
    printf "No code files to check (only documents staged); skipping Unicode check.\n"
    exit 0
fi

printf "Checking staged files for illegal Unicode characters...\n"
echo "$CODE_FILES" | xargs "$CHECKER_SCRIPT"
exit $?
