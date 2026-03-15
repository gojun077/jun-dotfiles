#!/usr/bin/env bash
# hooks/pre-commit.d/lint_dockerfile.sh
#
# Created on: Nov 24 2022
# Last Updated: Mar 15 2026
#
# Lints staged Dockerfiles using hadolint.

# 1. Find staged Dockerfiles, excluding deletions
# --diff-filter=ACMR ensures we don't try to lint files that were deleted.
gitfiles=$(git diff --cached --name-only --diff-filter=ACMR | grep -iE 'Dockerfile' | grep -v '\.sh$')

# 2. Fast exit if no relevant files are staged
if [ -z "$gitfiles" ]; then
    exit 0
fi

if ! command -v hadolint &> /dev/null; then
    printf "⚠️ 'hadolint' not found in PATH; please install it to lint Dockerfiles.\n"
    # We exit 0 to avoid blocking the commit if the tool is missing, 
    # but you could change this to 1 if it's a hard requirement.
    exit 0
fi

EXIT_CODE=0
for f in $gitfiles; do
    # Double check file existence (redundant with ACMR but safe)
    if [ -f "$f" ]; then
        printf "Linting Dockerfile with hadolint: %s\n" "$f"
        if ! hadolint "$f"; then
            EXIT_CODE=1
        fi
    fi
done

exit $EXIT_CODE
