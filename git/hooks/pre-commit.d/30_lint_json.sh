#!/usr/bin/env bash
# hooks/pre-commit.d/lint_json.sh
#
# Created on: Nov 24 2022
# Last Updated: Apr 2 2026
#
# Lints staged .json files using jq.

# 1. Find staged JSON files, excluding deletions
gitfiles=$(git diff --cached --name-only --diff-filter=ACMR | grep -E '\.json$')

# 2. Fast exit if no relevant files are staged
if [ -z "$gitfiles" ]; then
    exit 0
fi

if ! command -v jq &> /dev/null; then
    printf "⚠️ 'jq' not found in PATH; please install jq to lint JSON files.\n"
    exit 0
fi

EXIT_CODE=0
for f in $gitfiles; do
    if [ -f "$f" ]; then
        printf "Linting JSON with jq: %s\n" "$f"
        if ! jq . "$f" > /dev/null; then
            EXIT_CODE=1
        fi
    fi
done

exit $EXIT_CODE
