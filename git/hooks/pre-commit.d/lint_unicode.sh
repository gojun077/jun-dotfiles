#!/usr/bin/env bash
# .git/hooks/pre-commit (for dotfiles repo)
#
# Created on: Sun 14 Sep 2025
# Last Updated: Sun 14 Sep 2025

# Get the top-level directory of the Git repository
REPO_ROOT=$(git rev-parse --show-toplevel)

echo "--- Running pre-commit hook ---"

# Define the full path to your checker script
CHECKER_SCRIPT="$REPO_ROOT/.git/scripts/check_illegal_chars.py"

STAGED_FILES=$(git diff --cached --name-only --diff-filter=AM)

if [ -z "$STAGED_FILES" ]; then
  echo "No staged files to check. Skipping."
  exit 0
fi

echo "Checking the following staged files:"
echo "$STAGED_FILES"
echo "---------------------------------"

# Run the Python checker on the staged files.
# Using xargs to correctly pass the list of files to the Python script
echo "$STAGED_FILES" | xargs "$CHECKER_SCRIPT"

# Capture the exit code of the script
EXIT_CODE=$?

if [ $EXIT_CODE -ne 0 ]; then
  echo "---------------------------------"
  echo "Pre-commit check failed. Aborting commit."
  exit 1
fi

echo "Pre-commit check passed."
exit 0
