#!/usr/bin/env bash
#
# tflint.sh
#
# Created on: Thu 12 Mar 2026
# Last Updated: Thu 12 Mar 2026

# Define the target directory relative to the repo root
TARGET_BASE="terraform/projects/gcp/my-proj"

echo "Running tflint checks (Errors only)..."

# Loop through subfolders 1 level deep
for dir in "$TARGET_BASE"/*/; do
    # Ensure it is a directory
    if [ -d "$dir" ]; then
        echo "--> Checking: $dir"

        # --minimum-failure-severity=error ensures exit code 2 is ONLY thrown for errors.
        # Warnings will still be printed but will return exit code 0.
        tflint --chdir="$dir" --minimum-failure-severity=error --recursive

        status=$?

        # If tflint exits with anything other than 0 (usually 2 for lint errors), block the commit.
        if [ $status -ne 0 ]; then
            echo "❌ Error: tflint found issues in $dir (Exit code: $status)"
            exit 1
        fi
    fi
done

echo "✅ TFLint passed (or only found warnings). Committing..."
exit 0
