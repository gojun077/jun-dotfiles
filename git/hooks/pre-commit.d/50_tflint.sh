#!/usr/bin/env bash
# hooks/pre-commit.d/tflint.sh
#
# Created on: Mar 15 2026
#
# Lints directories containing staged Terraform/HCL files using tflint.

# 1. Find staged Terraform and HCL files, excluding deletions
staged_tf_files=$(git diff --cached --name-only --diff-filter=ACMR | grep -E '\.(tf|hcl)$')

# 2. Fast exit if no relevant files are staged
if [ -z "$staged_tf_files" ]; then
    exit 0
fi

if ! command -v tflint &> /dev/null; then
    printf "⚠️ 'tflint' not found in PATH; please install it to lint Terraform files.\n"
    exit 0
fi

# 3. Extract unique directories containing the staged files
# Using xargs -n1 dirname to handle multiple files, then sort -u to deduplicate
tf_dirs=$(echo "$staged_tf_files" | xargs -n1 dirname | sort -u)

EXIT_CODE=0
for dir in $tf_dirs; do
    printf "--> Running tflint in: %s\n" "$dir"
    
    # Run tflint targeting only the specific directory
    # --minimum-failure-severity=error ensures we only fail on actual errors
    tflint --chdir="$dir" --minimum-failure-severity=error
    status=$?
    
    if [ $status -ne 0 ]; then
        printf "❌ Error: tflint found issues in %s (Exit code: %s)\n" "$dir" "$status"
        EXIT_CODE=1
    fi
done

exit $EXIT_CODE
