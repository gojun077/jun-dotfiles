#!/usr/bin/env bash
# hooks/pre-commit.d/00_gitleaks.sh
#
# Created on: Apr 2 2026
#
# Runs before other pre-commit.d scripts (00_ sorts first).
# Scans staged changes only; requires gitleaks v8+.

if ! command -v gitleaks &>/dev/null; then
    printf "⚠️ 'gitleaks' not found in PATH; skipping secret scan.\n"
    exit 0
fi

# No staged changes: nothing to scan (--quiet exits 0 only when diff is empty)
if git diff --cached --quiet 2>/dev/null; then
    exit 0
fi

printf "--> Running gitleaks (staged changes)...\n"
# --pre-commit uses git diff; --staged limits to the index
gitleaks git --pre-commit --staged --redact --verbose
exit $?
