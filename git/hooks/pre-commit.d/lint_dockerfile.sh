#!/usr/bin/env bash
# lint_dockerfile.sh
#
# Created on: Nov 24 2022
# Created by: gojun077@gmail.com
# Last Updated: Nov 24 2022
#
# This bash script is intended to be executed locally in a git pre-commit
# hook to check Dockerfile syntax. This script
# requires that `hadolint` linter be installed locally

# bash array to store list of shell scripts to be committed
gitfiles=$(git diff-index --cached --name-only HEAD | grep -E '(Dockerfile)')

function test_file() {
  myfile="${1}"

  if [ ! -f "${myfile}" ]; then
    return
  fi

  printf "%s\\n" "Linting Dockerfile with *hadolint*..."
  if test hadolint; then
    hadolint "${myfile}"
  else
    printf "%s\\n" "'hadolint' not found in PATH; please install hadolint"
  fi
}

case "${1}" in
  --about )
      printf "%s\\n" "Dockerfile syntax lint"
      ;;
  * )
    for f in $gitfiles; do
      test_file "$f"
    done
    ;;
esac
