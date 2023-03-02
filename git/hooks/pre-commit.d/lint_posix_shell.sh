#!/usr/bin/env bash
# lint_posix_shell.sh
#
# Created on: Nov 24 2022
# Created by: gojun077@gmail.com
# Last Updated: Nov 24 2022
#
# This bash script is intended to be executed locally in a git pre-commit
# hook to check bash shell syntax for `.sh` and `.bash` files. This script
# requires that `shellcheck` linter be installed locally

# bash array to store list of shell scripts to be committed
gitfiles=$(git diff-index --cached --name-only HEAD | grep -E '\.(sh)|\.(bash)')

function test_file() {
  myfile="${1}"

  if [ ! -f "${myfile}" ]; then
    return
  fi

  printf "%s\\n" "Linting shell script with *shellcheck*..."
  if test shellcheck; then
    shellcheck "${myfile}"
  else
    printf "%s\\n" "'shellcheck' not found in PATH; please install shellcheck"
  fi
}

case "${1}" in
  --about )
      printf "%s\\n" "bash/sh syntax lint"
      ;;
  * )
    for f in $gitfiles; do
      test_file "$f"
    done
    ;;
esac
