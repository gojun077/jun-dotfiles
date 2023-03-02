#!/usr/bin/env bash
# lint_json.sh
#
# Created on: Nov 24 2022
# Created by: gojun077@gmail.com
# Last Updated: Nov 24 2022
#
# This bash script is intended to be executed locally in a git pre-commit
# hook to check json syntax for `.json` files. This script
# requires that `jq` be installed locally.

# bash array to store list of .json files to be committed
gitfiles=$(git diff-index --cached --name-only HEAD | grep -E '\.(json)')

function test_file() {
  myfile="${1}"

  if [ ! -f "${myfile}" ]; then
    return
  fi

  printf "%s\\n" "Linting json with *jq*..."
  if test jq; then
    jq "${myfile}"
  else
    printf "%s\\n" "'jq' not found in PATH; please install jq"
  fi
}

case "${1}" in
  --about )
      printf "%s\\n" "json syntax lint"
      ;;
  * )
    for f in $gitfiles; do
      test_file "$f"
    done
    ;;
esac
