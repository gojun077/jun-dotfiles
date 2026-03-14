#!/usr/bin/env python
# check_illegal_chars.py
#
# Created on: Late 2025
# Last Updated: Sat 14 Mar 2026
#
# This script is intended to be run by a pre-commit script under
# `.git/hooks/` within a code repo. Since files in `.git/` are not
# added to the git index, I placed a copy in my dotfiles repo.
#
# This script checks for invalid Unicode characters that can cause
# shell scripts and other interpreted languages to return errors in
# POSIX environments. Invalid Unicode chars include unprintable
# whitespace, endash, emdash, etc.

import sys
import re
import argparse

# CORRECTED PATTERN: This pattern is designed to catch characters that can
# cause issues in source code, focusing only on non-standard whitespace,
# WYSIWYG (ex: endash, emdash..) characters, and unprintable control characters.
FORBIDDEN_CHARS_PATTERN = re.compile(
    r"("
    # GROUP 1: Problematic Unicode Whitespace & Separators
    # Includes non-breaking spaces, zero-width spaces, special paragraph separators, etc.
    r"[\u00A0\u1680\u180E\u2000-\u200B\u202F\u205F\u3000]"
    r"|"
    # GROUP 2: "Fancy" Characters from WYSIWYG Editors (e.g., MS Word)
    # Includes smart quotes, dashes, and ellipses that aren't standard ASCII.
    r"[\u2013-\u2014\u2018-\u2019\u201C-\u201D\u2026]"
    r"|"
    # GROUP 3: Unprintable Control Characters (C0 and C1 controls)
    # We explicitly EXCLUDE tab(\x09), newline(\x0A), and carriage return(\x0D).
    r"[\x00-\x08\x0B-\x0C\x0E-\x1F\x7F-\x9F]"
    r")"
)

def main():
    """Checks files for forbidden characters and exits with a status code."""
    parser = argparse.ArgumentParser()
    parser.add_argument('filenames', nargs='+', help='Filenames to check.')
    args = parser.parse_args()

    found_issues = False

    for filename in args.filenames:
        try:
            with open(filename, 'r', encoding='utf-8') as f:
                for line_num, line in enumerate(f, 1):
                    # Use re.finditer to find all occurrences in a line
                    for match in re.finditer(FORBIDDEN_CHARS_PATTERN, line):
                        found_issues = True
                        # Use repr() to make unprintable characters visible
                        char_found = repr(match.group(1))
                        print(
                            f"ERROR: Forbidden character {char_found} found in "
                            f"{filename} on line {line_num}."
                        )
        except (IOError, UnicodeDecodeError) as e:
            print(f"Could not check {filename}: {e}")

    if found_issues:
        print("\nCommit aborted. Please remove the characters listed above.")
        return 1

    return 0

if __name__ == '__main__':
    sys.exit(main())
