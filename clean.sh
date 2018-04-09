#!/bin/sh
set -euo pipefail
find "$(dirname "$0")" -name '*~' -o \( -empty -type d \) -delete
find -L ~ \! -path ~/.'*' -prune -o -type l -exec rm {} \;
