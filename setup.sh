#!/bin/bash
set -euo pipefail
lst="$(realpath "${1:-"$(dirname "$0")"/normal.lst}")"
cd "$(dirname "$0")"
git pull
ln -sfn ~ .homelink
xargs stow < "$lst"
systemctl --user daemon-reload
systemctl --user enable emacs redshift-gtk psd
