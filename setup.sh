#!/bin/sh
set -euo pipefail
cd "$(dirname "$0")"
git pull
ln -sfn ~ .homelink
xargs stow < normal.lst
systemctl --user daemon-reload
systemctl --user enable dropbox redshift-gtk psd
