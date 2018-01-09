#!/bin/sh
set -e
cd "$(dirname "$0")"
git pull
ln -sfn ~ .homelink
xargs stow < normal.lst
