#!/bin/sh

cd "$(dirname "$0")" || exit
ln -sf ~ .homelink
xargs stow < normal.lst
