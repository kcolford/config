#!/bin/sh
set -eu

lst="$(realpath "${1:-"$(dirname "$0")"/normal.lst}")"
cd "$(dirname "$0")"
git pull
ln -sfn ~ .homelink
xargs stow < "$lst"
systemctl --user daemon-reload
systemctl --user enable emacs redshift-gtk psd || true

# install ssh keys
mkdir -p ~/.ssh
chmod 700 ~/.ssh
curl -s https://api.github.com/users/kcolford/keys | jq -r '.[].key' > ~/.ssh/authorized_keys
chmod 600 ~/.ssh/authorized_keys

# setup completions
complete_dir="${BASH_COMPLETION_USER_DIR:-${XDG_DATA_HOME:-$HOME/.local/share}/bash-completion}/completions"
set +e
npm completion > "$complete_dir"/npm
pip completion --bash > "$complete_dir"/pip
set -e

true
