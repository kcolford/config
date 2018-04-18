#!/bin/sh
set -eu

# install ssh keys
mkdir -p ~/.ssh
chmod 700 ~/.ssh
curl -s https://api.github.com/users/kcolford/keys | jq -r '.[].key' > ~/.ssh/authorized_keys
chmod 600 ~/.ssh/authorized_keys
