#!/bin/sh
set -eu

parse() {
    if command -v jq > /dev/null; then
	jq -r '.[].key'
    elif command -v python3 > /dev/null; then
	python3 -c 'import json,sys
for k in json.load(sys.stdin):
    print(k["key"])
'
    else
	false
    fi
}

# install ssh keys
mkdir -p ~/.ssh
chmod 700 ~/.ssh
curl -s https://api.github.com/users/kcolford/keys | parse > ~/.ssh/authorized_keys
chmod 600 ~/.ssh/authorized_keys
