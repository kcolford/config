#!/bin/sh
set -eu
dir="$(dirname "$0")"
category="$(realpath -s --relative-to "$dir" "$1")"
shift
for file; do
    rel="$(realpath -s --relative-to ~ "$file")"
    mkdir -p "$dir"/"$category"/"$(dirname "$rel")"
    if [ -h "$file" ]; then
	ln -sr "$file" "$dir"/"$category"/"$rel"
	rm "$file"
    else
	mv "$file" "$dir"/"$category"/"$rel"
    fi
done
cd "$dir" || exit
stow "$category"
