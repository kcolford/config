#!/bin/sh
set -euo pipefail
dir="$(dirname "$0")"
category="$1"
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
sh "$dir"/setup.sh
