for i in ~/.profile.d/*; do
    if [ -f "$i" ]; then
	. "$i"
    fi
done
