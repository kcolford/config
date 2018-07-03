for i in ~/.profile.d/*; do
    if [ -r "$i" ]; then
	. "$i"
    fi
done
