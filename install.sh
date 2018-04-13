#!/bin/sh
set -euo pipefail

cat <<EOF

Install emacs for a workstation, emacs-nox for a headless workstation,
and base-devel for AUR support.

EOF

# configuration variables
country=CA
timezone=America/Toronto

check_installed() {
    pacman -Q "$@" > /dev/null 2>&1
}

check_runable() {
    for i; do
	[ -x "$(command -v "$i")" ] || return 1
    done
}

systemctl_activate() {
    systemctl enable "$@"
    systemctl start "$@"
}

. /etc/os-release
if [ "$ID" != arch ]; then
    echo "Only run this script on Arch Linux" >&2
    exit 1
fi

# setup timezone
ln -sf /usr/share/zoneinfo/$timezone /etc/localtime

# setup localization
cat > /etc/locale.gen <<EOF
en_$country.UTF-8 UTF-8
en_US.UTF-8 UTF-8
EOF
locale-gen
cat > /etc/locale.conf <<EOF
LANG=en_$country.UTF-8
EOF

# setup the linux console
cat > /etc/vconsole.conf <<EOF
KEYMAP=us
FONT=Lat2-Terminus16
EOF

# I don't like vi so make nano the global minimal default since it's
# also installed
if check_runable emacs; then
    cat > /etc/environment <<EOF
EDITOR=emacs
EOF
else
    cat > /etc/environment <<EOF
EDITOR=nano
EOF
fi

# update mirror list
if check_runable reflector; then
    reflector -c $country --sort rate > /etc/pacman.d/mirrorlist.tmp
else
    curl "https://www.archlinux.org/mirrorlist/?country=$country" | sed 's/#//' | rankmirrors - > /etc/pacman.d/mirrorlist.tmp
fi
mv /etc/pacman.d/mirrorlist.tmp /etc/pacman.d/mirrorlist

# setup package installer
pacman='pacman'
installer() {
    $pacman -S --quiet --noconfirm --needed "$@" > /dev/null
}

# cleaner package installs
installer pacmatic
pacman='pacmatic'

# setup sudo
installer sudo
add_sudo_policy() {
    if ! check_installed sudo; then
	return 0
    fi
    if [ -f "$1" ]; then
	cp -a "$1" /etc/sudoers.d.tmp
    fi
    touch "$1"
    chmod 440 "$1"
    cat > "$1"
    if ! visudo -c > /dev/null; then
	if [ -f /etc/sudoers.d.tmp ]; then
	    mv /etc/sudoers.d.tmp /etc/sudoers.d/psd
	else
	    rm /etc/sudoers.d/psd
	fi
	if visudo -c; then
	    return 1
	else
	    # TODO: manually intervene to fix things
	    exit 2
	fi
    fi
}
add_sudo_policy /etc/sudoers.d/wheel <<EOF
%wheel ALL=(ALL:ALL) ALL
#%wheel ALL=(ALL:ALL) NOPASSWD: ALL
EOF

# configure networking
installer networkmanager
systemctl_activate NetworkManager

# wireless regulation conformance
installer crda
if [ -f /etc/conf.d/wireless-regdom ]; then
    sed -i "/$country/s/#//" /etc/conf.d/wireless-regdom
fi

# sync time
installer chrony
systemctl_activate chronyd

# crontab support, not essential but often useful
installer cronie
systemctl_activate cronie

# lock the root account if we have a sudo user
if [ "$SUDO_USER" ]; then
    passwd -l root
fi

# if we're run with sudo then that means there's an unprivileged user
# to run AUR builds with trizen, but only add AUR support if
# base-devel is installed
if [ "$SUDO_USER" ] && check_installed $(pacman -Sqg base-devel); then
    if ! check_installed trizen; then
	sudo -u "$SUDO_USER" git clone https://aur.archlinux.org/trizen /tmp/trizen
	( cd /tmp/trizen && sudo -u "$SUDO_USER" makepkg -si )
    fi
    pacman="sudo -u $SUDO_USER trizen"
    if check_installed pacmatic; then
	pacman="$pacman --pacman_command=/usr/bin/pacmatic"
    fi
fi

# update system
installer -yu

# if a version of Emacs is installed then we must be a user
# installation
if check_runable emacs; then
    installer ccid cups ghostscript foomatic-db sane pkgfile pkgstats aspell aspell-en
    systemctl_activate pcscd org.cups.cupsd pkgfile-update.timer
fi

# if the plain emacs package is installed rather than emacs-nox then
# it means we're working with a graphical environment
if check_installed emacs && ! check_installed emacs-nox; then
    installer i3 || true	# the i3 group is weird
    installer xorg xterm terminus-font compton xss-lock udiskie feh redshift python-xdg dunst xdotool dex dmenu linux-zen linux-zen-headers lightdm lightdm-gtk-greeter syncthing-gtk qbittorrent chromium ttf-liberation noto-fonts libu2f-host
    systemctl enable lightdm
    # these are from the AUR and don't have to be installed
    installer dropbox chromium-widevine profile-sync-daemon || true
fi

# set the initcpio hooks
sed -i '/^HOOKS=/s/=(.*)/=(base systemd autodetect modconf pcmcia block mdadm_udev keyboard sd-vconsole sd-encrypt sd-lvm2 filesystems fsck)/' /etc/mkinitcpio.conf
echo "Generating initcpio..."
mkinitcpio -P > /dev/null

# if grub is installed then use it
installer grub
if check_installed grub; then
    sed -i '/^GRUB_CMDLINE_LINUX=/s/=.*/="zswap.enabled=1"/' /etc/default/grub
    sed -i 's/^#\? \?GRUB_ENABLE_CRYPTODISK=.*/GRUB_ENABLE_CRYPTODISK=y/' /etc/default/grub
    grub-mkconfig -o /boot/grub/grub.cfg
    if [ -d /sys/firmware/efi/efivars ]; then
	installer efibootmgr
	grub-install
    else
	grub-install "$(lsblk -psno NAME "$(findmnt -no SOURCE /)" | tail -n 1)"
    fi
fi

# setup profile sync daemon if it's installed
if [ "$SUDO_USER" ] && check_installed profile-sync-daemon; then
    add_sudo_policy /etc/sudoers.d/psd <<EOF
$SUDO_USER ALL=(ALL) NOPASSWD: /usr/bin/psd-overlay-helper
EOF
fi

# setup postgres
if check_installed postgresql; then
    if ! [ -f /var/lib/postgres/data/postgresql.conf ]; then
	sudo -u postgres initdb -D /var/lib/postgres/data --auth-host=pam --auth-local=ident
    fi
fi

