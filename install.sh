#!/bin/sh
set -euo pipefail

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

systemctl_deactivate() {
    systemctl disable "$@" || true
    systemctl stop "$@" || true
}

check_file() {
    for file; do
	if [ -e "$file" ]; then
	    return 0
	fi
    done
    return 1
}

. /etc/os-release
if [ "$ID" != arch ]; then
    echo "Only run this script on Arch Linux" >&2
    exit 1
fi

# determine country
country="$(curl -s https://ipinfo.io/country)" || true
country="${country:-CA}"
echo "Your country is $country"

# need this for adding pacman hooks
mkdir -p /etc/pacman.d/hooks

# update mirror list
if check_runable reflector; then
    reflector -c "$country" --sort rate > /etc/pacman.d/mirrorlist.tmp
else
    curl -s "https://www.archlinux.org/mirrorlist/?country=$country" | sed 's/#//' | rankmirrors - > /etc/pacman.d/mirrorlist.tmp
fi
mv /etc/pacman.d/mirrorlist.tmp /etc/pacman.d/mirrorlist

# enable multilib repos
sed -i '/\[multilib]/{s/#//;n;s/#//}' /etc/pacman.conf

# update the system
if check_installed pacmatic; then
    env DIFFPROG=diff pacmatic -Syyu
else
    pacman -Syyu
fi

# setup package installer
pacman='pacman'
installer() {
    $pacman -S --quiet --noconfirm --needed "$@"
}

# install these packages to optimize this script and provide better
# integration later on
installer pacmatic jq reflector sudo

# improve the installer used
# if check_installed pacmatic; then
#     export DIFFPROG=diff
#     pacman='pacmatic'
# fi

if systemd-detect-virt -q; then
    virtualized=true
else
    virtualized=false
fi

if check_file /sys/class/power_supply/BAT*; then
    laptop=true
else
    laptop=false
fi

if check_file /sys/class/net/wl*; then
    wireless=true
else
    wireless=false
fi

# chrony is more reliable than ntpd or systemd-timesyncd with an
# unreliable connection
if $laptop; then
    installer chrony
fi

if $wireless; then
    installer crda networkmanager
fi

if ! $virtualized; then
    installer grub
fi

cat <<EOF
Install emacs for a workstation, emacs-nox for a headless workstation,
and base-devel for AUR support.
EOF
if check_runable emacs; then
    userinstall=true
else
    userinstall=false
fi
if [ "$SUDO_USER" ] && check_installed $(pacman -Sqg base-devel); then
    aursupport=true
else
    aursupport=false
fi
if check_installed chromium || check_installed emacs && ! check_installed emacs-nox; then
    graphical=true
else
    graphical=false
fi

# add aur support
if $aursupport; then
    if ! check_installed trizen; then
	sudo -u "$SUDO_USER" git clone https://aur.archlinux.org/trizen /tmp/trizen
	( cd /tmp/trizen && sudo -u "$SUDO_USER" makepkg -si )
    fi
    pacman="sudo -u $SUDO_USER trizen"
fi

# setup timezone
timezone="$(curl -s 'ipinfo.io/loc' | sed 's/,/ /' | awk '{print "api.geonames.org/timezoneJSON?lat=" $1 "&lng=" $2 "&username=kcolford";}' | xargs curl -s | jq -r .timezoneId)" || true
timezone="${timezone:-America/Toronto}"
echo "Your timezone is $timezone"
ln -sf "/usr/share/zoneinfo/$timezone" /etc/localtime

# setup localization
locale=en_CA
cat > /etc/locale.gen <<EOF
$locale.UTF-8 UTF-8
en_US.UTF-8 UTF-8
EOF
locale-gen
cat > /etc/locale.conf <<EOF
LANG=$locale.UTF-8
EOF

# setup the linux console
cat > /etc/vconsole.conf <<EOF
KEYMAP=us
FONT=Lat2-Terminus16
EOF

# I don't like vi so make nano or emacs the global default.
if check_runable emacs; then
    cat > /etc/environment <<EOF
EDITOR=emacs
EOF
else
    cat > /etc/environment <<EOF
EDITOR=nano
EOF
fi

# setup sudo
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

if check_installed networkmanager; then
    systemctl_deactivate dhcpcd
    systemctl_activate NetworkManager
fi

if [ -f /etc/conf.d/wireless-regdom ]; then
    sed -i "/$country/s/#//" /etc/conf.d/wireless-regdom
fi

# sync time
if check_installed chrony; then
    timedatectl set-ntp false
    systemctl_deactivate ntpd
    systemctl_activate chronyd
fi

# lock the root account if we have a sudo user
if [ "$SUDO_USER" ] && [ "$SUDO_USER" != root ]; then
    passwd -l root
fi

if $userinstall; then
    installer ccid cups ghostscript foomatic-db sane pkgfile pkgstats
    installer watchman || true
    systemctl_activate pcscd org.cups.cupsd pkgfile-update.timer
fi

if $graphical; then
    installer i3 || true	# the i3 group is weird
    installer linux-zen lightdm syncthing-gtk qbittorrent chromium
    installer dropbox || true
fi

if check_installed linux-zen; then
    installer linux-zen-headers
fi

if check_installed linux; then
    installer linux-headers
fi

if check_installed lightdm; then
    installer lightdm-gtk-greeter
    systemctl enable lightdm
fi

if check_runable i3; then
    installer xorg xterm compton xss-lock udiskie feh redshift dunst xdotool dex dmenu
fi

if check_installed redshift; then
    installer python-xdg
fi

if check_runable xterm; then
    installer terminus-font
fi

sed '/^HOOKS=/s/=(.*)/=(base systemd autodetect modconf pcmcia block mdadm_udev keyboard sd-vconsole sd-encrypt sd-lvm2 filesystems fsck)/' /etc/mkinitcpio.conf > /etc/mkinitcpio.conf.tmp
if cmp -s /etc/mkinitcpio.conf /etc/mkinitcpio.conf.tmp; then
    rm /etc/mkinitcpio.conf.tmp
else
    mv /etc/mkinitcpio.conf.tmp /etc/mkinitcpio.conf
    echo "Generating initcpio..."
    mkinitcpio -P > /dev/null
fi

linuxcmdline='zswap.enabled=1 nvidia-drm.modeset=1'

if check_installed grub; then
    # add linux command line
    sed -i "s/^#\\? *\\(GRUB_CMDLINE_LINUX_DEFAULT\\)=.*/\\1=\"quiet $linuxcmdline\"/" /etc/default/grub
    # enable encrypted /boot support
    sed -i 's/^#\? *\(GRUB_ENABLE_CRYPTODISK\)=.*/\1=y/' /etc/default/grub

    # save last booted kernel
    sed -i '/^#\? *\(GRUB_DEFAULT\)=.*/\1=saved/' /etc/default/grub
    sed -i '/^#\? *\(GRUB_SAFEDEFAULT\)=.*/\1="true"/' /etc/default/grub

    # install grub
    grub-mkconfig -o /boot/grub/grub.cfg
    if [ -d /sys/firmware/efi/efivars ]; then
	installer efibootmgr
	grub-install
    else
	grub-install "$(lsblk -lpsno NAME "$(findmnt -no SOURCE /)" | tail -n 1)"
    fi
fi

if check_installed postgresql; then
    chattr +C -R /var/lib/postgres/data/
    if ! [ -f /var/lib/postgres/data/postgresql.conf ]; then
	sudo -u postgres initdb -D /var/lib/postgres/data --auth-host=pam --auth-local=peer --data-checksums
    fi
fi

if check_installed libvirt; then
    chattr +C -R /var/lib/libvirt/images/
    installer
fi

if [ "$(findmnt -no FSTYPE /)" = btrfs ]; then
    sed -i '/autodefrag/n;s=\([^[:space:]]\+\) */ *\(auto\|btrfs\) *\(.*\)=\1 / \2 \3,autodefrag=' /etc/fstab
    mount -o remount /
fi

if check_installed cronie; then
    systemctl_activate cronie
fi

# install helper applications for the shell in a user installation
if $userinstall; then
    installer hub thefuck
fi

if $userinstall && ( check_installed chromium || check_installed google_chrome ); then
    installer ttf-liberation noto-fonts libu2f-host
    installer profile-sync-daemon || true
fi

if $userinstall && check_installed chromium; then
    installer chromium-widevine || true
fi

# if emacs is installed then we need to install the helper
# applications for it
if $userinstall && check_runable emacs; then
    installer xclip shellcheck aspell aspell-en
    installer go-tools
    installer gocode-git || true
    installer flake8 autopep8 yapf ipython python-jedi python-rope python-virtualenv
    installer prettier
    installer cmake clang
fi

if [ "$SUDO_USER" ] && check_installed profile-sync-daemon; then
    add_sudo_policy /etc/sudoers.d/psd <<EOF
$SUDO_USER ALL=(ALL) NOPASSWD: /usr/bin/psd-overlay-helper
EOF
fi

if $laptop; then
    sed -i 's/#\? *\(HandleLidSwitch\)=.*/\1=sleep/' /etc/systemd/logind.conf
fi

if $graphical && [ -d /sys/module/i915 ]; then
    installer xf86-video-intel
fi

# if [ -d /sys/module/nouveau ]; then
#     installer nvidia-dkms
# fi

if $graphical && [ -d /sys/module/nouveau ]; then
    installer xf86-video-nouveau
fi

if check_installed nvidia || check_installed nvidia-dkms; then
    installer nvidia-utils
fi

if check_installed nvidia; then
    cat > /etc/pacman.d/hooks/nvidia.hook <<EOF
[Trigger]
Operation=Install
Operation=Upgrade
Operation=Remove
Type=Package
Target=nvidia

[Action]
Depends=mkinitcpio
When=PostTransaction
Exec=/usr/bin/mkinitcpio -P
EOF

if $graphical; then
    cat > /etc/X11/xorg.conf.d/30-touchpad.conf <<EOF
Section "InputClass"
	Identifier "devname"
	Driver "libinput"
	MatchIsTouchpad "on"
	Option "NaturalScrolling" "true"
EndSection
EOF
fi
