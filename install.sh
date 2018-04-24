#!/bin/bash
set -euo pipefail

check_installed() {
    pacman -Q "$@" > /dev/null 2>&1
}

check_runable() {
    for i; do
	[ -x "$(command -v "$i")" ] || return 1
    done
}

c="^ *#\\? *"
shellvar_edit() {
    first="$1"
    second="$2"
    if [ "$#" = 2 ]; then
	if grep -q "$c$2" "$1"; then
	    echo "# $2" >> "$1"
	fi
	sed "s/^/#/;s/^#$//;s|$c$2|$2|" "$1" > "$1".tmp
    elif [ "$#" = 3 ]; then
	if grep -q "$c$2=.*" "$1"; then
	    echo "# $2=''" >> "$1"
	fi
	if [ "$(grep -c "$c$2=" "$1")" != 1 ]; then
	    sed "s|$c\\($2=\"\\?$(printf %q "$3")\"\\?\\)|\\1|" "$1" > "$1".tmp
	else
	    sed "s|$c$2=.*|$2=$(printf %q "$3")|" "$1" > "$1".tmp
	fi
    else
	if grep -q "$c$2=(.*)" "$1"; then
	    echo "# $2=()" >> "$1"
	fi
	third="$3"
	shift 3
	sed "s|$c$second=(.*)|$second=($(printf %q "$third")$(printf ' %q' "$@"))|" "$first" > "$first".tmp
    fi
    if cmp -s "$first" "$first".tmp; then
	rm "$first".tmp
	changed=true
    else
	cat "$first".tmp > "$first"
	rm "$first".tmp
	changed=false
    fi
}

systemctl_activate() {
    q systemctl enable "$@"
    q systemctl start "$@"
}

systemctl_deactivate() {
    systemctl disable "$@" > /dev/null 2>&1 || true
    systemctl stop "$@" > /dev/null 2>&1 || true
}

check_file() {
    for file; do
	if [ -e "$file" ]; then
	    return 0
	fi
    done
    return 1
}

check_fstype() {
    [ "$(findmnt -lno FSTYPE "$(df -P "$1" | awk 'END{print $NF}')")" = "$2" ]
}

qq() {
    t="$(mktemp)"
    if "$@" 2> "$t"; then
	rm "$t"
	return 0
    else
	cat "$t"
	rm "$t"
	return 1
    fi
}

q() {
    t="$(mktemp)"
    if "$@" > "$t" 2>&1 < /dev/null; then
	rm "$t"
	return 0
    else
	cat "$t" >&2
	rm "$t"
	return 1
    fi
}

uninstaller() {
    if [ $# != 0 ]; then
	q pacman -Rs --noconfirm "$@"
    fi
}

pacman='q pacman'
installer() {
    $pacman -S --noconfirm --needed "$@"
}

. /etc/os-release
if [ "$ID" != arch ]; then
    echo "Only run this script on Arch Linux" >&2
    exit 1
fi

# instructions
cat <<EOF
Install emacs for a workstation, emacs-nox for a headless workstation,
and base-devel for AUR support.

Also include configuration options on the command line to decide how
to configure the server.

EOF

linuxcmdline=''
locale=en_CA
router=false
freeonly=false
server=false
hibernate=false
full=true
if check_runable emacs; then
    userinstall=true
else
    userinstall=false
fi
if check_installed chromium || check_installed emacs && ! check_installed emacs-nox; then
    graphical=true
else
    graphical=false
fi
if [ "$SUDO_USER" ] && [ "$SUDO_USER" != root ]; then
    has_admin=true
    admin_user="$SUDO_USER"
else
    has_admin=false
fi
# shellcheck disable=SC2046
if $has_admin && check_installed $(pacman -Sqg base-devel); then
    aursupport=true
else
    aursupport=false
fi

for cfg in "$@"; do
    case "$cfg" in
	server) server=true ;;
	router) router=true ;;
	freeonly) freeonly=true ;;
	headless) graphical=false ;;
	nographical) graphical=false ;;
	graphics|graphical) graphical=true ;;
	hibernate|hibernation) hibernate=true ;;
	aur) aursupport=true ;;
	user) userinstall=true ;;
	nouser) userinstall=false ;;
	nofull|min|minimal) full=false ;;
	*) echo "Invalid configuration '$cfg'." >&2; exit 2 ;;
    esac
done

if check_runable powerpill; then
    pacman='q powerpill'
fi

if check_file /sys/class/power_supply/BAT*; then
    laptop=true
    echo "System has a battery and so is a laptop."
else
    laptop=false
    echo "System does not have a battery and so is not a laptop."
fi

if check_file /sys/class/net/wl*; then
    wireless=true
    echo "Wireless card detected."
else
    wireless=false
    echo "No wireless card detected."
fi

if systemd-detect-virt -v -q; then
    virtualized=true
    virtualization="$(systemd-detect-virt -v)"
else
    virtualized=false
fi

if systemd-detect-virt -c -q; then
    contained=true
    container="$(systemd-detect-virt -c)"
else
    contained=false
fi

if $contained && [ "$container" = systemd-nspawn ]; then
    systemctl_activate systemd-networkd
fi

if systemctl is-enabled systemd-networkd > /dev/null; then
    systemctl_activate systemd-resolved
fi

country="$(curl -s https://ipinfo.io/country)" || true
country="${country:-CA}"
echo "Your country is $country"

# update mirror list
if check_runable reflector; then
    reflector -c "$country" --sort rate > /etc/pacman.d/mirrorlist.tmp
else
    curl -s "https://www.archlinux.org/mirrorlist/?country=$country" | sed 's/#//' | rankmirrors - > /etc/pacman.d/mirrorlist.tmp
fi
mv /etc/pacman.d/mirrorlist.tmp /etc/pacman.d/mirrorlist

# enable multilib repos
if $full; then
    sed -i '/\[multilib]/{s/#//;n;s/#//}' /etc/pacman.conf
fi

# add xyne's repositories to improve package selection
if $full && ! grep -q '^\[xyne-.*]' /etc/pacman.conf; then
    # he only publishes certain architectures
    case "$(uname -m)" in
	x86_64)
	    arch="$(uname -m)"
	    ;;
	*)
	    arch="any"
	    ;;
    esac
    cat >> /etc/pacman.conf <<EOF
[xyne-$arch]
Server = https://xyne.archlinux.ca/repos/xyne
EOF
    q pacman -Sy
fi

if $full && check_installed pacserve; then
    systemctl_activate pacserve
    sed -i -f - /etc/pacman.conf <<'EOF'
/^\[.*]$/N
s|^\(\[.*]\)\nInclude = /etc/pacman.d/pacserve$|\1|
s|^\(\[.*]\)|\1\nInclude = /etc/pacman.d/pacserve|
s|^\(\[options]\)\nInclude = /etc/pacman.d/pacserve|\1|
EOF
else
    systemctl_deactivate pacserve
    sed -i '\|^Include = /etc/pacman.d/pacserve$|d' /etc/pacman.conf
fi

if $aursupport; then
    # if bauerbill can be easily used then use it, otherwise install
    # and use trizen
    if check_installed bauerbill || grep -q '^\[xyne-.*]$' /etc/pacman.conf; then
	installer base-devel
	installer bauerbill
	pacman="bb-wrapper --bb-quiet --build-dir /tmp/build --aur"
    else
	if ! check_installed trizen; then
	    installer base-devel # just to make sure it's available
	    sudo -u "$admin_user" git clone https://aur.archlinux.org/trizen /tmp/trizen
	    ( cd /tmp/trizen && q sudo -u "$admin_user" makepkg -si )
	fi
	pacman="q sudo -u $admin_user trizen"
    fi
fi

# update the system
installer -yu
env DIFFPROG='diff -aur' pacdiff

# From here on out we can start installing and configuring whatever we
# want.

# install these by default
if $full; then
    installer jq reflector sudo dash cronie etckeeper borg rsync
fi

# need this for adding pacman hooks
mkdir -p /etc/pacman.d/hooks

# keep track of all installed packages
cat > /etc/pacman.d/hooks/pkglist.hook <<EOF
[Trigger]
Operation=Install
Operation=Remove
Type=Package
Target=*

[Action]
When=PostTransaction
Exec=/bin/sh -c '/usr/bin/pacman -Qqe > /etc/pkglist.txt'
EOF

if check_installed etckeeper; then
    git config --global user.name root
fi

# keep the package cache trimmed
cat > /etc/pacman.d/hooks/paccache.hook <<EOF
[Trigger]
Operation=Install
Operation=Upgrade
Operation=Remove
Type=Package
Target=*

[Action]
When=PostTransaction
Exec=/usr/bin/paccache -ru
EOF

# choose the method of time synchronization, we can disable all forms
# temporarily while we choose which one to setup
timedatectl set-ntp false
systemctl_deactivate ntpd chronyd
if $router; then
    installer ntp
fi
if $laptop; then
    installer chrony
    systemctl_activate chronyd
elif check_installed ntp; then
    systemctl_activate ntpd
else
    timedatectl set-ntp true
fi

if $wireless; then
    installer crda wpa_supplicant iw
fi

if ! $router && $wireless; then
    installer networkmanager
fi

if $router && $wireless; then
    installer hostapd
fi

if ! $contained; then
    installer grub
fi

# setup timezone
timezone="$(curl -s 'ipinfo.io/loc' | sed 's/,/ /' | awk '{print "api.geonames.org/timezoneJSON?lat=" $1 "&lng=" $2 "&username=kcolford";}' | xargs curl -s | jq -r .timezoneId)" || true
timezone="${timezone:-America/Toronto}"
echo "Your timezone is $timezone"
ln -sf "/usr/share/zoneinfo/$timezone" /etc/localtime

# setup localization
cat > /etc/locale.gen <<EOF
$locale.UTF-8 UTF-8
en_US.UTF-8 UTF-8
EOF
shellvar_edit /etc/locale.conf LANG "$locale.UTF-8"
echo "Generating locales.."
q locale-gen

# setup the linux console
shellvar_edit /etc/vconsole.conf KEYMAP us
shellvar_edit /etc/vconsole.conf FONT Lat2-Terminus16
if [ "$TERM" = linux ]; then
    setfont Lat2-Terminus16
fi

if $userinstall; then
    if $graphical; then
	installer emacs
    else
	installer emacs-nox
    fi
fi

# I don't like vi so make nano or emacs the global default.
if check_runable emacs; then
    shellvar_edit /etc/environment EDITOR emacs
else
    shellvar_edit /etc/environment EDITOR nano
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
	    echo "A huge error happened. Starting shell so you can fix sudo."
	    bash
	    exit 2
	fi
    fi
}
add_sudo_policy /etc/sudoers.d/wheel <<EOF
%wheel ALL=(ALL:ALL) ALL
#%wheel ALL=(ALL:ALL) NOPASSWD: ALL
EOF

if $wireless && ! $router && check_installed networkmanager; then
    systemctl_deactivate dhcpcd
    systemctl_activate NetworkManager
fi

if [ -f /etc/conf.d/wireless-regdom ]; then
    shellvar_edit /etc/conf.d/wireless-regdom WIRELESS_REGDOM "$country"
fi

# lock the root account if we have a sudo user
if $has_admin; then
    q passwd -l root
fi

if $userinstall; then
    installer ccid cups ghostscript foomatic-db sane pkgfile pkgstats
    installer watchman || true
    systemctl_activate pcscd org.cups.cupsd pkgfile-update.timer
fi

if $graphical; then
    installer i3 2> /dev/null || true # the i3 group is weird
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

# for my local configuration files
if check_runable i3; then
    installer xorg xterm compton xss-lock udiskie feh redshift dunst xdotool dex dmenu terminus-font
fi

if check_installed redshift; then
    installer python-xdg
fi

if $virtualized && [ "$virtualization" = oracle ]; then
    installer virtualbox-guest-dkms virtualbox-guest-iso
    if $graphical; then
	installer virtualbox-guest-utils
    else
	installer virtualbox-guest-utils-nox
    fi
fi

if $virtualized && [ "$virtualization" = kvm ]; then
    if $graphical; then
	installer xf86-video-qxl
    fi
fi

linuxcmdline="$linuxcmdline zswap.enabled=1"

if check_installed postgresql; then
    chattr +C -R /var/lib/postgres/data/
    if ! [ -f /var/lib/postgres/data/postgresql.conf ]; then
	sudo -u postgres initdb -D /var/lib/postgres/data --auth-host=pam --auth-local=peer --data-checksums
    fi
fi

if check_installed libvirt; then
    chattr +C -R /var/lib/libvirt/images/
    installer ebtables dnsmasq bridge-utils qemu radvd dmidecode
    if $graphical; then
	installer virt-viewer
    fi
    installer libguestfs || true
    systemctl_activate libvirtd
fi

if check_fstype / btrfs; then
    installer btrfs-progs
    sed -i -f - /etc/fstab <<'EOF'
/autodefrag/n
s=\([^[:space:]]\+\) */ *\(auto\|btrfs\) *\(.*\)=\1 / \2 \3,autodefrag=
EOF
    sed -i -f - /etc/fstab <<'EOF'
/user_subvol_rm_allowed/n
s=\([^[:space:]]\+\) */ *\(auto\|btrfs\) *\(.*\)=\1 / \2 \3,user_subvol_rm_allowed=
EOF
    mount -o remount /
fi

if check_installed cronie; then
    systemctl_activate cronie
fi

# install helper applications for the shell in a user installation
if $userinstall; then
    installer hub thefuck
fi

if $userinstall && ( check_installed chromium || check_installed google-chrome ); then
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
    installer bear || true
fi

# for eduroam configuration
if $laptop && $graphical && check_installed networkmanager; then
    installer network-manager-applet
fi

if $has_admin && check_installed profile-sync-daemon; then
    add_sudo_policy /etc/sudoers.d/psd <<EOF
$admin_user ALL=(ALL) NOPASSWD: /usr/bin/psd-overlay-helper
EOF
fi

if $laptop; then
    if $server; then
	shellvar_edit /etc/systemd/logind.conf HandleLidSwitch ignore
    elif $hibernate; then
	shellvar_edit /etc/systemd/logind.conf HandleLidSwitch hybrid-sleep
    else
	shellvar_edit /etc/systemd/logind.conf HandleLidSwitch sleep
    fi
fi

if $graphical && [ -d /sys/module/i915 ]; then
    installer xf86-video-intel
fi

# the proprietary module is faster but doesn't always work with other
# features
if ! $freeonly && [ -d /sys/module/nouveau ]; then
    installer nvidia-dkms
fi

if $graphical && [ -d /sys/module/nouveau ]; then
    installer xf86-video-nouveau
fi

if check_installed nvidia; then
    uninstaller nvidia
    installer nvidia-dkms
fi

if check_installed nvidia-dkms; then
    linuxcmdline="$linuxcmdline nvidia-drm.modeset=1"
fi

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

if [ "$(cat /sys/devices/virtual/dmi/id/product_name)" = Glimmer ]; then
    installer lenovo-thinkpad-yoga-11e-chromebook-git || true
fi

# clean up unneeded packages
# shellcheck disable=SC2046
uninstaller $(pacman -Qqdtt)

# optimize the pacman database
q pacman-optimize

if check_installed dash; then
    ln -sfT dash /usr/bin/sh
    cat > /etc/pacman.d/hooks/dash.hook <<EOF
[Trigger]
Operation=Install
Operation=Upgrade
Type=Package
Target=bash

[Action]
Description=Pointing /bin/sh to dash...
When=PostTransaction
Exec=/usr/bin/ln -sfT dash /usr/bin/sh
Depends=dash
EOF
    cat > /etc/pacman.d/hooks/dash-remove.hook <<EOF
[Trigger]
Operation=Remove
Type=Package
Target=dash

[Action]
Description=Pointing /bin/sh to bash...
When=PostTransaction
Exec=/bin/bash -c 'ln -sfT bash /usr/bin/sh && rm /etc/pacman.d/hooks/dash.hook && rm /etc/pacman.d/hooks/dash-remove.hook'
EOF
fi

if $server; then
    installer haveged
fi

if check_installed haveged; then
    systemctl_activate haveged
fi

if check_installed docker; then
    if check_fstype /var/lib/docker btrfs; then
	installer btrfs-progs
    fi
    systemctl_activate docker.socket
fi

if check_installed tesseract; then
    case "$locale" in
	en*)
	    installer tesseract-data-eng
	    ;;
    esac
fi

if check_installed openssh; then
    systemctl_activate sshd.socket
    systemctl start sshdgenkeys
fi

shellvar_edit /etc/mkinitcpio.conf HOOKS base systemd autodetect modconf pcmcia block mdadm_udev keyboard sd-vconsole sd-encrypt sd-lvm2 filesystems fsck
if $changed; then
    echo "Generating initcpio..."
    q mkinitcpio -P > /dev/null
fi

if check_installed grub; then
    shellvar_edit /etc/default/grub GRUB_CMDLINE_LINUX_DEFAULT quiet
    shellvar_edit /etc/default/grub GRUB_CMDLINE_LINUX "$linuxcmdline"
    shellvar_edit /etc/default/grub GRUB_ENABLE_CRYPTODISK y
    shellvar_edit /etc/default/grub GRUB_DEFAULT saved
    shellvar_edit /etc/default/grub GRUB_SAVEDEFAULT true
    shellvar_edit /etc/default/grub GRUB_FORCE_HIDDEN_MENU true
    cat > /etc/grub.d/31_hold_shift <<'_EOF'
#!/bin/sh
set -e

prefix="/usr"
exec_prefix="${prefix}"
datarootdir="${prefix}/share"

export TEXTDOMAIN=grub
export TEXTDOMAINDIR="${datarootdir}/locale"
. "${datarootdir}/grub/grub-mkconfig_lib"

found_other_os=

make_timeout () {

  if [ "x${GRUB_FORCE_HIDDEN_MENU}" = "xtrue" ] ; then 
    if [ "x${1}" != "x" ] ; then
      if [ "x${GRUB_HIDDEN_TIMEOUT_QUIET}" = "xtrue" ] ; then
    verbose=
      else
    verbose=" --verbose"
      fi

      if [ "x${1}" = "x0" ] ; then
    cat <<EOF
if [ "x\${timeout}" != "x-1" ]; then
  if keystatus; then
    if keystatus --shift; then
      set timeout=-1
    else
      set timeout=0
    fi
  else
    if sleep$verbose --interruptible 3 ; then
      set timeout=0
    fi
  fi
fi
EOF
      else
    cat << EOF
if [ "x\${timeout}" != "x-1" ]; then
  if sleep$verbose --interruptible ${GRUB_HIDDEN_TIMEOUT} ; then
    set timeout=0
  fi
fi
EOF
      fi
    fi
  fi
}

adjust_timeout () {
  if [ "x$GRUB_BUTTON_CMOS_ADDRESS" != "x" ]; then
    cat <<EOF
if cmostest $GRUB_BUTTON_CMOS_ADDRESS ; then
EOF
    make_timeout "${GRUB_HIDDEN_TIMEOUT_BUTTON}" "${GRUB_TIMEOUT_BUTTON}"
    echo else
    make_timeout "${GRUB_HIDDEN_TIMEOUT}" "${GRUB_TIMEOUT}"
    echo fi
  else
    make_timeout "${GRUB_HIDDEN_TIMEOUT}" "${GRUB_TIMEOUT}"
  fi
}

  adjust_timeout

    cat <<EOF
if [ "x\${timeout}" != "x-1" ]; then
  if keystatus; then
    if keystatus --shift; then
      set timeout=-1
    else
      set timeout=0
    fi
  else
    if sleep$verbose --interruptible 3 ; then
      set timeout=0
    fi
  fi
fi
EOF
_EOF
    chmod a+x /etc/grub.d/31_hold_shift

    # install grub
    q grub-mkconfig -o /boot/grub/grub.cfg
    if [ -d /sys/firmware/efi/efivars ]; then
	installer efibootmgr
	q grub-install
    else
	q grub-install "$(lsblk -lpsno NAME "$(findmnt -no SOURCE /)" | tail -n 1)"
    fi
fi
