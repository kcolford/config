#!/bin/bash
set -euo pipefail

cat > /etc/pacman.conf <<EOF
[options]
HoldPkg = pacman glibc
Architecture = auto
Color
CheckSpace
SigLevel = Required DatabaseOptional
LocalFileSigLevel = Optional

[core]
# Include = /etc/pacman.d/pacserve
Include = /etc/pacman.d/mirrorlist

[extra]
# Include = /etc/pacman.d/pacserve
Include = /etc/pacman.d/mirrorlist

[community]
# Include = /etc/pacman.d/pacserve
Include = /etc/pacman.d/mirrorlist

[multilib]
# Include = /etc/pacman.d/pacserve
Include = /etc/pacman.d/mirrorlist

[xyne-any]
# Include = /etc/pacman.d/pacserve
Server = https://xyne.archlinux.ca/repos/xyne
EOF
if ! pacman -Q bauerbill > /dev/null 2>&1; then
    pacman -Syu bauerbill
fi
bb-wrapper --aur						\
	   --build-dir /home/${SUDO_USER}/.cache/bauerbill/	\
	   -Syu							\
	   --needed						\
	   base							\
	   base-devel						\
	   cronie						\
	   chronyd						\
	   networkmanager					\
	   network-manager-applet				\
	   pkgfile						\
	   pkgstats						\
	   pacserve						\
	   dash							\
	   etckeeper						\
	   borg							\
	   rsync						\
	   cups							\
	   ghostscript						\
	   foomatic-db						\
	   sane							\
	   ccid							\
	   hub							\
	   thefuck						\
	   watchman						\
	   linux-zen						\
	   linux-zen-headers					\
	   lightdm						\
	   lightdm-gtk-greeter					\
	   syncthing-gtk					\
	   qbittorrent						\
	   kdeconnect						\
	   dropbox						\
	   i3-gaps i3blocks i3lock i3status			\
	   xorg							\
	   xterm						\
	   compton						\
	   xss-lock						\
	   udiskie						\
	   feh							\
	   dunst						\
	   xdotool						\
	   dex							\
	   dmenu						\
	   terminux-font					\
	   arandr						\
	   redshift python-xdg					\
	   chromium						\
	   ttf-liberation					\
	   noto-fonts						\
	   libu2f-host						\
	   profile-sync-daemon					\
	   chromium-widevine					\
	   emacs						\
	   xclip						\
	   shellcheck						\
	   aspell						\
	   aspell-en						\
	   go-tools						\
	   flake8						\
	   autopep8						\
	   yapf							\
	   ipython						\
	   python-jedi						\
	   python-rope						\
	   python-virtualenv					\
	   prettier						\
	   cmake						\
	   clang						\
	   bear							\
	   gocode-git

mkdir -p /etc/pacman.d/hooks/
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
cat > /etc/vconsole.conf <<EOF
KEYMAP=us
FONT=Lat2-Terminus16
EOF
cat > /etc/locale.gen <<EOF
en_CA.UTF-8 UTF-8
en_US.UTF-8 UTF-8
EOF
cat > /etc/locale.conf <<EOF
LANG=en_CA.UTF-8
EOF
cat > /etc/sudoers.d/config <<EOF
%wheel ALL=(ALL:ALL) ALL
ALL ALL=(ALL) NOPASSWD: /usr/bin/psd-overlay-helper 
EOF
chmod 440 /etc/sudoers.d/config
cat > /etc/X11/xorg.conf.d/30-touchpad.conf <<EOF
Section "InputClass"
	Identifier "devname"
	Driver "libinput"
	MatchIsTouchpad "on"
	Option "NaturalScrolling" "true"
EndSection
EOF

systemctl enable --now chronyd NetworkManager pacserv pcscd org.cups.cupsd pkgfile-update.timer cronie lightdm

locale-gen	   
