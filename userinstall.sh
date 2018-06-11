#!/bin/bash
set -euo pipefail

SSH=9034@usw-s009.rsync.net

if [ "$UID" = 0 ]; then
    pacman -S --needed sudo moreutils
    visudo -c
    sponge /etc/sudoers.d/config <<EOF
%wheel ALL=(ALL:ALL) ALL
ALL ALL=(ALL) NOPASSWD: /usr/bin/psd-overlay-helper
EOF
    chmod 440 /etc/sudoers.d/config
    if ! visudo -c; then
	rm /etc/sudoers.d/config
    fi
    if [ "$1" ]; then
	useradd -m -G wheel "$1" || true
    fi
    exit 0
fi

sudo pacman -S --needed moreutils

mkdir -p ~/.cache
btrfs subvolume create ~/.cache/bauerbill/ || true

sudo sponge /etc/pacman.conf <<EOF
[options]
HoldPkg = pacman glibc
Architecture = auto
Color
CheckSpace
SigLevel = Required DatabaseOptional
LocalFileSigLevel = Optional

[core]
Include = /etc/pacman.d/mirrorlist

[extra]
Include = /etc/pacman.d/mirrorlist

[community]
Include = /etc/pacman.d/mirrorlist

[multilib]
Include = /etc/pacman.d/mirrorlist

[xyne-any]
Server = https://xyne.archlinux.ca/repos/xyne
EOF
if ! pacman -Q bauerbill > /dev/null 2>&1; then
    sudo pacman -S bauerbill 
fi
bb-wrapper --aur				\
	   --build-dir ~/.cache/bauerbill/	\
	   -Syu					\
	   --needed				\
	   base					\
	   base-devel				\
	   bash-completion			\
	   grub					\
	   btrfs-progs				\
	   moreutils				\
	   sshfs				\
	   cronie				\
	   chronyd				\
	   networkmanager			\
	   network-manager-applet		\
	   pkgfile				\
	   pkgstats				\
	   pacserve				\
	   dash					\
	   borg python-llfuse			\
	   rsync				\
	   cups					\
	   ghostscript				\
	   foomatic-db				\
	   sane					\
	   ccid					\
	   hub					\
	   thefuck				\
	   watchman				\
	   linux-zen linux-zen-headers		\
	   lightdm				\
	   lightdm-gtk-greeter			\
	   syncthing-gtk			\
	   qbittorrent				\
	   kdeconnect				\
	   dropbox				\
	   i3-gaps i3blocks i3lock i3status	\
	   xorg					\
	   xterm				\
	   compton				\
	   xss-lock				\
	   udiskie				\
	   feh					\
	   dunst				\
	   xdotool				\
	   dex					\
	   dmenu				\
	   terminux-font			\
	   arandr				\
	   redshift python-xdg			\
	   chromium				\
	   ttf-liberation			\
	   noto-fonts				\
	   libu2f-host				\
	   profile-sync-daemon			\
	   chromium-widevine			\
	   emacs				\
	   xclip				\
	   shellcheck				\
	   aspell				\
	   aspell-en				\
	   go-tools				\
	   flake8				\
	   autopep8				\
	   yapf					\
	   ipython				\
	   python-jedi				\
	   python-rope				\
	   python-virtualenv			\
	   prettier				\
	   cmake				\
	   clang				\
	   bear					\
	   gocode-git				\
	   openssh				\
	   snapper

# setup pacserve
pacman.conf-insert_pacserve | sudo sponge /etc/pacman.conf

# configure the package cache
if ! btrfs subvolume show /var/cache/pacman/pkg > /dev/null 2>&2; then
    sudo rm -r /var/cache/pacman/pkg
    sudo btrfs subvolume create /var/cache/pacman/pkg
fi
sudo mkdir -p /etc/pacman.d/hooks/
sudo sponge cat /etc/pacman.d/hooks/paccache.hook <<EOF
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

# adjust user controls
sudo sed -i '/CHFN_RESTRICT/s/\brwh/frwh/' /etc/login.defs
sudo sponge /etc/profile.d/umask.sh <<EOF
umask 002
EOF

# locale and 
sudo sponge /etc/vconsole.conf <<EOF
KEYMAP=us
FONT=Lat2-Terminus16
EOF
sudo sponge /etc/locale.gen <<EOF
en_CA.UTF-8 UTF-8
en_US.UTF-8 UTF-8
EOF
sudo locale-gen
sudo sponge /etc/locale.conf <<EOF
LANG=en_CA.UTF-8
EOF
sudo ln -sf /usr/share/zoneinfo/America/Toronto /etc/localtime

# use natural scrolling with a touchpad
sudo sponge /etc/X11/xorg.conf.d/30-touchpad.conf <<EOF
Section "InputClass"
	Identifier "devname"
	Driver "libinput"
	MatchIsTouchpad "on"
	Option "NaturalScrolling" "true"
EndSection
EOF

# setup virtual machine images, these should be nocow
mkdir -p ~/.local/share/libvirt/images
chattr +C ~/.local/share/libvirt/images
mkdir -p ~/VirtualBox\ VMs
chattr +C ~/VirtualBox\ VMs

# setup backup programs
sudo sponge /etc/systemd/system/borg@.service <<EOF
[Unit]
Description=Run the borg backup program.

[Service]
ExecStart=/usr/bin/borg create ::{hostname}-%i-{now} .
WorkingDirectory=%I
EnvironmentFile=/etc/conf.d/borg
EOF
sudo sponge /etc/systemd/system/borg@.timer <<EOF
[Unit]
Description=Regularly perform a borg backup

[Timer]
OnCalendar=daily
AccuracySec=12h
Persistent=true

[Install]
WantedBy=multi-user.target
EOF

# start a bunch of services for an interactive machine
sudo systemctl enable chronyd NetworkManager pacserv pcscd org.cups.cupsd pkgfile-update.timer cronie lightdm

# setup ssh keys for borg
yes '' | sudo ssh-keygen || true
sudo cp /root/.ssh/id_rsa.pub /etc/ssh/
mkdir -p /tmp/mnt
sshfs $SSH: /tmp/mnt
echo "restrict,command=\"borg serve --restrict-to-path $(hostname)\"" $(cat /etc/ssh/id_rsa.pub) | sponge -a /tmp/mnt/.ssh/authorized_keys
fusermount -u /tmp/mnt
yes '' | sudo borg init -e repokey "ssh://$SSH/~/$(hostname)"
sudo tee /etc/conf.d/borg <<EOF
BORG_REPO=ssh://$SSH/~/$(hostname)
BORG_PASSPHRASE=
EOF
sudo chmod 600 /etc/conf.d/borg

# update grub
sudo grub-mkconfig -o /boot/grub/grub.cfg

# setup snapper
sudo snapper -c root create-config / || true
if [ -d /.snapshots/ ]; then
    sudo systemctl enable borg@-.snapshots-.timer
fi

# setup cups
sudo sed -i '/^SystemGroup/s/sys root$/sys root wheel/' /etc/cups/cups-files.conf

# setup grub
sudo sed -i 's/#\? *\(GRUB_ENABLE_CRYPTODISK=\).*/\1=y/' /etc/default/grub

# initcpio
(
    source /etc/mkinitcpio.conf
    sudo tee /etc/mkinitcpio.conf <<EOF
MODULES=(${MODULES[@]})
BINARIES=(${BINARIES[@]})
FILES=(${FILES[@]})
HOOKS=(base systemd autodetect modconf pcmcia block mdadm_udev keyboard sd-vconsole sd-encrypt sd-lvm2 filesystems fsck)
COMPRESSION="lz4"
EOF
)
sudo mkinitcpio -P

