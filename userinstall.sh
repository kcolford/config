#!/bin/bash
set -euo pipefail

# This script sets up a user system.
#
# It tries to make all file modifications atomic by using sponge from
# moreutils. If it has been run as root then it sets up sudo and if
# the script was given an argument it creates a user with a name based
# on that argument and makes that user the administrator with sudo. It
# also adds helper scripts that convert directories into subvolumes.
# It installs a pacman.conf file that has access to a number of
# repositories. It installs bauerbill as an AUR helper and configures
# it to use the local network for speeding up downloads. It uses
# bauerbill to install all packages that may be useful to a user. It
# sets up the package cache to clean itself regularly and exclude it
# from backups. It sets up a more user friendly permission system. It
# configures locales, fonts, and timezones. It configures natural
# scrolling on touchpads.

SSH=9034@usw-s009.rsync.net
dda='dd conv=notrunc oflag=append'
tee=tee

if [ "$UID" = 0 ]; then
    pacman -S --needed sudo moreutils && tee=sponge
    visudo -c
    $tee /etc/sudoers.d/config <<'EOF'
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

sudo pacman -S --needed moreutils && tee=sponge

sudo $tee /usr/local/bin/btrfs-convert-to-subvolume <<'EOF'
#!/bin/bash
set -euo pipefail

if btrfs subvolume show "$1" > /dev/null 2>&1; then
    exit 0
fi

mkdir -p "$1"
btrfs subvolume create "$1"/../.tmp
find "$1" -mindepth 1 -maxdepth 1 -print0 | xargs -0 mv -t "$1"/../.tmp
rmdir "$1" || btrfs subvolume delete "$1"
mv "$(realpath -m "$1"/../.tmp)" "$1"
EOF
sudo chmod +x /usr/local/bin/btrfs-convert-to-subvolume

# exclude the bauerbill cache from being backed up
btrfs-convert-to-subvolume ~/.cache/bauerbill/

# setup pacman to our liking
sudo $tee /etc/pacman.conf <<'EOF'
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
sudo pacman -S --needed bauerbill
chattr +C /var/cache/pacman/pkg/
sudo sed -i '/"server":/s|null|"http://localhost:15678"|' /etc/powerpill/powerpill.json
bb-wrapper --aur				\
	   --build-dir ~/.cache/bauerbill/	\
	   -Syu					\
	   --needed				\
	   base					\
	   base-devel				\
	   bash-completion			\
	   pacman-contrib			\
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
	   scrot				\
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
	   openssh

# configure the package cache
sudo btrfs-convert-to-subvolume /var/cache/pacman/pkg
sudo mkdir -p /etc/pacman.d/hooks/
sudo $tee /etc/pacman.d/hooks/paccache.hook <<'EOF'
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
sudo $tee /etc/profile.d/umask.sh <<'EOF'
umask 002
EOF

# locale and fonts
sudo $tee /etc/vconsole.conf <<'EOF'
KEYMAP=us
FONT=Lat2-Terminus16
EOF
sudo $tee /etc/locale.gen <<'EOF'
en_CA.UTF-8 UTF-8
en_US.UTF-8 UTF-8
EOF
sudo locale-gen
sudo $tee /etc/locale.conf <<'EOF'
LANG=en_CA.UTF-8
EOF
sudo ln -sf /usr/share/zoneinfo/America/Toronto /etc/localtime

# use natural scrolling with a touchpad
sudo $tee /etc/X11/xorg.conf.d/30-touchpad.conf <<'EOF'
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

# setup backups
sudo $tee /etc/systemd/system/borg@.service <<'EOF'
[Unit]
Description=Run the borg backup program.

[Service]
Type=oneshot
ExecStart=-/usr/bin/btrfs subvolume delete %I/.backup
ExecStart=-/usr/bin/rmdir %I/.backup
ExecStart=/usr/bin/btrfs subvolume snapshot -r %I %I/.backup
ExecStart=/usr/bin/borg create ::{hostname}-%i-{now} %I/.backup
EnvironmentFile=/etc/conf.d/borg
EOF
sudo $tee /etc/systemd/system/borg@.timer <<'EOF'
[Unit]
Description=Regularly perform a borg backup

[Timer]
OnCalendar=daily
AccuracySec=12h
Persistent=true

[Install]
WantedBy=multi-user.target
EOF
yes '' | sudo ssh-keygen || true
authorized_key_line="restrict,command=\"borg serve --restrict-to-path $(hostname)\" $(sudo cat /root/.ssh/id_rsa.pub)"
echo "$authorized_key_line" | ssh "$SSH" $dda of=.ssh/authorized_keys
sudo $tee /etc/conf.d/borg <<EOF
BORG_REPO=$SSH:$(hostname)
BORG_PASSPHRASE=
EOF
sudo btrfs-convert-to-subvolume /root/.cache/borg
sudo borg init -e repokey "$SSH:$(hostname)" || true

# start a bunch of services for an interactive machine
sudo systemctl daemon-reload
sudo systemctl enable chronyd NetworkManager pacserve pcscd org.cups.cupsd pkgfile-update.timer cronie lightdm fstrim.timer

# enable root backups
if sudo btrfs subvolume show / > /dev/null 2>&1; then
    sudo systemctl enable borg@-.timer
fi

# update grub
sudo grub-mkconfig -o /boot/grub/grub.cfg

# setup cups
sudo sed -i '/^SystemGroup/s/sys root$/sys root wheel/' /etc/cups/cups-files.conf

# setup grub
sudo sed -i 's/#\? *\(GRUB_ENABLE_CRYPTODISK=\).*/\1=y/' /etc/default/grub

# hide the initramfses
chmod o-r /boot/initramfs-*.img || true

# initcpio
(
    source /etc/mkinitcpio.conf
    sudo $tee /etc/mkinitcpio.conf <<EOF
# make sure new initramfses are hidden
umask o-r
MODULES=(${MODULES[@]})
BINARIES=(${BINARIES[@]})
FILES=(${FILES[@]})
HOOKS=(base systemd autodetect modconf pcmcia block mdadm_udev keyboard sd-vconsole sd-encrypt sd-lvm2 filesystems fsck)
COMPRESSION="lz4"
EOF
)
sudo mkinitcpio -P

