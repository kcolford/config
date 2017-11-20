. ~/.profile
. ~/.bashrc
if command -v systemctl > /dev/null 2>&1; then
    systemctl --user import-environment PATH
fi
