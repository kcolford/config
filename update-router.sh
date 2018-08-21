#!/bin/bash
set -euo pipefail

ssh -J linux.student.cs.uwaterloo.ca,geezer.kcolford.com root@192.168.1.1 <<EOF
opkg update
opkg list-upgradable | cut -d ' ' -f 1 | xargs opkg upgrade
EOF
