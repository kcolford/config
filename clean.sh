#!/bin/sh
exec find "$(dirname "$0")" -name '*~' -o \( -empty -type d \) -delete
