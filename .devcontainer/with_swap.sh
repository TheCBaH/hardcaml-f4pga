#!/bin/bash
set -eu
set -x
swap=/tmp/swapfile
cleanup() {
    rc=$?
    if [ -n "$swap" ]; then
        sudo swapoff $swap || true
        sudo rm -rf $swap || true
    fi
    exit $rc
}
trap cleanup EXIT
fallocate -l 4G $swap
sudo chmod 600 $swap
mkswap $swap
sudo chown 0:0 $swap
sudo swapon $swap
cmd="$@"
if [ -n "$cmd" ]; then
    "$@"
else
    swap=''
fi
