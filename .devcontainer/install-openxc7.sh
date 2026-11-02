#!/bin/bash
set -euo pipefail

export DEBIAN_FRONTEND=noninteractive
export INSTALL_PREFIX=/opt/openxc7
export JOBS=${JOBS:-4}
builder=/usr/local/share/openxc7/toolchain-sources-builder.sh
mkdir -p "$(dirname "$builder")"
curl -fLsS "https://raw.githubusercontent.com/openXC7/toolchain-installer/${OPENXC7_REV}/toolchain-sources-builder.sh" -o "$builder"
build_dir=$(mktemp -d)
cd "$build_dir"
bash "$builder" "$@"
if [ -d fpga-assembler ]; then
    (cd fpga-assembler && "$INSTALL_PREFIX/bin/bazel" shutdown)
fi
cd /
rm -rf "$build_dir" /root/.cache
apt-get clean
rm -rf /var/lib/apt/lists/*
