#!/bin/sh
set -eu
set -x

echo "Activating feature 'F4FPGA'"

USERNAME="${USERNAME:-"${_REMOTE_USER:-"automatic"}"}"
UPDATE_RC="${UPDATE_RC:-"true"}"

# Determine the appropriate non-root user
if [ "${USERNAME}" = "auto" ] || [ "${USERNAME}" = "automatic" ]; then
    USERNAME=""
    POSSIBLE_USERS="vscode node codespace $(awk -v val=1000 -F ":" '$3==val{print $1}' /etc/passwd)"
    for CURRENT_USER in $POSSIBLE_USERS; do
        if id -u "${CURRENT_USER}" > /dev/null 2>&1; then
            USERNAME="${CURRENT_USER}"
            break
        fi
    done
    if [ "${USERNAME}" = "" ]; then
        USERNAME=root
    fi
elif [ "${USERNAME}" = "none" ] || ! id -u ${USERNAME} > /dev/null 2>&1; then
    USERNAME=root
fi

TARGET=${TARGET:-arty_35}
case "$TARGET" in
arty_35|basys3)
    FPGA_FAM='xc7'
    PACKAGES='xc7a50t_test'
    ;;
fc7)
    FPGA_FAM="$TARGET"
    PACKAGES='xc7a50t_test xc7a100t_test xc7a200t_test xc7z010_test'
    ;;
*)
    echo "Not supported TARGET=${TARGET}" >&2
    exit 1
    ;;
esac
F4PGA_INSTALL_DIR=${1:-/opt/f4pga}
FPGA_FAM=${FPGA_FAM:-xc7}

updaterc() {
    if [ "${UPDATE_RC}" = "true" ]; then
        echo "Updating /etc/bash.bashrc and /etc/zsh/zshrc..."
        if [ -f /etc/bash.bashrc ]; then
            /bin/echo -e "$1" >> /etc/bash.bashrc
        fi
        if [ -f "/etc/zsh/zshrc" ]; then
            /bin/echo -e "$1" >> /etc/zsh/zshrc
        fi
    fi
}

do_curl() {
    curl --fail --location --show-error $@
}

do_curl -o /tmp/conda.sh https://repo.continuum.io/miniconda/Miniconda3-latest-Linux-x86_64.sh
commit='5ec56e796afae5848e4c805a056d438566e3b075'
environment="$FPGA_FAM/environment.yml"
url="https://raw.githubusercontent.com/chipsalliance/f4pga-examples/$commit/"
files="$environment common/requirements.txt $FPGA_FAM/requirements.txt"
for f in $files; do
    fout="/tmp/$f"
    mkdir -p $(dirname $fout)
    do_curl -o $fout "$url/$f"
done
conda_dir="$F4PGA_INSTALL_DIR/$FPGA_FAM/conda"
bash /tmp/conda.sh -u -b -p $conda_dir

rc="$(cat << EOF
# >>> F4FPGA >>>
export F4PGA_INSTALL_DIR="$F4PGA_INSTALL_DIR"
export FPGA_FAM="$FPGA_FAM"
export TARGET="$TARGET"
#. "$conda_dir/etc/profile.d/conda.sh"
# conda activate "$FPGA_FAM"
# <<< F4FPGA <<<
EOF
)"
updaterc "$rc"

. "$conda_dir/etc/profile.d/conda.sh"
conda env create -f /tmp/$environment
for f in $files; do
    rm -f "/tmp/$f"
done
mkdir -p $F4PGA_INSTALL_DIR/$FPGA_FAM
F4PGA_TIMESTAMP='20220920-124259'
F4PGA_HASH='007d1c1'
F4PGA_PACKAGES="install-xc7 $PACKAGES"
for PKG in $F4PGA_PACKAGES; do
    f="symbiflow-arch-defs-${PKG}-${F4PGA_HASH}.tar.xz"
    fname="/tmp/$f"
    do_curl -o $fname https://storage.googleapis.com/symbiflow-arch-defs/artifacts/prod/foss-fpga-tools/symbiflow-arch-defs/continuous/install/$F4PGA_TIMESTAMP/$f
    tar -f $fname -xJC $F4PGA_INSTALL_DIR/${FPGA_FAM}
    rm -f $fname
done

conda clean --all --force-pkgs-dirs
chown -R "${USERNAME}" $conda_dir
