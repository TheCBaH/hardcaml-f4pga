ARG VARIANT=12
FROM debian:${VARIANT}-slim AS openxc7
RUN apt-get update && apt-get install -y --no-install-recommends ca-certificates curl \
    && rm -rf /var/lib/apt/lists/*
ARG TARGETARCH
ARG OPENXC7_VERSION=v0.0.1
COPY openxc7.sha256 /tmp/openxc7.sha256
RUN set -eu; \
    case "$TARGETARCH" in amd64) arch=x86_64 ;; arm64) arch=aarch64 ;; *) exit 1 ;; esac; \
    archive="openxc7-linux-$arch-musl.tar.gz"; \
    grep -F "  $archive" /tmp/openxc7.sha256 > /tmp/checksums; \
    cd /tmp; \
    curl -fLsS --retry 3 "https://github.com/TheCBaH/devcontainer.openxc7/releases/download/$OPENXC7_VERSION/$archive" -o "$archive"; \
    sha256sum -c /tmp/checksums; \
    mkdir -p /opt/openxc7; \
    tar -xzf "$archive" -C /opt/openxc7

FROM debian:${VARIANT}
RUN set -eux;\
    apt-get update;\
    env DEBIAN_FRONTEND=noninteractive apt-get install -y --no-install-recommends\
  ca-certificates\
  curl\
  git\
  iverilog\
  make\
  openfpgaloader\
  picocom\
  verilator\
  wget\
  xz-utils\
;\
    apt-get autoremove -y; apt-get clean -y ; apt-get clean; rm -rf /var/lib/apt/lists/*;\
    echo DONE

COPY --from=openxc7 /opt/openxc7 /opt/openxc7

ENV PATH="/opt/openxc7/bin:${PATH}" \
    PRJXRAY_DB_DIR=/opt/openxc7/share/nextpnr/prjxray-db \
    OPENXC7_CHIPDB=/opt/openxc7/share/chipdb \
    DIGILENT_XDC_DIR=/opt/openxc7/share/digilent-xdc

ARG DIGILENT_XDC_REV=69d35015d4c3a0cb384a964459593cea5260697a
RUN set -eu; mkdir -p "$DIGILENT_XDC_DIR" \
    && for board in Basys-3 Arty-A7-35; do \
       curl -fLsS "https://raw.githubusercontent.com/Digilent/digilent-xdc/${DIGILENT_XDC_REV}/${board}-Master.xdc" \
       -o "$DIGILENT_XDC_DIR/${board}-Master.xdc"; done
