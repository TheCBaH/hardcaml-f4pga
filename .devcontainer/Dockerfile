ARG VARIANT=12
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

ARG OPENXC7_REV=efe9e07330e88434a831f2c52bee72739da0f0a5
ARG JOBS=4
COPY install-openxc7.sh /usr/local/bin/install-openxc7
RUN install-openxc7 yosys
RUN install-openxc7 nextpnr
RUN install-openxc7 fpga-as
RUN ln -s /opt/openxc7/export.sh /etc/profile.d/openxc7.sh

ENV PATH="/opt/openxc7/venv/bin:/opt/openxc7/bin:${PATH}" \
    NEXTPNR_XILINX_DIR=/opt/openxc7 \
    PRJXRAY_DB_DIR=/opt/openxc7/share/nextpnr/prjxray-db \
    OPENXC7_CHIPDB=/opt/openxc7/share/chipdb \
    DIGILENT_XDC_DIR=/opt/openxc7/share/digilent-xdc

COPY prepare-xc7.sh /usr/local/bin/prepare-xc7
RUN prepare-xc7

ARG DIGILENT_XDC_REV=69d35015d4c3a0cb384a964459593cea5260697a
RUN set -eu; mkdir -p "$DIGILENT_XDC_DIR" \
    && for board in Basys-3 Arty-A7-35; do \
       curl -fLsS "https://raw.githubusercontent.com/Digilent/digilent-xdc/${DIGILENT_XDC_REV}/${board}-Master.xdc" \
       -o "$DIGILENT_XDC_DIR/${board}-Master.xdc"; done
