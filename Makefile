
default:
	opam exec dune build

rtl:
	opam exec dune exec ./main.exe

format:
	opam exec dune fmt

ocaml_clean:
	opam exec dune $@
	rm -f main.v

utop:
	opam exec dune $@


OFL_BOARD.basys3=basys3
TARGET=basys3

BUILD_DIR=_build
COMMON.MK=${BUILD_DIR}/common.mk
F4PGA_VERSION=a5a44fa0c585783dcc1c6be32e88613be800287b

CURL=curl --fail --location --show-error -o
${COMMON.MK}:
	${CURL} $@ https://raw.githubusercontent.com/chipsalliance/f4pga-examples/${F4PGA_VERSION}/common/$(notdir $@)

DIGILENT_XDC_VERSION=69d35015d4c3a0cb384a964459593cea5260697a

${BUILD_DIR}/Arty-A7-35-Master.xdc:
	${CURL} $@ https://raw.githubusercontent.com/Digilent/digilent-xdc/${DIGILENT_XDC_VERSION}/$(notdir $@)

${BUILD_DIR}/arty_35.xdc: ${BUILD_DIR}/Arty-A7-35-Master.xdc
	cp $^ $@.tmp
	sed -i -E 's/#(.+)(CLK100MHZ)(.+)/\1\2\3/' $@.tmp
	mv $@.tmp $@

%.design: ${COMMON.MK} rtl
	rm -rf build
	set -eu;. ${F4PGA_INSTALL_DIR}/${FPGA_FAM}/conda/etc/profile.d/conda.sh;\
	 conda activate ${FPGA_FAM};${MAKE} -f ${COMMON.MK}\
	 TARGET=${TARGET} current_dir=${CURDIR} XDC=$(realpath $(basename $@).xdc)\
	  SOURCES='$(realpath $(basename $@).v $(wildcard ${BUILD_DIR}/*.v))' TOP=top

download:
	set -eux;file=build/basys3/top.bit;\
     . ${F4PGA_INSTALL_DIR}/${FPGA_FAM}/conda/etc/profile.d/conda.sh;\
     conda activate ${FPGA_FAM};\
     sudo $$(command -v openFPGALoader) -b $(OFL_BOARD.${TARGET}) $$file

.PHONY: default main clean
