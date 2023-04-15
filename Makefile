
default:
	opam exec dune build

basys3:
	opam exec dune exec ./basys3.exe

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

${COMMON.MK}:
	curl --fail --location --show-error -o $@ https://raw.githubusercontent.com/chipsalliance/f4pga-examples/${F4PGA_VERSION}/common/$(notdir $@)

basys3.design: basys3

%.design: ${COMMON.MK}
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
