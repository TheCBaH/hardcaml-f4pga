
default:
	opam exec dune build

main:
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

design: main
	rm -rf build
	set -eux;. ${F4PGA_INSTALL_DIR}/${FPGA_FAM}/conda/etc/profile.d/conda.sh;\
	 conda activate ${FPGA_FAM};${MAKE} -f ../f4pga-examples/common/common.mk\
	 TARGET=${TARGET} current_dir=${CURDIR} XDC=$(realpath ${TARGET}.xdc) SOURCES='${CURDIR}/*.v' TOP=top

download:
	set -eux;file=build/basys3/top.bit;\
     . ${F4PGA_INSTALL_DIR}/${FPGA_FAM}/conda/etc/profile.d/conda.sh;\
     conda activate ${FPGA_FAM};\
     sudo $$(command -v openFPGALoader) -b $(OFL_BOARD.${TARGET}) $$file

.PHONY: default main clean
