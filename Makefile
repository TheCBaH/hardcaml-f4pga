
default:
	opam exec dune build

rtl:
	opam exec dune exec ./main.exe

simulator:
	opam exec dune exec ./main.exe $@

format:
	opam exec dune fmt

ocaml_clean:
	opam exec dune $@
	rm -f main.v

utop:
	opam exec dune exec ./hardcaml.exe

OFL_BOARD.basys3=basys3
OFL_BOARD.arty_35=arty_a7_35t

BUILD_DIR=_build
COMMON.MK=${BUILD_DIR}/common.mk
F4PGA_VERSION=a5a44fa0c585783dcc1c6be32e88613be800287b

CURL=curl --fail --location --show-error -o
${COMMON.MK}:
	${CURL} $@ https://raw.githubusercontent.com/chipsalliance/f4pga-examples/${F4PGA_VERSION}/common/$(notdir $@)

DIGILENT_XDC_VERSION=69d35015d4c3a0cb384a964459593cea5260697a

${BUILD_DIR}/%-Master.xdc:
	${CURL} $@ https://raw.githubusercontent.com/Digilent/digilent-xdc/${DIGILENT_XDC_VERSION}/$(notdir $@)

activate_xdc=sed -i -E $(foreach v, $(1),-e 's/#(.+)($v)(.+)/\1\2\3/')

${BUILD_DIR}/arty_35.xdc: ${BUILD_DIR}/Arty-A7-35-Master.xdc
	cp $^ $@.tmp
	$(call activate_xdc, CLK100MHZ led0_? led1_? led2_? btn ) $@.tmp
	mv $@.tmp $@

${BUILD_DIR}/basys3.xdc: ${BUILD_DIR}/Basys-3-Master.xdc
	cp $^ $@.tmp
	$(call activate_xdc, clk seg dp an btnC sw) $@.tmp
	mv $@.tmp $@

%.design: ${BUILD_DIR}/%.xdc ${COMMON.MK} rtl
	echo $<
	rm -rf build
	set -eu;. ${F4PGA_INSTALL_DIR}/${FPGA_FAM}/conda/etc/profile.d/conda.sh;\
	 conda activate ${FPGA_FAM};${MAKE} -f ${COMMON.MK}\
	 TARGET=$(basename $@) current_dir=${CURDIR} XDC=$(realpath $<)\
	  SOURCES='$(realpath $(basename $@).v $(wildcard ${BUILD_DIR}/*.v))' TOP=top

design: $(addsuffix .design, arty_35 basys3)

%.download:
	set -eux;file=build/$(basename $@)/top.bit;\
     . ${F4PGA_INSTALL_DIR}/${FPGA_FAM}/conda/etc/profile.d/conda.sh;\
     conda activate ${FPGA_FAM};\
     sudo $$(command -v openFPGALoader) -b $(OFL_BOARD.$(basename $@)) $$file

.PHONY: default main clean
