
default:
	opam exec dune build

RTL_FILES=_build/clock.v _build/reset_top.v _build/led_top.v _build/eater_top.v

rtl: $(RTL_FILES)

$(RTL_FILES) &: main.ml dune dune-project src/dune $(wildcard src/*.ml src/*.mli)
	opam exec dune exec ./main.exe

simulator:
	opam exec dune exec ./main.exe $@

format:
	opam exec dune fmt

runtest:
	opam exec -- dune $@ --auto-promote

ocaml_clean:
	opam exec dune $@
	rm -f main.v

utop:
	opam exec dune exec ./hardcaml.exe

OFL_BOARD.basys3=basys3
OFL_BOARD.arty_35=arty_a7_35t

BUILD_DIR=_build
OPENXC7_CHIPDB?=/opt/openxc7/share/chipdb
PRJXRAY_DB_DIR?=/opt/openxc7/share/nextpnr/prjxray-db
DIGILENT_XDC_DIR?=/opt/openxc7/share/digilent-xdc
PART.basys3=xc7a35tcpg236-1
PART.arty_35=xc7a35tcsg324-1

${BUILD_DIR}/%-Master.xdc: $(DIGILENT_XDC_DIR)/%-Master.xdc
	mkdir -p $(@D)
	cp $< $@

activate_xdc=sed -i -E $(foreach v, $(1),-e 's/#(.+)($v)(.+)/\1\2\3/')
# nextpnr accepts a subset of Vivado's XDC syntax.
normalize_xdc=sed -i -E -e 's/\#.*//' -e 's/;[[:space:]]*$$//' -e 's/\[get_ports[[:space:]]+\{[[:space:]]*([^{}[:space:]]+)[[:space:]]*\}\]/[get_ports {\1}]/g'

${BUILD_DIR}/arty_35.xdc: ${BUILD_DIR}/Arty-A7-35-Master.xdc Makefile
	cp $< $@.tmp
	$(call activate_xdc, CLK100MHZ led0_? led1_? led2_? btn ) $@.tmp
	$(normalize_xdc) $@.tmp
	mv $@.tmp $@

${BUILD_DIR}/basys3.xdc: ${BUILD_DIR}/Basys-3-Master.xdc Makefile
	cp $< $@.tmp
	$(call activate_xdc, clk seg dp an btnC sw) $@.tmp
	$(normalize_xdc) $@.tmp
	mv $@.tmp $@

build/%/top.json: %.v $(RTL_FILES) $(wildcard $(BUILD_DIR)/*.v) Makefile
	mkdir -p $(@D)
	yosys -ql $(@D)/synth.log -p 'synth_xilinx -flatten -abc9 -arch xc7 -top top; write_json $@' $< $(wildcard $(BUILD_DIR)/*.v)

build/%/top.fasm: build/%/top.json ${BUILD_DIR}/%.xdc $(OPENXC7_CHIPDB)/xc7a50t.bin $(OPENXC7_CHIPDB)/xc7a50t.delay
	nextpnr-himbaechel --device $(PART.$*) --chipdb $(OPENXC7_CHIPDB)/xc7a50t.bin \
	 -o xdc=$(word 2,$^) --json $< -o fasm=$@ --freq 100 \
	 -o delay-matrix=$(OPENXC7_CHIPDB)/xc7a50t.delay \
	 --log $(@D)/pnr.log --report $(@D)/timing.json

build/%/top.bit: build/%/top.fasm
	fpga-as --prjxray_db_path=$(PRJXRAY_DB_DIR)/artix7 --part $(PART.$*) $< > $@

%.design: build/%/top.bit
	@test -s $<

design: $(addsuffix .design, arty_35 basys3)

%.download: build/%/top.bit
	sudo $$(command -v openFPGALoader) -b $(OFL_BOARD.$*) $<

.DELETE_ON_ERROR:
.SECONDARY:
.PHONY: default rtl design simulator format runtest ocaml_clean utop
