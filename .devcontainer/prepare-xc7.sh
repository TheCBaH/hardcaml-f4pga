#!/bin/bash
set -euo pipefail

build_dir=$(mktemp -d)
trap 'rm -rf "$build_dir"' EXIT
cd "$build_dir"
mkdir -p "$OPENXC7_CHIPDB"
python3 "$NEXTPNR_XILINX_DIR/share/nextpnr/himbaechel/uarch/xilinx/gen/xilinx_gen.py" \
    --xray "$PRJXRAY_DB_DIR/artix7" --device xc7a50t --bba chipdb.bba
bbasm -l chipdb.bba "$OPENXC7_CHIPDB/xc7a50t.bin"
rm chipdb.bba

cat > image_check.v <<'VERILOG'
module image_check(input clk, output q);
    reg [3:0] count = 0;
    always @(posedge clk) count <= count + 1;
    assign q = count[3];
endmodule
VERILOG
yosys -Q -p 'synth_xilinx -flatten -abc9 -arch xc7 -top image_check; write_json image_check.json' image_check.v
for package in csg324 cpg236; do
    part=xc7a35t$package
    ln -s xc7a50t.bin "$OPENXC7_CHIPDB/$part.bin"
    case "$package" in
        csg324) clock_pin=E3; output_pin=H5 ;;
        cpg236) clock_pin=W5; output_pin=U16 ;;
    esac
    cat > image_check.xdc <<XDC
set_property PACKAGE_PIN $clock_pin [get_ports clk]
set_property IOSTANDARD LVCMOS33 [get_ports clk]
set_property PACKAGE_PIN $output_pin [get_ports q]
set_property IOSTANDARD LVCMOS33 [get_ports q]
create_clock -period 10.0 [get_ports clk]
XDC
    nextpnr-xilinx --chipdb "$OPENXC7_CHIPDB/$part.bin" --xdc image_check.xdc \
        --json image_check.json --fasm image_check.fasm --freq 100 \
        -o "delay-matrix=$OPENXC7_CHIPDB/xc7a50t.delay"
    fpga-as --prjxray_db_path="$PRJXRAY_DB_DIR/artix7" --part "$part-1" image_check.fasm > image_check.bit
    test -s image_check.bit
done
test -s "$OPENXC7_CHIPDB/xc7a50t.delay"
