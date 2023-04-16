
# Clock
 set_property -dict { PACKAGE_PIN W5 IOSTANDARD LVCMOS33 } [get_ports { clk }];
 create_clock -period 10.00 [get_ports {clk}];

# Buttons
 set_property -dict { PACKAGE_PIN U18 IOSTANDARD LVCMOS33 } [get_ports { btnC }];


# Switches
 set_property -dict { PACKAGE_PIN V17 IOSTANDARD LVCMOS33 } [get_ports { sw[0] }];


# Seven Segment Display
 set_property -dict { PACKAGE_PIN W7 IOSTANDARD LVCMOS33 } [get_ports { seg[0] }];
 set_property -dict { PACKAGE_PIN W6 IOSTANDARD LVCMOS33 } [get_ports { seg[1] }];
 set_property -dict { PACKAGE_PIN U8 IOSTANDARD LVCMOS33 } [get_ports { seg[2] }];
 set_property -dict { PACKAGE_PIN V8 IOSTANDARD LVCMOS33 } [get_ports { seg[3] }];
 set_property -dict { PACKAGE_PIN U5 IOSTANDARD LVCMOS33 } [get_ports { seg[4] }];
 set_property -dict { PACKAGE_PIN V5 IOSTANDARD LVCMOS33 } [get_ports { seg[5] }];
 set_property -dict { PACKAGE_PIN U7 IOSTANDARD LVCMOS33 } [get_ports { seg[6] }];
 set_property -dict { PACKAGE_PIN V7 IOSTANDARD LVCMOS33 } [get_ports { seg[7] }];
 set_property -dict { PACKAGE_PIN U2 IOSTANDARD LVCMOS33 } [get_ports { an[0] }];
 set_property -dict { PACKAGE_PIN U4 IOSTANDARD LVCMOS33 } [get_ports { an[1] }];
 set_property -dict { PACKAGE_PIN V4 IOSTANDARD LVCMOS33 } [get_ports { an[2] }];
 set_property -dict { PACKAGE_PIN W4 IOSTANDARD LVCMOS33 } [get_ports { an[3] }];