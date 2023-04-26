
module top (
    input CLK100MHZ,
    input [3:0] btn,
    output led0_b,
    output led0_g,
    output led0_r,
);
  reg  powered = 1'b0;
  assign reset_ctrl = ~powered | btn[0];
  assign clk = CLK100MHZ;

  always @(posedge clk) begin
      powered <= 1'b1;
  end

  reset RESET (
      .clock(clk),
      .activate(reset_ctrl),
      .reset(reset)
  );

/*
  led_top TOP (
      .clock(clk),
      .reset_(reset),
      .blue(led0_b),
      .green(led0_g),
      .red(led0_r),
  );
*/

endmodule
