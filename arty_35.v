
module top (
    input CLK100MHZ,
    input [3:0] btn,
    output led0_b,
    output led0_g,
    output led0_r,
    output led1_b,
    output led1_g,
    output led1_r,
    output led2_b,
    output led2_g,
    output led2_r,
);
  reg  powered = 1'b0;
  assign reset_ctrl = ~powered | btn[0];
  assign clk = CLK100MHZ;

  always @(posedge clk) begin
      powered <= 1'b1;
  end

  reset_top RESET (
      .activate(reset_ctrl),
      .clock(clk),
      .reset(reset)
  );

  led_top TOP (
      .clock(clk),
      .reset_(reset),
      .blue_0(led0_b),
      .green_0(led0_g),
      .red_0(led0_r),
      .blue_1(led1_b),
      .green_1(led1_g),
      .red_1(led1_r),
      .blue_2(led2_b),
      .green_2(led2_g),
      .red_2(led2_r),
  );

endmodule
