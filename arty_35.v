
module top (
    input CLK100MHZ,
    input btn,
    output led0_b,
    output led0_g,
    output led0_r,
);
  reg  powered = 1'b0;
  assign reset_ctrl = ~powered | btn[0];

  always @(posedge clk) begin
      powered <= 1'b1;
  end

  reset RESET (
      .clock(clk),
      .activate(reset_ctrl),
      .reset(reset)
  );

  led TOP (
      .clock(clk),
      .reset(reset),
      .led0_b(led0_b),
      .led0_g(led0_g),
      .led0_r(led0_r),
  );

endmodule
