
module top (
    input clk,
    input btnC,
    input [15:0] sw,
    output [3:0] an,
    output [6:0] seg,
    output dp,
);
  reg  powered = 1'b0;
  assign reset_ctrl = ~powered | btnC;

  always @(posedge clk) begin
      powered <= 1'b1;
  end

  reset_top RESET (
      .activate(reset_ctrl),
      .clock(clk),
      .reset(reset)
  );

  clock TOP (
      .clock(clk),
      .reset(reset),
      .anode(an),
      .segment(seg),
      .dot(dp)
  );

endmodule
