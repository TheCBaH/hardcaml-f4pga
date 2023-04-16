
module top (
    input clk,
    input btnC,
    input [15:0] sw,
    output [3:0] an,
    output [7:0] seg
);
  reg  powered = 1'b0;
  assign reset_ctrl = ~powered | btnC;

  always @(posedge clk) begin
      powered <= 1'b1;
  end

  reset RESET (
      .clock(clk),
      .activate(reset_ctrl),
      .reset(reset)
  );

  clock TOP (
      .clock(clk),
      .reset(reset),
      .anode(anode),
      .segment(segment)
  );

endmodule
