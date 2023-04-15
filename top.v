
module top (
    input clk,
    input btnc,
    input sw,
    output [3:0] anode,
    output [7:0] segment
);
  reg  powered = 1'b0;
  assign reset_ctrl = ~powered | btnc;

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
