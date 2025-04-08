open Hardcaml
open Eater8bit

let%expect_test "register_test" =
  let scope = Scope.create ~flatten_design:true () in
  let module Simulator = Cyclesim.With_interface (Register.I) (Register.O) in
  let waves, sim =
    Register.create scope |> Simulator.create ~config:Cyclesim.Config.trace_all |> Hardcaml_waveterm.Waveform.create
  in
  let inputs = Cyclesim.inputs sim in
  let set wire = wire := Bits.vdd in
  let clear wire = wire := Bits.gnd in
  let data v = inputs.w_data := Bits.of_int ~width:Register.bits v in
  let cycles n =
    for _ = 1 to n do
      Cyclesim.cycle sim
    done
  in
  cycles 1;
  data 1;
  set inputs.w_en;
  cycles 1;
  clear inputs.w_en;
  data 2;
  cycles 2;
  set inputs.w_en;
  cycles 2;
  set inputs.reset;
  cycles 2;
  clear inputs.reset;
  for i = 4 to 8 do
    data i;
    cycles 1
  done;
  cycles 1;
  Hardcaml_waveterm.Waveform.print ~display_height:18 ~display_width:80 ~wave_width:1 waves;
  [%expect
    {|
      ┌Signals───────────┐┌Waves─────────────────────────────────────────────────────┐
      │clock             ││┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─│
      │                  ││  └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ │
      │reset             ││                                                          │
      │                  ││────────────────────────────────────────────────────────  │
      │                  ││────┬───┬───────────────────────┬───┬───┬───┬───┬───────  │
      │w_data            ││ 00 │01 │02                     │04 │05 │06 │07 │08       │
      │                  ││────┴───┴───────────────────────┴───┴───┴───┴───┴───────  │
      │w_en              ││    ┌───┐       ┌───────────────────────────────────────  │
      │                  ││────┘   └───────┘                                         │
      │                  ││────────┬───────────┬───────┬───────┬───┬───┬───┬───┬───  │
      │data              ││ 00     │01         │02     │00     │04 │05 │06 │07 │08   │
      │                  ││────────┴───────────┴───────┴───────┴───┴───┴───┴───┴───  │
      │                  ││────────┬───────────┬───────┬───────┬───┬───┬───┬───┬───  │
      │register          ││ 00     │01         │02     │00     │04 │05 │06 │07 │08   │
      │                  ││────────┴───────────┴───────┴───────┴───┴───┴───┴───┴───  │
      │                  ││                                                          │
      └──────────────────┘└──────────────────────────────────────────────────────────┘
   |}]
