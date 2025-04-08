open Hardcaml
open Clock

let%expect_test "display_test" =
  let scope = Scope.create ~flatten_design:true () in
  let _digit_0 = "digit_0" in
  let _digit_1 = "digit_1" in
  let _dot = "dot" in
  let _enable = "enable" in
  let _clock = "clock" in
  let _reset = "reset" in
  let _segment = "segment" in
  let _anode = "anode" in
  let digit_0 = Signal.input _digit_0 4 in
  let digit_1 = Signal.input _digit_1 4 in
  let clock = Signal.input _clock 1 in
  let enable = Signal.input _enable 1 in
  let dot = Signal.input _dot 1 in
  let digits = [ { data = digit_0; enable; dot }; { data = digit_1; enable; dot } ] in
  let reset = Signal.input _reset 1 in
  let anode, segment, dot = display scope ~clock ~digits ~reset ~next:enable in
  let circuit =
    Circuit.create_exn ~name:"display"
      [ Signal.output _anode anode; Signal.output _segment segment; Signal.output "_dot" dot ]
  in
  let waves, sim = Hardcaml_waveterm.Waveform.create (Cyclesim.create circuit) in
  let set wire = Cyclesim.in_port sim wire := Bits.vdd in
  (* let clear wire = Cyclesim.in_port sim wire := Bits.gnd in *)
  let cycles n =
    for _ = 1 to n do
      Cyclesim.cycle sim
    done
  in
  set _enable;
  Cyclesim.in_port sim _digit_0 := Bits.of_int ~width:4 1;
  Cyclesim.in_port sim _digit_1 := Bits.of_int ~width:4 2;
  cycles 2;
  set _dot;
  Cyclesim.in_port sim _digit_0 := Bits.of_int ~width:4 8;
  Cyclesim.in_port sim _digit_1 := Bits.of_int ~width:4 7;
  cycles 2;
  let display_rules =
    Hardcaml_waveterm.Display_rule.[ port_name_is_one_of [ _segment; _anode ] ~wave_format:Bit; default ]
  in
  Hardcaml_waveterm.Waveform.print ~display_rules ~display_height:20 ~display_width:47 ~wave_width:3 waves;
  [%expect {|
    ┌Signals──┐┌Waves─────────────────────────────┐
    │         ││────────┬───────┬───────┬───────  │
    │anode    ││ 10     │01     │10     │01       │
    │         ││────────┴───────┴───────┴───────  │
    │         ││────────┬───────┬───────┬───────  │
    │segment  ││ 1111001│0100100│0000000│1111000  │
    │         ││────────┴───────┴───────┴───────  │
    │clock    ││┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌─│
    │         ││    └───┘   └───┘   └───┘   └───┘ │
    │         ││────────────────┬───────────────  │
    │digit_0  ││ 1              │8                │
    │         ││────────────────┴───────────────  │
    │         ││────────────────┬───────────────  │
    │digit_1  ││ 2              │7                │
    │         ││────────────────┴───────────────  │
    │dot      ││                ┌───────────────  │
    │         ││────────────────┘                 │
    │enable   ││────────────────────────────────  │
    │         ││                                  │
    └─────────┘└──────────────────────────────────┘
  |}]

