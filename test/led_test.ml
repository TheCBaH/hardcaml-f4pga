open Hardcaml
open Led

let level_control_test control =
  let test max levels level =
    control ~max ~levels ~level |> Printf.printf "max:%u levels:%u level:%u value:%u\n%!" max levels level
  in
  let levels = [ 15; 14; 11; 8; 4; 1; 0 ] in
  List.iter (test 255 16) levels;
  List.iter (test 25 16) levels;
  List.iter (test 7 16) levels

let%expect_test "level_control_float_test" =
  level_control_test level_control_float;
  [%expect
    {|
      max:255 levels:16 level:15 value:255
      max:255 levels:16 level:14 value:238
      max:255 levels:16 level:11 value:187
      max:255 levels:16 level:8 value:136
      max:255 levels:16 level:4 value:68
      max:255 levels:16 level:1 value:17
      max:255 levels:16 level:0 value:0
      max:25 levels:16 level:15 value:25
      max:25 levels:16 level:14 value:23
      max:25 levels:16 level:11 value:18
      max:25 levels:16 level:8 value:13
      max:25 levels:16 level:4 value:6
      max:25 levels:16 level:1 value:1
      max:25 levels:16 level:0 value:0
      max:7 levels:16 level:15 value:7
      max:7 levels:16 level:14 value:6
      max:7 levels:16 level:11 value:5
      max:7 levels:16 level:8 value:3
      max:7 levels:16 level:4 value:1
      max:7 levels:16 level:1 value:0
      max:7 levels:16 level:0 value:0
 |}]

let%expect_test "level_control_int_test" =
  level_control_test (level_control_int ~scale:1);
  [%expect
    {|
      max:255 levels:16 level:15 value:255
      max:255 levels:16 level:14 value:238
      max:255 levels:16 level:11 value:187
      max:255 levels:16 level:8 value:136
      max:255 levels:16 level:4 value:68
      max:255 levels:16 level:1 value:17
      max:255 levels:16 level:0 value:0
      max:25 levels:16 level:15 value:15
      max:25 levels:16 level:14 value:14
      max:25 levels:16 level:11 value:11
      max:25 levels:16 level:8 value:8
      max:25 levels:16 level:4 value:4
      max:25 levels:16 level:1 value:1
      max:25 levels:16 level:0 value:0
      max:7 levels:16 level:15 value:0
      max:7 levels:16 level:14 value:0
      max:7 levels:16 level:11 value:0
      max:7 levels:16 level:8 value:0
      max:7 levels:16 level:4 value:0
      max:7 levels:16 level:1 value:0
      max:7 levels:16 level:0 value:0
 |}]

let%expect_test "level_control_int_test_scaled_4" =
  level_control_test (level_control_int ~scale:4);
  [%expect
    {|
      max:255 levels:16 level:15 value:255
      max:255 levels:16 level:14 value:238
      max:255 levels:16 level:11 value:187
      max:255 levels:16 level:8 value:136
      max:255 levels:16 level:4 value:68
      max:255 levels:16 level:1 value:17
      max:255 levels:16 level:0 value:0
      max:25 levels:16 level:15 value:22
      max:25 levels:16 level:14 value:21
      max:25 levels:16 level:11 value:16
      max:25 levels:16 level:8 value:12
      max:25 levels:16 level:4 value:6
      max:25 levels:16 level:1 value:1
      max:25 levels:16 level:0 value:0
      max:7 levels:16 level:15 value:3
      max:7 levels:16 level:14 value:3
      max:7 levels:16 level:11 value:2
      max:7 levels:16 level:8 value:2
      max:7 levels:16 level:4 value:1
      max:7 levels:16 level:1 value:0
      max:7 levels:16 level:0 value:0
 |}]

let%expect_test "level_control_int_test_scaled_16" =
  level_control_test (level_control_int ~scale:16);
  [%expect
    {|
      max:255 levels:16 level:15 value:255
      max:255 levels:16 level:14 value:238
      max:255 levels:16 level:11 value:187
      max:255 levels:16 level:8 value:136
      max:255 levels:16 level:4 value:68
      max:255 levels:16 level:1 value:17
      max:255 levels:16 level:0 value:0
      max:25 levels:16 level:15 value:24
      max:25 levels:16 level:14 value:22
      max:25 levels:16 level:11 value:17
      max:25 levels:16 level:8 value:13
      max:25 levels:16 level:4 value:6
      max:25 levels:16 level:1 value:1
      max:25 levels:16 level:0 value:0
      max:7 levels:16 level:15 value:6
      max:7 levels:16 level:14 value:6
      max:7 levels:16 level:11 value:4
      max:7 levels:16 level:8 value:3
      max:7 levels:16 level:4 value:1
      max:7 levels:16 level:1 value:0
      max:7 levels:16 level:0 value:0
 |}]

let%expect_test "bits_of_constant_test" =
  let test c = bits_of_constant c |> Printf.printf "constant:%u bits:%u\n%!" c in
  test 0;
  test 1;
  test 2;
  test 3;
  test 4;
  test 7;
  test 8;
  [%expect
    {|
    constant:0 bits:1
    constant:1 bits:1
    constant:2 bits:2
    constant:3 bits:2
    constant:4 bits:3
    constant:7 bits:3
    constant:8 bits:4 |}]

let%expect_test "level_control_bit_test" =
  let test scale max levels level =
    let level_bits = bits_of_constant level in
    let level = Bits.of_int ~width:level_bits level in
    let value = level_control_bit ~max ~levels ~level ~scale in
    Printf.printf "scale:%u max:%u levels:%u level:%u value:%u:%u\n%!" scale max levels (Bits.to_int level)
      (Bits.to_int value) (Bits.width value)
  in
  List.iter
    (fun scale ->
      let levels = [ 15; 14; 11; 8; 4; 1; 0 ] in
      List.iter (test scale 255 16) levels;
      List.iter (test scale 25 16) levels;
      List.iter (test scale 7 16) levels)
    [ 1; 4; 16 ];
  [%expect
    {|
    scale:1 max:255 levels:16 level:15 value:255:8
    scale:1 max:255 levels:16 level:14 value:238:8
    scale:1 max:255 levels:16 level:11 value:187:8
    scale:1 max:255 levels:16 level:8 value:136:8
    scale:1 max:255 levels:16 level:4 value:68:8
    scale:1 max:255 levels:16 level:1 value:17:8
    scale:1 max:255 levels:16 level:0 value:0:8
    scale:1 max:25 levels:16 level:15 value:15:5
    scale:1 max:25 levels:16 level:14 value:14:5
    scale:1 max:25 levels:16 level:11 value:11:5
    scale:1 max:25 levels:16 level:8 value:8:5
    scale:1 max:25 levels:16 level:4 value:4:5
    scale:1 max:25 levels:16 level:1 value:1:5
    scale:1 max:25 levels:16 level:0 value:0:5
    scale:1 max:7 levels:16 level:15 value:0:3
    scale:1 max:7 levels:16 level:14 value:0:3
    scale:1 max:7 levels:16 level:11 value:0:3
    scale:1 max:7 levels:16 level:8 value:0:3
    scale:1 max:7 levels:16 level:4 value:0:3
    scale:1 max:7 levels:16 level:1 value:0:3
    scale:1 max:7 levels:16 level:0 value:0:3
    scale:4 max:255 levels:16 level:15 value:255:8
    scale:4 max:255 levels:16 level:14 value:238:8
    scale:4 max:255 levels:16 level:11 value:187:8
    scale:4 max:255 levels:16 level:8 value:136:8
    scale:4 max:255 levels:16 level:4 value:68:8
    scale:4 max:255 levels:16 level:1 value:17:8
    scale:4 max:255 levels:16 level:0 value:0:8
    scale:4 max:25 levels:16 level:15 value:22:5
    scale:4 max:25 levels:16 level:14 value:21:5
    scale:4 max:25 levels:16 level:11 value:16:5
    scale:4 max:25 levels:16 level:8 value:12:5
    scale:4 max:25 levels:16 level:4 value:6:5
    scale:4 max:25 levels:16 level:1 value:1:5
    scale:4 max:25 levels:16 level:0 value:0:5
    scale:4 max:7 levels:16 level:15 value:3:3
    scale:4 max:7 levels:16 level:14 value:3:3
    scale:4 max:7 levels:16 level:11 value:2:3
    scale:4 max:7 levels:16 level:8 value:2:3
    scale:4 max:7 levels:16 level:4 value:1:3
    scale:4 max:7 levels:16 level:1 value:0:3
    scale:4 max:7 levels:16 level:0 value:0:3
    scale:16 max:255 levels:16 level:15 value:255:8
    scale:16 max:255 levels:16 level:14 value:238:8
    scale:16 max:255 levels:16 level:11 value:187:8
    scale:16 max:255 levels:16 level:8 value:136:8
    scale:16 max:255 levels:16 level:4 value:68:8
    scale:16 max:255 levels:16 level:1 value:17:8
    scale:16 max:255 levels:16 level:0 value:0:8
    scale:16 max:25 levels:16 level:15 value:24:5
    scale:16 max:25 levels:16 level:14 value:22:5
    scale:16 max:25 levels:16 level:11 value:17:5
    scale:16 max:25 levels:16 level:8 value:13:5
    scale:16 max:25 levels:16 level:4 value:6:5
    scale:16 max:25 levels:16 level:1 value:1:5
    scale:16 max:25 levels:16 level:0 value:0:5
    scale:16 max:7 levels:16 level:15 value:6:3
    scale:16 max:7 levels:16 level:14 value:6:3
    scale:16 max:7 levels:16 level:11 value:4:3
    scale:16 max:7 levels:16 level:8 value:3:3
    scale:16 max:7 levels:16 level:4 value:1:3
    scale:16 max:7 levels:16 level:1 value:0:3
    scale:16 max:7 levels:16 level:0 value:0:3 |}]

let%expect_test "level_control_test" =
  let _level = "level" in
  let _value = "value" in
  let levels = 16 in
  let test scale max =
    let level = Base.Int.ceil_log2 levels |> Signal.input _level in
    let value = level_control ~levels ~level ~max ~scale in
    let circuit = Circuit.create_exn ~name:"level_control" [ Signal.output _value value ] in
    let waves, sim = Hardcaml_waveterm.Waveform.create (Cyclesim.create circuit) in
    List.iter
      (fun v ->
        Cyclesim.in_port sim _level := Bits.of_int ~width:(Signal.width level) v;
        Cyclesim.cycle sim)
      [ 15; 14; 11; 9; 8; 7; 4; 1; 0 ];
    let display_rules =
      Hardcaml_waveterm.Display_rule.[ port_name_is_one_of [ _level; _value ] ~wave_format:Unsigned_int; default ]
    in
    Hardcaml_waveterm.Waveform.print ~display_rules ~display_height:8 ~display_width:80 ~wave_width:2 waves
  in
  List.iter
    (fun scale ->
      List.iter
        (fun max ->
          Printf.printf "scale:%u max:%u\n" scale max;
          test max scale)
        [ 4; 16 ])
    [ 255; 25; 7 ];
  [%expect
    {|
    scale:255 max:4
    ┌Signals───────────┐┌Waves─────────────────────────────────────────────────────┐
    │                  ││──────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────    │
    │level             ││ 15   │14   │11   │9    │8    │7    │4    │1    │0        │
    │                  ││──────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────    │
    │                  ││──────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────    │
    │value             ││ 255  │238  │187  │153  │136  │119  │68   │17   │0        │
    │                  ││──────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────    │
    └──────────────────┘└──────────────────────────────────────────────────────────┘
    scale:255 max:16
    ┌Signals───────────┐┌Waves─────────────────────────────────────────────────────┐
    │                  ││──────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────    │
    │level             ││ 15   │14   │11   │9    │8    │7    │4    │1    │0        │
    │                  ││──────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────    │
    │                  ││──────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────    │
    │value             ││ 255  │238  │187  │153  │136  │119  │68   │17   │0        │
    │                  ││──────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────    │
    └──────────────────┘└──────────────────────────────────────────────────────────┘
    scale:25 max:4
    ┌Signals───────────┐┌Waves─────────────────────────────────────────────────────┐
    │                  ││──────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────    │
    │level             ││ 15   │14   │11   │9    │8    │7    │4    │1    │0        │
    │                  ││──────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────    │
    │                  ││──────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────    │
    │value             ││ 22   │21   │16   │13   │12   │10   │6    │1    │0        │
    │                  ││──────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────    │
    └──────────────────┘└──────────────────────────────────────────────────────────┘
    scale:25 max:16
    ┌Signals───────────┐┌Waves─────────────────────────────────────────────────────┐
    │                  ││──────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────    │
    │level             ││ 15   │14   │11   │9    │8    │7    │4    │1    │0        │
    │                  ││──────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────    │
    │                  ││──────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────    │
    │value             ││ 24   │22   │17   │14   │13   │11   │6    │1    │0        │
    │                  ││──────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────    │
    └──────────────────┘└──────────────────────────────────────────────────────────┘
    scale:7 max:4
    ┌Signals───────────┐┌Waves─────────────────────────────────────────────────────┐
    │                  ││──────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────    │
    │level             ││ 15   │14   │11   │9    │8    │7    │4    │1    │0        │
    │                  ││──────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────    │
    │                  ││────────────┬─────────────────┬───────────┬───────────    │
    │value             ││ 3          │2                │1          │0              │
    │                  ││────────────┴─────────────────┴───────────┴───────────    │
    └──────────────────┘└──────────────────────────────────────────────────────────┘
    scale:7 max:16
    ┌Signals───────────┐┌Waves─────────────────────────────────────────────────────┐
    │                  ││──────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────    │
    │level             ││ 15   │14   │11   │9    │8    │7    │4    │1    │0        │
    │                  ││──────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────    │
    │                  ││────────────┬─────┬─────────────────┬─────┬───────────    │
    │value             ││ 6          │4    │3                │1    │0              │
    │                  ││────────────┴─────┴─────────────────┴─────┴───────────    │
    └──────────────────┘└──────────────────────────────────────────────────────────┘ |}]

let%expect_test "pwm_control_test" =
  let (module Levels) = Util.integer 4 in
  let (module Base) = Util.integer 8 in
  let scope = Scope.create ~flatten_design:true () in
  let module PwmControl = PwmControl (Levels) (Base) in
  let module Simulator = Cyclesim.With_interface (PwmControl.WithCounter.I) (PwmControl.O) in
  let base = 8 in
  let waves, sim =
    PwmControl.WithCounter.create ~max:6 scope
    |> Simulator.create ~config:Cyclesim.Config.trace_all
    |> Hardcaml_waveterm.Waveform.create
  in
  let cycles n =
    for _ = 1 to n do
      Cyclesim.cycle sim
    done
  in
  let inputs = Cyclesim.inputs sim in
  let set wire = wire := Bits.vdd in
  let set_level v =
    inputs.level := Bits.of_int ~width:(Bits.width !(inputs.level)) v;
    2 * base |> cycles
  in
  set inputs.counter.enable;
  set_level 1;
  set_level 2;
  set_level 3;
  Hardcaml_waveterm.Waveform.print ~display_height:18 ~display_width:100 ~wave_width:0 waves;
  [%expect
    {|
    ┌Signals───────────┐┌Waves─────────────────────────────────────────────────────────────────────────┐
    │clock             ││┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐│
    │                  ││ └┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└│
    │enable            ││──────────────────────────────────────────────────────────────────────────────│
    │                  ││                                                                              │
    │                  ││────────────────────────────────┬───────────────────────────────┬─────────────│
    │level             ││ 1                              │2                              │3            │
    │                  ││────────────────────────────────┴───────────────────────────────┴─────────────│
    │reset_            ││                                                                              │
    │                  ││──────────────────────────────────────────────────────────────────────────────│
    │control           ││────┐           ┌───┐           ┌───────┐       ┌───────┐       ┌───────────┐ │
    │                  ││    └───────────┘   └───────────┘       └───────┘       └───────┘           └─│
    │gnd               ││                                                                              │
    │                  ││──────────────────────────────────────────────────────────────────────────────│
    │                  ││────────────────────────────────┬───────────────────────────────┬─────────────│
    │value             ││ 2                              │4                              │6            │
    │                  ││────────────────────────────────┴───────────────────────────────┴─────────────│
    └──────────────────┘└──────────────────────────────────────────────────────────────────────────────┘ |}]

let%expect_test "led_control_test" =
  let (module Levels) = Util.integer 4 in
  let (module Base) = Util.integer 8 in
  let scope = Scope.create ~flatten_design:true () in
  let module Led = Led (Levels) (Base) in
  let module Simulator = Cyclesim.With_interface (Led.WithCounter.I) (Led.O) in
  let base = 8 in
  let color = { Color.red = 2; Color.blue = 4; Color.green = 7 } in
  let waves, sim = Led.WithCounter.create ~color scope |> Simulator.create |> Hardcaml_waveterm.Waveform.create in
  let cycles n =
    for _ = 1 to n do
      Cyclesim.cycle sim
    done
  in
  let inputs = Cyclesim.inputs sim in
  let set wire = wire := Bits.vdd in
  let set_level v =
    List.iter
      (fun i -> i := Bits.of_int ~width:(Bits.width !i) v)
      [ inputs.level.l_blue; inputs.level.l_green; inputs.level.l_red ];
    2 * base |> cycles
  in
  set inputs.counter.enable;
  set_level 1;
  set_level 2;
  set_level 3;
  Hardcaml_waveterm.Waveform.print ~display_height:23 ~display_width:100 ~wave_width:0 waves;
  [%expect
    {|
    ┌Signals───────────┐┌Waves─────────────────────────────────────────────────────────────────────────┐
    │clock             ││┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐│
    │                  ││ └┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└│
    │enable            ││──────────────────────────────────────────────────────────────────────────────│
    │                  ││                                                                              │
    │                  ││────────────────────────────────┬───────────────────────────────┬─────────────│
    │l_blue            ││ 1                              │2                              │3            │
    │                  ││────────────────────────────────┴───────────────────────────────┴─────────────│
    │                  ││────────────────────────────────┬───────────────────────────────┬─────────────│
    │l_green           ││ 1                              │2                              │3            │
    │                  ││────────────────────────────────┴───────────────────────────────┴─────────────│
    │                  ││────────────────────────────────┬───────────────────────────────┬─────────────│
    │l_red             ││ 1                              │2                              │3            │
    │                  ││────────────────────────────────┴───────────────────────────────┴─────────────│
    │reset_            ││                                                                              │
    │                  ││──────────────────────────────────────────────────────────────────────────────│
    │blue              ││──┐             ┌─┐             ┌───┐           ┌───┐           ┌─────┐       │
    │                  ││  └─────────────┘ └─────────────┘   └───────────┘   └───────────┘     └───────│
    │green             ││────┐           ┌───┐           ┌───────┐       ┌───────┐       ┌───────────┐ │
    │                  ││    └───────────┘   └───────────┘       └───────┘       └───────┘           └─│
    │red               ││                                ┌─┐             ┌─┐             ┌─┐           │
    │                  ││────────────────────────────────┘ └─────────────┘ └─────────────┘ └───────────│
    └──────────────────┘└──────────────────────────────────────────────────────────────────────────────┘ |}]
