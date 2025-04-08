open Hardcaml
open Util

let%expect_test "pulse_test" =
  let scope = Scope.create ~flatten_design:true () in
  let module Simulator = Cyclesim.With_interface (Pulse.I) (Pulse.O) in
  let config =
    {
      Cyclesim.Config.default with
      is_internal_port = Some (fun s -> Signal.names s |> List.exists (String.starts_with ~prefix:"count"));
    }
  in
  let waves, sim = Pulse.create ~length:4 scope |> Simulator.create ~config |> Hardcaml_waveterm.Waveform.create in
  let inputs = Cyclesim.inputs sim in
  let cycles n =
    for _ = 1 to n do
      Cyclesim.cycle sim
    done
  in
  let set wire = wire := Bits.vdd in
  let clear wire = wire := Bits.gnd in
  cycles 1;
  set inputs.reset;
  cycles 1;
  clear inputs.reset;
  cycles 4;
  set inputs.reset;
  cycles 3;
  clear inputs.reset;
  cycles 4;
  set inputs.reset;
  cycles 1;
  clear inputs.reset;
  cycles 1;
  set inputs.reset;
  cycles 1;
  clear inputs.reset;
  cycles 5;
  Hardcaml_waveterm.Waveform.print ~display_height:14 ~display_width:60 ~wave_width:0 waves;
  [%expect
    {|
    ┌Signals──────┐┌Waves──────────────────────────────────────┐
    │clock        ││┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌│
    │             ││ └┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘│
    │reset_       ││  ┌─┐       ┌─────┐       ┌─┐ ┌─┐          │
    │             ││──┘ └───────┘     └───────┘ └─┘ └───────── │
    │pulse        ││──────────┐   ┌─────────┐   ┌─────────┐    │
    │             ││          └───┘         └───┘         └─── │
    │             ││──┬─┬─┬─┬─┬───┬─────┬─┬─┬───┬─┬─┬─┬─┬─┬─── │
    │count        ││ 0│1│0│1│2│3  │0    │1│2│3  │0│1│0│1│2│3   │
    │             ││──┴─┴─┴─┴─┴───┴─────┴─┴─┴───┴─┴─┴─┴─┴─┴─── │
    │             ││──┬─┬─┬─┬─────┬─────┬─┬─────┬─┬─┬─┬─┬───── │
    │count_succ   ││ 1│2│1│2│3    │1    │2│3    │1│2│1│2│3     │
    │             ││──┴─┴─┴─┴─────┴─────┴─┴─────┴─┴─┴─┴─┴───── │
    └─────────────┘└───────────────────────────────────────────┘
  |}]

let%expect_test "counter_with_carry_test_1" =
  let scope = Scope.create ~flatten_design:true () in
  let (module CounterBits) = integer 3 in
  let module Counter = Counter (CounterBits) in
  let module Simulator = Cyclesim.With_interface (Counter.I) (Counter.O) in
  let waves, sim = Counter.create ~base:5 scope |> Simulator.create |> Hardcaml_waveterm.Waveform.create in
  let inputs = Cyclesim.inputs sim in
  let set wire = wire := Bits.vdd in
  let clear wire = wire := Bits.gnd in
  let cycles n =
    for _ = 1 to n do
      Cyclesim.cycle sim
    done
  in
  cycles 2;
  set inputs.enable;
  cycles 5;
  clear inputs.enable;
  cycles 3;
  set inputs.enable;
  cycles 7;
  clear inputs.enable;
  cycles 1;
  set inputs.enable;
  cycles 2;
  clear inputs.enable;
  cycles 4;
  set inputs.enable;
  cycles 7;
  Hardcaml_waveterm.Waveform.print ~display_height:14 ~display_width:60 ~wave_width:0 waves;
  [%expect
    {|
    ┌Signals──────┐┌Waves──────────────────────────────────────┐
    │clock        ││┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌│
    │             ││ └┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘│
    │enable       ││    ┌─────────┐     ┌─────────────┐ ┌───┐  │
    │             ││────┘         └─────┘             └─┘   └──│
    │reset_       ││                                           │
    │             ││───────────────────────────────────────────│
    │cary         ││            ┌─┐             ┌─┐            │
    │             ││────────────┘ └─────────────┘ └────────────│
    │             ││──────┬─┬─┬─┬─┬───────┬─┬─┬─┬─┬─┬─┬───┬─┬──│
    │count        ││ 0    │1│2│3│4│0      │1│2│3│4│0│1│2  │3│4 │
    │             ││──────┴─┴─┴─┴─┴───────┴─┴─┴─┴─┴─┴─┴───┴─┴──│
    │             ││                                           │
    └─────────────┘└───────────────────────────────────────────┘ |}]

let%expect_test "counter_with_carry_test_2" =
  let scope = Scope.create ~flatten_design:true () in
  let (module CounterBits) = integer 4 in
  let module Counter = Counter (CounterBits) in
  let module Simulator = Cyclesim.With_interface (Counter.I) (Counter.O) in
  let circuit input =
    let count0 = Counter.create scope input in
    let count1 = Counter.create scope { input with enable = count0.cary } in
    let open Signal in
    count0.count -- "count0" |> ignore;
    count0.cary -- "cary0" |> ignore;
    count1
  in
  let waves, sim = circuit |> Simulator.create ~config:Cyclesim.Config.trace_all |> Hardcaml_waveterm.Waveform.create in
  let inputs = Cyclesim.inputs sim in
  let set wire = wire := Bits.vdd in
  let clear wire = wire := Bits.gnd in
  let cycles n =
    for _ = 1 to n do
      Cyclesim.cycle sim
    done
  in
  cycles 2;
  set inputs.enable;
  cycles 5;
  clear inputs.enable;
  cycles 3;
  set inputs.enable;
  cycles 12;
  clear inputs.enable;
  cycles 1;
  set inputs.enable;
  cycles 2;
  set inputs.reset;
  cycles 2;
  clear inputs.reset;
  cycles 4;
  Hardcaml_waveterm.Waveform.print ~display_height:18 ~display_width:85 ~wave_width:0 waves;
  [%expect
    {|
    ┌Signals───────────┐┌Waves──────────────────────────────────────────────────────────┐
    │clock             ││┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌│
    │                  ││ └┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘│
    │enable            ││    ┌─────────┐     ┌───────────────────────┐ ┌─────────────── │
    │                  ││────┘         └─────┘                       └─┘                │
    │reset_            ││                                                  ┌───┐        │
    │                  ││──────────────────────────────────────────────────┘   └─────── │
    │cary              ││                                                               │
    │                  ││────────────────────────────────────────────────────────────── │
    │                  ││──────────────────────────────────────────┬─────────┬───────── │
    │count             ││ 0                                        │1        │0         │
    │                  ││──────────────────────────────────────────┴─────────┴───────── │
    │cary0             ││                                        ┌─┐                    │
    │                  ││────────────────────────────────────────┘ └─────────────────── │
    │                  ││──────┬─┬─┬─┬─┬───────┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬───┬─┬─┬───┬─┬─┬─ │
    │count0            ││ 0    │1│2│3│4│5      │6│7│8│9│A│B│C│D│E│F│0│1  │2│3│0  │1│2│3 │
    │                  ││──────┴─┴─┴─┴─┴───────┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴───┴─┴─┴───┴─┴─┴─ │
    └──────────────────┘└───────────────────────────────────────────────────────────────┘ |}]

let%expect_test "trigger_test" =
  let scope = Scope.create ~flatten_design:true () in
  let module Simulator = Cyclesim.With_interface (Trigger.I) (Trigger.O) in
  let waves, sim =
    Trigger.create ~clock_freq:10 ~target:2 scope
    |> Simulator.create ~config:Cyclesim.Config.trace_all
    |> Hardcaml_waveterm.Waveform.create
  in
  let inputs = Cyclesim.inputs sim in
  let set wire = wire := Bits.vdd in
  let clear wire = wire := Bits.gnd in
  let cycles n =
    for _ = 1 to n do
      Cyclesim.cycle sim
    done
  in
  cycles 7;
  set inputs.reset;
  cycles 2;
  clear inputs.reset;
  cycles 8;
  set inputs.reset;
  cycles 2;
  clear inputs.reset;
  cycles 10;
  Hardcaml_waveterm.Waveform.print ~display_height:11 ~display_width:80 ~wave_width:0 waves;
  [%expect
    {|
    ┌Signals───────────┐┌Waves─────────────────────────────────────────────────────┐
    │clock             ││┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐│
    │                  ││ └┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└│
    │reset_            ││              ┌───┐               ┌───┐                   │
    │                  ││──────────────┘   └───────────────┘   └───────────────────│
    │pulse             ││        ┌─┐               ┌─┐                 ┌─┐       ┌─│
    │                  ││────────┘ └───────────────┘ └─────────────────┘ └───────┘ │
    │                  ││──┬─┬─┬─┬─┬─┬─┬─┬───┬─┬─┬─┬─┬─┬─┬─┬─┬───┬─┬─┬─┬─┬─┬─┬─┬─┬─│
    │count             ││ 0│1│2│3│4│0│1│2│0  │1│2│3│4│0│1│2│3│0  │1│2│3│4│0│1│2│3│4│
    │                  ││──┴─┴─┴─┴─┴─┴─┴─┴───┴─┴─┴─┴─┴─┴─┴─┴─┴───┴─┴─┴─┴─┴─┴─┴─┴─┴─│
    └──────────────────┘└──────────────────────────────────────────────────────────┘ |}]

let%expect_test "trigger_with_enable_test" =
  let scope = Scope.create ~flatten_design:true () in
  let module Simulator = Cyclesim.With_interface (TriggerWithEnable.I) (TriggerWithEnable.O) in
  let waves, sim =
    TriggerWithEnable.create ~divider:3 scope
    |> Simulator.create ~config:Cyclesim.Config.trace_all
    |> Hardcaml_waveterm.Waveform.create
  in
  let inputs = Cyclesim.inputs sim in
  let set wire = wire := Bits.vdd in
  let clear wire = wire := Bits.gnd in
  let cycles n =
    for _ = 1 to n do
      Cyclesim.cycle sim
    done
  in
  cycles 1;
  set inputs.enable;
  cycles 5;
  set inputs.reset;
  cycles 2;
  clear inputs.reset;
  cycles 5;
  clear inputs.enable;
  cycles 2;
  set inputs.enable;
  cycles 1;
  clear inputs.enable;
  cycles 1;
  set inputs.reset;
  cycles 1;
  clear inputs.reset;
  cycles 1;
  set inputs.enable;
  cycles 15;
  Hardcaml_waveterm.Waveform.print ~display_height:14 ~display_width:88 ~wave_width:0 waves;
  [%expect
    {|
    ┌Signals───────────┐┌Waves─────────────────────────────────────────────────────────────┐
    │clock             ││┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐│
    │                  ││ └┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└│
    │enable            ││  ┌───────────────────────┐   ┌─┐     ┌───────────────────────────│
    │                  ││──┘                       └───┘ └─────┘                           │
    │reset_            ││            ┌───┐                 ┌─┐                             │
    │                  ││────────────┘   └─────────────────┘ └─────────────────────────────│
    │pulse             ││      ┌─┐   ┌─┐     ┌─┐       ┌─┐         ┌─┐   ┌─┐   ┌─┐   ┌─┐   │
    │                  ││──────┘ └───┘ └─────┘ └───────┘ └─────────┘ └───┘ └───┘ └───┘ └───│
    │                  ││────┬─┬─┬─┬─┬─┬───┬─┬─┬─┬─┬─────┬───────┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─│
    │count             ││ 0  │1│2│0│1│2│0  │1│2│0│1│2    │0      │1│2│0│1│2│0│1│2│0│1│2│0│1│
    │                  ││────┴─┴─┴─┴─┴─┴───┴─┴─┴─┴─┴─────┴───────┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─│
    │vdd               ││──────────────────────────────────────────────────────────────────│
    └──────────────────┘└──────────────────────────────────────────────────────────────────┘ |}]

let%expect_test "pwm_test_1" =
  let (module Int4) = integer 4 in
  let module Pwm = Pwm (Int4) in
  let scope = Scope.create ~flatten_design:true () in
  let module Simulator = Cyclesim.With_interface (Pwm.WithCounter.I) (Pwm.O) in
  let waves, sim = Pwm.WithCounter.create scope |> Simulator.create |> Hardcaml_waveterm.Waveform.create in
  let cycles n =
    for _ = 1 to n do
      Cyclesim.cycle sim
    done
  in
  let inputs = Cyclesim.inputs sim in
  let set wire = wire := Bits.vdd in
  let clear wire = wire := Bits.gnd in
  let set_value v = inputs.value := Bits.of_int ~width:(Bits.width !(inputs.value)) v in
  set_value 0;
  cycles 1;
  set inputs.counter.enable;
  cycles 1;
  set inputs.counter.reset;
  cycles 1;
  clear inputs.counter.reset;
  set_value 2;
  cycles 2;
  set_value 1;
  cycles 5;
  set_value 3;
  cycles 5;
  set_value 0;
  cycles 1;
  set_value 3;
  cycles 10;
  Hardcaml_waveterm.Waveform.print ~display_height:14 ~display_width:70 ~wave_width:0 waves;
  [%expect
    {|
    ┌Signals────────┐┌Waves──────────────────────────────────────────────┐
    │clock          ││┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌│
    │               ││ └┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘│
    │enable         ││  ┌────────────────────────────────────────────────│
    │               ││──┘                                                │
    │reset_         ││    ┌─┐                                            │
    │               ││────┘ └────────────────────────────────────────────│
    │               ││──────┬───┬─────────┬─────────┬─┬──────────────────│
    │value          ││ 0    │2  │1        │3        │0│3                 │
    │               ││──────┴───┴─────────┴─────────┴─┴──────────────────│
    │control        ││      ┌───┐   ┌─┐     ┌─────┐   ┌───┐ ┌─────┐ ┌────│
    │               ││──────┘   └───┘ └─────┘     └───┘   └─┘     └─┘    │
    │               ││                                                   │
    └───────────────┘└───────────────────────────────────────────────────┘ |}]

let%expect_test "pwm_test_2" =
  let scope = Scope.create ~flatten_design:true () in
  let (module Int4) = integer 4 in
  let module Pwm = Pwm (Int4) in
  let module Simulator = Cyclesim.With_interface (Pwm.WithCounter.I) (Pwm.O) in
  let waves, sim = Pwm.WithCounter.create scope |> Simulator.create |> Hardcaml_waveterm.Waveform.create in
  let cycles n =
    for _ = 1 to n do
      Cyclesim.cycle sim
    done
  in
  let inputs = Cyclesim.inputs sim in
  let set wire = wire := Bits.vdd in
  let clear wire = wire := Bits.gnd in
  let set_value v = inputs.value := Bits.of_int ~width:(Bits.width !(inputs.value)) v in
  set inputs.counter.enable;
  set_value 0;
  cycles 1;
  set inputs.counter.reset;
  cycles 1;
  set_value 2;
  clear inputs.counter.reset;
  cycles 1;
  set_value 1;
  cycles 5;
  set_value 0;
  cycles 5;
  set_value 3;
  cycles 10;
  Hardcaml_waveterm.Waveform.print ~display_height:14 ~display_width:60 ~wave_width:0 waves;
  [%expect
    {|
    ┌Signals──────┐┌Waves──────────────────────────────────────┐
    │clock        ││┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌│
    │             ││ └┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘│
    │enable       ││───────────────────────────────────────────│
    │             ││                                           │
    │reset_       ││  ┌─┐                                      │
    │             ││──┘ └──────────────────────────────────────│
    │             ││────┬─┬─────────┬─────────┬────────────────│
    │value        ││ 0  │2│1        │0        │3               │
    │             ││────┴─┴─────────┴─────────┴────────────────│
    │control      ││    ┌─┐     ┌─┐             ┌─────┐ ┌─────┐│
    │             ││────┘ └─────┘ └─────────────┘     └─┘     └│
    │             ││                                           │
    └─────────────┘└───────────────────────────────────────────┘ |}]

let%expect_test "segment_encode_test" =
  let scope = Scope.create ~flatten_design:true () in
  let module Simulator = Cyclesim.With_interface (SegmentEncoder.I) (SegmentEncoder.O) in
  let waves, sim = SegmentEncoder.create scope |> Simulator.create |> Hardcaml_waveterm.Waveform.create in
  let set wire v =
    let width = Bits.width !wire in
    wire := Bits.of_int ~width v
  in
  let set n =
    let inputs = Cyclesim.inputs sim in
    set inputs.digit n;
    Cyclesim.cycle sim
  in
  set 0;
  set 1;
  set 8;
  set 0xb;
  set 0xF;
  let display_rules = Hardcaml_waveterm.Display_rule.[ port_name_is "segment" ~wave_format:Bit; default ] in
  Hardcaml_waveterm.Waveform.print ~display_rules ~display_height:8 ~display_width:80 ~wave_width:4 waves;
  [%expect
    {|
    ┌Signals───────────┐┌Waves─────────────────────────────────────────────────────┐
    │                  ││──────────┬─────────┬─────────┬─────────┬─────────        │
    │segment           ││ 1000000  │1111001  │0000000  │0000011  │0001110          │
    │                  ││──────────┴─────────┴─────────┴─────────┴─────────        │
    │                  ││──────────┬─────────┬─────────┬─────────┬─────────        │
    │digit             ││ 0        │1        │8        │B        │F                │
    │                  ││──────────┴─────────┴─────────┴─────────┴─────────        │
    └──────────────────┘└──────────────────────────────────────────────────────────┘ |}]
