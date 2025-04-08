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

