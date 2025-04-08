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

let%expect_test "alu_test" =
  let scope = Scope.create ~flatten_design:true () in
  let module Simulator = Cyclesim.With_interface (Alu.I) (Alu.O) in
  let waves, sim = Alu.create scope |> Simulator.create |> Hardcaml_waveterm.Waveform.create in
  let inputs = Cyclesim.inputs sim in
  let set w v = w := Bits.of_int ~width:Alu.bits v in
  let alu op a b =
    set inputs.a a;
    set inputs.b b;
    Alu.Binary.sim_set inputs.op op;
    Cyclesim.cycle sim;
    let open Base in
    let outputs = Cyclesim.outputs sim in
    let read s = Bits.to_int !s in
    let i_op = Or_error.ok_exn (Alu.Binary.sim_get inputs.op) in
    let data = read outputs.data in
    let zero = read outputs.zero in
    let carry = read outputs.carry in
    Stdio.print_s [%message (a : int) (i_op : Alu.Code.t) (b : int) (data : int) (zero : int) (carry : int)]
  in
  alu Alu.Code.Add 0x2 0x4;
  alu Alu.Code.Sub 0x6 0x1;
  alu Alu.Code.Sub 0x3 0x3;
  alu Alu.Code.Sub 0x1 0x3;
  alu Alu.Code.Add 0x70 0x80;
  alu Alu.Code.Add 0xF0 0x10;
  alu Alu.Code.Add 0xF0 0x20;
  Hardcaml_waveterm.Waveform.print ~display_height:18 ~display_width:80 ~wave_width:1 waves;
  [%expect
    {|
    ((a 2) (i_op Add) (b 4) (data 6) (zero 0) (carry 0))
    ((a 6) (i_op Sub) (b 1) (data 5) (zero 0) (carry 0))
    ((a 3) (i_op Sub) (b 3) (data 0) (zero 1) (carry 0))
    ((a 1) (i_op Sub) (b 3) (data 254) (zero 0) (carry 1))
    ((a 112) (i_op Add) (b 128) (data 240) (zero 0) (carry 0))
    ((a 240) (i_op Add) (b 16) (data 0) (zero 1) (carry 1))
    ((a 240) (i_op Add) (b 32) (data 16) (zero 0) (carry 1))
    ┌Signals───────────┐┌Waves─────────────────────────────────────────────────────┐
    │                  ││────┬───┬───┬───┬───┬───────                              │
    │a                 ││ 02 │06 │03 │01 │70 │F0                                   │
    │                  ││────┴───┴───┴───┴───┴───────                              │
    │                  ││────┬───┬───────┬───┬───┬───                              │
    │b                 ││ 04 │01 │03     │80 │10 │20                               │
    │                  ││────┴───┴───────┴───┴───┴───                              │
    │op_binary_variant ││    ┌───────────┐                                         │
    │                  ││────┘           └───────────                              │
    │carry             ││            ┌───┐   ┌───────                              │
    │                  ││────────────┘   └───┘                                     │
    │                  ││────┬───┬───┬───┬───┬───┬───                              │
    │data              ││ 06 │05 │00 │FE │F0 │00 │10                               │
    │                  ││────┴───┴───┴───┴───┴───┴───                              │
    │zero              ││        ┌───┐       ┌───┐                                 │
    │                  ││────────┘   └───────┘   └───                              │
    │                  ││                                                          │
    └──────────────────┘└──────────────────────────────────────────────────────────┘ |}]

let%expect_test "memory_test" =
  let scope = Scope.create ~flatten_design:true () in
  let module Simulator = Cyclesim.With_interface (Ram.I) (Ram.O) in
  let waves, sim = Ram.create scope |> Simulator.create |> Hardcaml_waveterm.Waveform.create in
  let inputs = Cyclesim.inputs sim in
  let set wire v =
    let width = Bits.width !wire in
    wire := Bits.of_int ~width v
  in
  let cycle () = Cyclesim.cycle sim in
  let do_write addr data =
    set inputs.addr addr;
    set inputs.w_data data;
    inputs.w_en := Bits.vdd;
    cycle ()
  in
  let do_read addr =
    inputs.w_en := Bits.gnd;
    set inputs.addr addr;
    cycle ()
  in
  do_write 0 0x5;
  do_write 3 0x8;
  do_write 4 0x10;
  do_write 7 0x17;
  do_read 3;
  do_read 0;
  cycle ();
  do_read 4;
  do_write 3 0x7;
  do_read 0;
  do_write 8 0x18;
  do_read 3;
  do_read 8;
  do_read 7;
  cycle ();
  Hardcaml_waveterm.Waveform.print ~display_height:15 ~display_width:80 ~wave_width:1 waves;
  [%expect
    {|
    ┌Signals───────────┐┌Waves─────────────────────────────────────────────────────┐
    │clock             ││┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─│
    │                  ││  └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ │
    │                  ││────┬───┬───┬───┬───┬───────┬───┬───┬───┬───┬───┬───┬─────│
    │addr              ││ 0  │3  │4  │7  │3  │0      │4  │3  │0  │8  │3  │8  │7    │
    │                  ││────┴───┴───┴───┴───┴───────┴───┴───┴───┴───┴───┴───┴─────│
    │                  ││────┬───┬───┬───────────────────┬───────┬─────────────────│
    │w_data            ││ 05 │08 │10 │17                 │07     │18               │
    │                  ││────┴───┴───┴───────────────────┴───────┴─────────────────│
    │w_en              ││────────────────┐               ┌───┐   ┌───┐             │
    │                  ││                └───────────────┘   └───┘   └─────────────│
    │                  ││────────────────┬───┬───────┬───┬───┬───┬───┬───┬───┬─────│
    │data              ││ 00             │08 │05     │10 │08 │05 │00 │07 │18 │17   │
    │                  ││────────────────┴───┴───────┴───┴───┴───┴───┴───┴───┴─────│
    └──────────────────┘└──────────────────────────────────────────────────────────┘ |}]

let%expect_test "initialized_memory_test" =
  let scope = Scope.create ~flatten_design:true () in
  let module Simulator = Cyclesim.With_interface (InitializedMemory.I) (InitializedMemory.O) in
  let rom = List.map (Bits.of_int ~width:Ram.bits) [ 0x11; 0x22; 0x33 ] in
  let waves, sim =
    InitializedMemory.create ~rom scope
    |> Simulator.create ~config:Cyclesim.Config.trace_all
    |> Hardcaml_waveterm.Waveform.create
  in
  let inputs = Cyclesim.inputs sim in
  let set wire v =
    let width = Bits.width !wire in
    wire := Bits.of_int ~width v
  in
  let cycle () = Cyclesim.cycle sim in
  let do_write addr data =
    set inputs.bus.addr addr;
    set inputs.bus.w_data data;
    inputs.bus.w_en := Bits.vdd;
    cycle ()
  in
  let do_read addr =
    inputs.bus.w_en := Bits.gnd;
    set inputs.bus.addr addr;
    cycle ()
  in
  let set wire = wire := Bits.vdd in
  let clear wire = wire := Bits.gnd in
  let wait () =
    let rec do_wait () =
      cycle ();
      let outputs = Cyclesim.outputs sim in
      let read s = Bits.to_int !s in
      let ready = read outputs.ready in
      if ready = 0 then do_wait () else ()
    in
    do_wait ()
  in
  wait ();
  do_read 0;
  do_read 1;
  do_read 2;
  do_write 5 0x55;
  do_write 1 0xDD;
  do_write 7 0x77;
  do_read 1;
  do_read 5;
  set inputs.reset;
  cycle ();
  clear inputs.reset;
  wait ();
  do_read 1;
  do_read 7;
  do_read 5;
  Hardcaml_waveterm.Waveform.print ~display_height:24 ~display_width:94 ~wave_width:1 waves;
  [%expect
    {|
    ┌Signals───────────┐┌Waves───────────────────────────────────────────────────────────────────┐
    │clock             ││┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ │
    │                  ││  └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─│
    │                  ││────────────────┬───┬───┬───┬───┬───┬───┬───────────────────┬───┬───┬───│
    │addr              ││ 0              │1  │2  │5  │1  │7  │1  │5                  │1  │7  │5  │
    │                  ││────────────────┴───┴───┴───┴───┴───┴───┴───────────────────┴───┴───┴───│
    │reset_            ││                                            ┌───┐                       │
    │                  ││────────────────────────────────────────────┘   └───────────────────────│
    │                  ││────────────────────────┬───┬───┬───────────────────────────────────────│
    │w_data            ││ 00                     │55 │DD │77                                     │
    │                  ││────────────────────────┴───┴───┴───────────────────────────────────────│
    │w_en              ││                        ┌───────────┐                                   │
    │                  ││────────────────────────┘           └───────────────────────────────────│
    │                  ││────────────┬───┬───┬───┬───┬───┬───┬───┬───────┬───┬───┬───┬───┬───┬───│
    │data              ││ 00         │11 │22 │33 │00 │22 │00 │DD │55     │11 │DD │33 │22 │77 │55 │
    │                  ││────────────┴───┴───┴───┴───┴───┴───┴───┴───────┴───┴───┴───┴───┴───┴───│
    │ready             ││            ┌───────────────────────────────────┐           ┌───────────│
    │                  ││────────────┘                                   └───────────┘           │
    │                  ││────┬───┬───────────────────────────────────────┬───┬───┬───────────────│
    │rom_addr          ││ 0  │1  │2                                      │0  │1  │2              │
    │                  ││────┴───┴───────────────────────────────────────┴───┴───┴───────────────│
    │rom_done          ││            ┌───────────────────────────────────┐           ┌───────────│
    │                  ││────────────┘                                   └───────────┘           │
    └──────────────────┘└────────────────────────────────────────────────────────────────────────┘ |}]

let%expect_test "memory_rom_test" =
  let scope = Scope.create ~flatten_design:true () in
  let module Simulator = Cyclesim.With_interface (MemoryWithRom.I) (MemoryWithRom.O) in
  let rom = List.map (Bits.of_int ~width:Ram.bits) [ 0x11; 0x22; 0x33 (* 3;4;5;6;7;8;9;10;11;12;13;14 ;15 *) ] in
  let waves, sim =
    MemoryWithRom.create ~rom scope
    |> Simulator.create ~config:Cyclesim.Config.trace_all
    |> Hardcaml_waveterm.Waveform.create
  in
  let inputs = Cyclesim.inputs sim in
  let set wire v =
    let width = Bits.width !wire in
    wire := Bits.of_int ~width v
  in
  let cycle () = Cyclesim.cycle sim in
  let do_write addr data =
    set inputs.addr addr;
    set inputs.w_data data;
    inputs.w_en := Bits.vdd;
    cycle ()
  in
  let do_read addr =
    inputs.w_en := Bits.gnd;
    set inputs.addr addr;
    cycle ()
  in
  do_read 0;
  do_read 1;
  do_read 2;
  do_write 5 0x55;
  do_write 1 0xDD;
  do_write 7 0x77;
  do_read 1;
  do_read 7;
  do_read 5;
  do_write 8 0x88;
  do_write 9 0x99;
  do_write 0xE 0xEE;
  do_read 0;
  do_read 9;
  do_read 8;
  do_read 7;
  Hardcaml_waveterm.Waveform.print ~signals_width:12 ~display_height:15 ~display_width:80 ~wave_width:1 waves;
  [%expect
    {|
    ┌Signals───┐┌Waves─────────────────────────────────────────────────────────────┐
    │clock     ││┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─│
    │          ││  └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ │
    │          ││────┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───  │
    │addr      ││ 0  │1  │2  │5  │1  │7  │1  │7  │5  │8  │9  │E  │0  │9  │8  │7    │
    │          ││────┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───  │
    │          ││────────────┬───┬───┬───────────────┬───┬───┬───────────────────  │
    │w_data    ││ 00         │55 │DD │77             │88 │99 │EE                   │
    │          ││────────────┴───┴───┴───────────────┴───┴───┴───────────────────  │
    │w_en      ││            ┌───────────┐           ┌───────────┐                 │
    │          ││────────────┘           └───────────┘           └───────────────  │
    │          ││────┬───┬───┬───┬───┬───┬───┬───┬───┬───────────┬───┬───┬───┬───  │
    │data      ││ 11 │22 │33 │00 │22 │00 │22 │77 │55 │00         │11 │99 │88 │77   │
    │          ││────┴───┴───┴───┴───┴───┴───┴───┴───┴───────────┴───┴───┴───┴───  │
    └──────────┘└──────────────────────────────────────────────────────────────────┘ |}]

let%expect_test "pc_test" =
  let scope = Scope.create ~flatten_design:true () in
  let module Simulator = Cyclesim.With_interface (Pc.I) (Pc.O) in
  let waves, sim =
    Pc.create scope |> Simulator.create ~config:Cyclesim.Config.trace_all |> Hardcaml_waveterm.Waveform.create
  in
  let inputs = Cyclesim.inputs sim in
  let cycles n =
    for _ = 1 to n do
      Cyclesim.cycle sim
    done
  in
  let set wire = wire := Bits.vdd in
  let clear wire = wire := Bits.gnd in
  let jump v =
    inputs.w_data := Bits.of_int ~width:Pc.bits v;
    set inputs.w_en;
    cycles 1;
    clear inputs.w_en
  in
  set inputs.enable;
  cycles 4;
  jump 14;
  cycles 3;
  clear inputs.enable;
  cycles 2;
  jump 3;
  cycles 2;
  jump 9;
  set inputs.enable;
  cycles 8;
  Hardcaml_waveterm.Waveform.print ~signals_width:12 ~display_height:18 ~display_width:60 ~wave_width:0 waves;
  [%expect
    {|
    ┌Signals───┐┌Waves─────────────────────────────────────────┐
    │clock     ││┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐│
    │          ││ └┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└│
    │reset     ││                                              │
    │          ││────────────────────────────────────────────  │
    │enable    ││────────────────┐           ┌───────────────  │
    │          ││                └───────────┘                 │
    │          ││────────┬───────────┬─────┬─────────────────  │
    │w_data    ││ 0      │E          │3    │9                  │
    │          ││────────┴───────────┴─────┴─────────────────  │
    │w_en      ││        ┌─┐         ┌─┐   ┌─┐                 │
    │          ││────────┘ └─────────┘ └───┘ └───────────────  │
    │          ││──┬─┬─┬─┬─┬─┬─┬─┬─────┬─────┬─┬─┬─┬─┬─┬─┬─┬─  │
    │data      ││ 0│1│2│3│4│E│F│0│1    │3    │9│A│B│C│D│E│F│0  │
    │          ││──┴─┴─┴─┴─┴─┴─┴─┴─────┴─────┴─┴─┴─┴─┴─┴─┴─┴─  │
    │vdd       ││────────────────────────────────────────────  │
    │          ││                                              │
    └──────────┘└──────────────────────────────────────────────┘ |}]
