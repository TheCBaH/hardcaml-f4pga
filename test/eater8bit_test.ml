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

let%expect_test "cpu_control_test" =
  let scope = Scope.create ~flatten_design:true () in
  let module Simulator = Cyclesim.With_interface (CpuControl.I) (CpuControl.O) in
  let waves, sim =
    CpuControl.create scope |> Simulator.create ~config:Cyclesim.Config.trace_all |> Hardcaml_waveterm.Waveform.create
  in
  let inputs = Cyclesim.inputs sim in
  let set wire = wire := Bits.vdd in
  let clear wire = wire := Bits.gnd in
  let cycles n =
    for _ = 1 to n do
      Cyclesim.cycle sim
    done
  in
  let opcode code = inputs.cpu.opcode := Bits.of_int ~width:Isa.Instruction.code_bits code.Isa.Instruction.code in
  let exec code =
    opcode code;
    cycles Isa.max_cycles
  in
  set inputs.enable;
  Isa.nop |> exec;
  Isa.lda 14 |> exec;
  Isa.add 15 |> exec;
  Isa.out |> exec;
  Isa.jmp 0 |> exec;
  inputs.cpu.flags.zero := Bits.vdd;
  Isa.jz 4 |> exec;
  Isa.hlt |> exec;
  cycles 2;
  set inputs.cpu_reset;
  cycles 2;
  clear inputs.cpu_reset;
  Isa.nop |> exec;
  clear inputs.enable;
  cycles 2;
  set inputs.enable;
  cycles Isa.max_cycles;
  Isa.nop |> exec;
  cycles 2;
  Hardcaml_waveterm.Waveform.print ~signals_width:14 ~display_height:56 ~display_width:110 ~wave_width:0 waves;
  [%expect
    {|
    ┌Signals─────┐┌Waves─────────────────────────────────────────────────────────────────────────────────────────┐
    │clock       ││┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐│
    │            ││ └┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└│
    │reset       ││                                                                                              │
    │            ││──────────────────────────────────────────────────────────────────────────────────────────────│
    │enable      ││────────────────────────────────────────────────────────────────────────────────────────┐   ┌─│
    │            ││                                                                                        └───┘ │
    │cpu_reset   ││                                                                          ┌───┐               │
    │            ││──────────────────────────────────────────────────────────────────────────┘   └───────────────│
    │flags_carry ││                                                                                              │
    │            ││──────────────────────────────────────────────────────────────────────────────────────────────│
    │flags_zero  ││                                                  ┌───────────────────────────────────────────│
    │            ││──────────────────────────────────────────────────┘                                           │
    │            ││──────────┬─────────┬─────────┬─────────┬─────────┬─────────┬─────────────────┬───────────────│
    │opcode      ││ 0        │1        │2        │E        │6        │8        │F                │0              │
    │            ││──────────┴─────────┴─────────┴─────────┴─────────┴─────────┴─────────────────┴───────────────│
    │_AI         ││                ┌─┐         ┌─┐                                                               │
    │            ││────────────────┘ └─────────┘ └───────────────────────────────────────────────────────────────│
    │_AO         ││                                  ┌─┐                                                         │
    │            ││──────────────────────────────────┘ └─────────────────────────────────────────────────────────│
    │_BI         ││                          ┌─┐                                                                 │
    │            ││──────────────────────────┘ └─────────────────────────────────────────────────────────────────│
    │_BO         ││                          ┌─┐                                                                 │
    │            ││──────────────────────────┘ └─────────────────────────────────────────────────────────────────│
    │_CE         ││  ┌─┐       ┌─┐       ┌─┐       ┌─┐       ┌─┐       ┌─┐       ┌─┐               ┌─┐           │
    │            ││──┘ └───────┘ └───────┘ └───────┘ └───────┘ └───────┘ └───────┘ └───────────────┘ └───────────│
    │_CO         ││──┐       ┌─┐       ┌─┐       ┌─┐       ┌─┐       ┌─┐       ┌─┐             ┌───┐       ┌─────│
    │            ││  └───────┘ └───────┘ └───────┘ └───────┘ └───────┘ └───────┘ └─────────────┘   └───────┘     │
    │_EO         ││                            ┌─┐                                                               │
    │            ││────────────────────────────┘ └───────────────────────────────────────────────────────────────│
    │_FI         ││                            ┌─┐                                                               │
    │            ││────────────────────────────┘ └───────────────────────────────────────────────────────────────│
    │_HLT        ││                                                                ┌─────────┐                   │
    │            ││────────────────────────────────────────────────────────────────┘         └───────────────────│
    │_II         ││  ┌─┐       ┌─┐       ┌─┐       ┌─┐       ┌─┐       ┌─┐       ┌─┐               ┌─┐           │
    │            ││──┘ └───────┘ └───────┘ └───────┘ └───────┘ └───────┘ └───────┘ └───────────────┘ └───────────│
    │_IO         ││              ┌─┐       ┌─┐                 ┌─┐       ┌─┐                                     │
    │            ││──────────────┘ └───────┘ └─────────────────┘ └───────┘ └─────────────────────────────────────│
    │_J          ││                                            ┌─┐       ┌─┐                                     │
    │            ││────────────────────────────────────────────┘ └───────┘ └─────────────────────────────────────│
    │_MI         ││──┐       ┌─┐ ┌─┐   ┌─┐ ┌─┐   ┌─┐       ┌─┐       ┌─┐       ┌─┐             ┌───┐       ┌─────│
    │            ││  └───────┘ └─┘ └───┘ └─┘ └───┘ └───────┘ └───────┘ └───────┘ └─────────────┘   └───────┘     │
    │_OI         ││                                  ┌─┐                                                         │
    │            ││──────────────────────────────────┘ └─────────────────────────────────────────────────────────│
    │_RI         ││                                                                                              │
    │            ││──────────────────────────────────────────────────────────────────────────────────────────────│
    │_RO         ││  ┌─┐       ┌─┐ ┌─┐   ┌─┐ ┌─┐   ┌─┐       ┌─┐       ┌─┐       ┌─┐               ┌─┐           │
    │            ││──┘ └───────┘ └─┘ └───┘ └─┘ └───┘ └───────┘ └───────┘ └───────┘ └───────────────┘ └───────────│
    │_SU         ││                                                                                              │
    │            ││──────────────────────────────────────────────────────────────────────────────────────────────│
    │            ││──┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─────────┬───┬─┬─┬─┬─┬─────│
    │cycle       ││ 0│1│2│3│4│0│1│2│3│4│0│1│2│3│4│0│1│2│3│4│0│1│2│3│4│0│1│2│3│4│0│1│2│3        │0  │1│2│3│4│0    │
    │            ││──┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─────────┴───┴─┴─┴─┴─┴─────│
    │halt        ││                                                                  ┌─────────┐                 │
    │            ││──────────────────────────────────────────────────────────────────┘         └─────────────────│
    └────────────┘└──────────────────────────────────────────────────────────────────────────────────────────────┘ |}]

module CpuTest (Config : CpuConfig) = struct
  let scope = Scope.create ~flatten_design:true ()

  module Simulator = Cyclesim.With_interface (Cpu.I) (Cpu.O)

  let rom = List.map (Bits.of_int ~width:Ram.bits) Config.rom

  let _ =
    List.mapi (fun n v -> Bits.to_int v |> Printf.sprintf "%2u: %02x " n) rom
    |> Format.printf "[%a]\n%!" Format.(pp_print_list pp_print_string)

  let waves, sim =
    Cpu.create ~rom ~split_ram:Config.split_ram scope
    |> Simulator.create ~config:Cyclesim.Config.trace_all
    |> Hardcaml_waveterm.Waveform.create

  let inputs = Cyclesim.inputs sim
  let outputs = Cyclesim.outputs sim
  let read s = Bits.to_int !s
  let a () = read outputs.internal.a.data
  let b () = read outputs.internal.b.data
  let pc () = read outputs.internal.pc.data
  let output () = read outputs.output.data
  let zero () = read outputs.flags.zero
  let carry () = read outputs.flags.carry
  let set wire = wire := Bits.vdd
  let clear wire = wire := Bits.vdd

  let cycles n =
    for _ = 1 to n do
      Cyclesim.cycle sim
    done

  let rec wait n =
    let outputs = Cyclesim.outputs sim in
    let ready = !(outputs.Cpu.O.ready) |> Bits.to_int in
    if ready = 0 then (
      cycles 1;
      succ n |> wait)
    else if n = 0 then 0
    else n - 1

  let start_cycle = wait 0
  let step () = cycles Isa.max_cycles
  let _ = set inputs.enable
end

let cpu_test_waves ~rom ~split_ram =
  let module Cpu = CpuTest ((val cpu_config ~rom ~split_ram)) in
  let open Cpu in
  step ();
  step ();
  step ();
  cycles 3;
  set inputs.cpu_reset;
  cycles 1;
  clear inputs.cpu_reset;
  step ();
  Hardcaml_waveterm.Waveform.print ~start_cycle ~signals_width:14 ~display_height:50 ~display_width:110 ~wave_width:1
    waves

let%expect_test "cpu_test_halt" =
  (* Isa.(assembler [ lda 14; add 15; out; jc 5; jmp 1; hlt ]) *)
  let rom =
    Isa.(assembler [ lda 14; add 15; out; hlt ])
    |> rom_prepare ~process:(fun ~write code -> code |> write 14 0xFF |> write 15 0x45)
  in
  cpu_test_waves ~split_ram:true ~rom;
  [%expect
    {|
      [ 0: 1e  1: 2f  2: e0  3: f0  4: 02  5: 02  6: 02  7: 02  8: 02  9: 02
      10: 02 11: 02 12: 02 13: 02 14: ff
      15: 45 ]
      ┌Signals─────┐┌Waves─────────────────────────────────────────────────────────────────────────────────────────┐
      │clock       ││┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─│
      │            ││  └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ │
      │reset       ││                                                                                              │
      │            ││──────────────────────────────────────────────────────────────────────────────────────────────│
      │enable      ││    ┌─────────────────────────────────────────────────────────────────────────────────────────│
      │            ││────┘                                                                                         │
      │cpu_reset   ││                                                                            ┌─────────────────│
      │            ││────────────────────────────────────────────────────────────────────────────┘                 │
      │            ││────────────────────┬───────────────────────┬─────────────────────────────────────────────────│
      │a_data      ││ 00                 │FF                     │44                                               │
      │            ││────────────────────┴───────────────────────┴─────────────────────────────────────────────────│
      │            ││────────────────────────────────────────┬─────────────────────────────────────────────────────│
      │b_data      ││ 00                                     │45                                                   │
      │            ││────────────────────────────────────────┴─────────────────────────────────────────────────────│
      │carry       ││                                            ┌─────────────────────────────────────────────────│
      │            ││────────────────────────────────────────────┘                                                 │
      │            ││────────────────────────────────────────────────────────┬─────────────────────────────────────│
      │output_data ││ 00                                                     │44                                   │
      │            ││────────────────────────────────────────────────────────┴─────────────────────────────────────│
      │            ││────────────┬───────────────────┬───────────────────┬───────────────────┬───────┬─────────────│
      │pc_data     ││ 0          │1                  │2                  │3                  │4      │0            │
      │            ││────────────┴───────────────────┴───────────────────┴───────────────────┴───────┴─────────────│
      │ready       ││──────────────────────────────────────────────────────────────────────────────────────────────│
      │            ││                                                                                              │
      │zero        ││                                                                                              │
      │            ││──────────────────────────────────────────────────────────────────────────────────────────────│
      │            ││────────────────────┬───────────────────────┬─────────────────────────────────────────────────│
      │A           ││ 00                 │FF                     │44                                               │
      │            ││────────────────────┴───────────────────────┴─────────────────────────────────────────────────│
      │            ││────────────────┬───────────┬───────┬───────────┬───────────────────┬───────────────┬─────────│
      │ADDR        ││ 0              │E          │1      │F          │2                  │3              │0        │
      │            ││────────────────┴───────────┴───────┴───────────┴───────────────────┴───────────────┴─────────│
      │            ││────────────────────────────────────────┬─────────────────────────────────────────────────────│
      │B           ││ 00                                     │45                                                   │
      │            ││────────────────────────────────────────┴─────────────────────────────────────────────────────│
      │            ││────────────┬───────────────────┬───────────────────┬───────────────────┬───────┬─────────────│
      │PC          ││ 0          │1                  │2                  │3                  │4      │0            │
      │            ││────────────┴───────────────────┴───────────────────┴───────────────────┴───────┴─────────────│
      │            ││────────┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───────┬───┬───┬─────────────────────│
      │bus         ││ 00     │1E │0E │FF │00 │01 │2F │0F │45 │44 │02 │E0 │44 │00     │03 │F0 │00                   │
      │            ││────────┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───────┴───┴───┴─────────────────────│
      │            ││────────┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬─────────────│
      │cycle       ││ 0      │1  │2  │3  │4  │0  │1  │2  │3  │4  │0  │1  │2  │3  │4  │0  │1  │2  │3  │0            │
      │            ││────────┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴─────────────│
      │gnd         ││                                                                                              │
      │            ││──────────────────────────────────────────────────────────────────────────────────────────────│
      │halt        ││                                                                            ┌───┐             │
      │            ││────────────────────────────────────────────────────────────────────────────┘   └─────────────│
      └────────────┘└──────────────────────────────────────────────────────────────────────────────────────────────┘
   |}]

let cpu_test_state ~rom ~split_ram ~steps =
  let module Cpu = CpuTest ((val cpu_config ~rom ~split_ram)) in
  let print n =
    let carry = if Cpu.carry () != 0 then 'C' else '.' in
    let zero = if Cpu.zero () != 0 then 'Z' else '.' in
    Printf.printf "%-3u PC:%02X A:%02X B:%02X OUT:%02X %c%c\n%!" n (Cpu.pc ()) (Cpu.a ()) (Cpu.b ()) (Cpu.output ())
      zero carry
  in
  print 0;
  for n = 1 to steps do
    Cpu.step ();
    print n
  done

let%expect_test "cpu_test_loop" =
  let rom =
    Isa.(assembler [ lda 14; add 15; out; jc 5; jmp 1; hlt ])
    |> rom_prepare ~process:(fun ~write code -> code |> write 14 0x23 |> write 15 0x45)
  in
  cpu_test_state ~split_ram:false ~rom ~steps:20;
  [%expect
    {|
      [ 0: 1e  1: 2f  2: e0  3: 75  4: 61  5: f0  6: 02  7: 02  8: 02  9: 02
      10: 02 11: 02 12: 02 13: 02 14: 23
      15: 45 ]
      0   PC:00 A:00 B:00 OUT:00 ..
      1   PC:01 A:23 B:00 OUT:00 ..
      2   PC:02 A:68 B:45 OUT:00 ..
      3   PC:03 A:68 B:45 OUT:68 ..
      4   PC:04 A:68 B:45 OUT:68 ..
      5   PC:01 A:68 B:45 OUT:68 ..
      6   PC:02 A:AD B:45 OUT:68 ..
      7   PC:03 A:AD B:45 OUT:AD ..
      8   PC:04 A:AD B:45 OUT:AD ..
      9   PC:01 A:AD B:45 OUT:AD ..
      10  PC:02 A:F2 B:45 OUT:AD ..
      11  PC:03 A:F2 B:45 OUT:F2 ..
      12  PC:04 A:F2 B:45 OUT:F2 ..
      13  PC:01 A:F2 B:45 OUT:F2 ..
      14  PC:02 A:37 B:45 OUT:F2 .C
      15  PC:03 A:37 B:45 OUT:37 .C
      16  PC:05 A:37 B:45 OUT:37 .C
      17  PC:06 A:37 B:45 OUT:37 .C
      18  PC:06 A:37 B:45 OUT:37 .C
      19  PC:06 A:37 B:45 OUT:37 .C
      20  PC:06 A:37 B:45 OUT:37 .C
   |}]

let%expect_test "cpu_executor_test_rom" =
  let split_ram = true in
  let scope = Scope.create ~flatten_design:true () in
  let module Simulator = Cyclesim.With_interface (CpuExecutor.I) (CpuExecutor.O) in
  let rom = List.map (Bits.of_int ~width:Ram.bits) [ 0x01; 0x12; 0x23; 0x34; 0x45; 0x56; 0x76 ] in
  let waves, sim =
    CpuExecutor.create ~rom ~split_ram scope
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
  let step controls =
    List.iter set controls;
    cycles 1;
    List.iter clear controls
  in
  let c = inputs.control in
  let command c = List.iter step c in
  let _NOP =
    [
      [ c._MI; c._CO ] (*                              cycle 1 *);
      [ c._RO; c._II; c._CE ] (*                       cycle 2 *);
    ]
  in
  let _LDA =
    _NOP
    @ [
        [ c._IO; c._MI ] (*                              cycle 3 *);
        [ c._RO; c._AI ] (*                              cycle 4 *);
      ]
  in
  let _ADD =
    _NOP
    @ [
        [ c._IO; c._MI ] (*                              cycle 3 *);
        [ c._RO; c._BI ] (*                              cycle 4 *);
        [ c._EO; c._AI; c._FI ] (*                       cycle 5 *);
      ]
  in
  let _SUB =
    _NOP
    @ [
        [ c._IO; c._MI ] (*                              cycle 3 *);
        [ c._RO; c._BI ] (*                              cycle 4 *);
        [ c._EO; c._AI; c._SU; c._FI ] (*                cycle 5 *);
      ]
  in
  let _STA =
    _NOP
    @ [
        [ c._IO; c._MI ] (*                              cycle 3 *);
        [ c._AO; c._RI ] (*                              cycle 4 *);
      ]
  in
  let _LDI = _NOP @ [ [ c._IO; c._AI ] ] in
  let _JMP = _NOP @ [ [ c._IO; c._J ] ] in
  let _OUT = _NOP @ [ [ c._AO; c._OI ] ] in
  let rec wait n =
    let outputs = Cyclesim.outputs sim in
    let ready = !(outputs.CpuExecutor.O.state.ready) |> Bits.to_int in
    if ready = 0 then (
      cycles 1;
      succ n |> wait)
    else if n = 0 then 0
    else n - 1
  in
  let start_cycle = wait 0 in
  command _NOP;
  command _LDA;
  command _ADD;
  command _SUB;
  command _STA;
  command _LDI;
  command _JMP;
  command _OUT;
  cycles 2;
  set inputs.cpu_reset;
  cycles 4;
  clear inputs.cpu_reset;
  command _NOP;
  command _LDA;
  Hardcaml_waveterm.Waveform.print ~start_cycle ~display_height:76 ~display_width:100 ~wave_width:0 waves;
  [%expect {|
    ┌Signals───────────┐┌Waves─────────────────────────────────────────────────────────────────────────┐
    │clock             ││┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐│
    │                  ││ └┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└│
    │reset             ││                                                                              │
    │                  ││──────────────────────────────────────────────────────────────────────────────│
    │_AI               ││            ┌─┐       ┌─┐       ┌─┐           ┌─┐                             │
    │                  ││────────────┘ └───────┘ └───────┘ └───────────┘ └─────────────────────────────│
    │_AO               ││                                        ┌─┐               ┌─┐                 │
    │                  ││────────────────────────────────────────┘ └───────────────┘ └─────────────────│
    │_BI               ││                    ┌─┐       ┌─┐                                             │
    │                  ││────────────────────┘ └───────┘ └─────────────────────────────────────────────│
    │_BO               ││                                                                              │
    │                  ││──────────────────────────────────────────────────────────────────────────────│
    │_CE               ││    ┌─┐ ┌─┐     ┌─┐       ┌─┐       ┌─┐     ┌─┐   ┌─┐   ┌─┐               ┌─┐ │
    │                  ││────┘ └─┘ └─────┘ └───────┘ └───────┘ └─────┘ └───┘ └───┘ └───────────────┘ └─│
    │_CO               ││  ┌─┐ ┌─┐     ┌─┐       ┌─┐       ┌─┐     ┌─┐   ┌─┐   ┌─┐               ┌─┐ ┌─│
    │                  ││──┘ └─┘ └─────┘ └───────┘ └───────┘ └─────┘ └───┘ └───┘ └───────────────┘ └─┘ │
    │_EO               ││                      ┌─┐       ┌─┐                                           │
    │                  ││──────────────────────┘ └───────┘ └───────────────────────────────────────────│
    │_FI               ││                      ┌─┐       ┌─┐                                           │
    │                  ││──────────────────────┘ └───────┘ └───────────────────────────────────────────│
    │_II               ││    ┌─┐ ┌─┐     ┌─┐       ┌─┐       ┌─┐     ┌─┐   ┌─┐   ┌─┐               ┌─┐ │
    │                  ││────┘ └─┘ └─────┘ └───────┘ └───────┘ └─────┘ └───┘ └───┘ └───────────────┘ └─│
    │_IO               ││          ┌─┐     ┌─┐       ┌─┐       ┌─┐     ┌─┐   ┌─┐                       │
    │                  ││──────────┘ └─────┘ └───────┘ └───────┘ └─────┘ └───┘ └───────────────────────│
    │_J                ││                                                    ┌─┐                       │
    │                  ││────────────────────────────────────────────────────┘ └───────────────────────│
    │_MI               ││  ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐   ┌─┐ ┌─┐   ┌─┐ ┌─┐ ┌─┐   ┌─┐   ┌─┐               ┌─┐ ┌─│
    │                  ││──┘ └─┘ └─┘ └─┘ └─┘ └───┘ └─┘ └───┘ └─┘ └─┘ └───┘ └───┘ └───────────────┘ └─┘ │
    │_OI               ││                                                          ┌─┐                 │
    │                  ││──────────────────────────────────────────────────────────┘ └─────────────────│
    │_RI               ││                                        ┌─┐                                   │
    │                  ││────────────────────────────────────────┘ └───────────────────────────────────│
    │_RO               ││    ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐   ┌─┐ ┌─┐   ┌─┐     ┌─┐   ┌─┐   ┌─┐               ┌─┐ │
    │                  ││────┘ └─┘ └─┘ └─┘ └─┘ └───┘ └─┘ └───┘ └─────┘ └───┘ └───┘ └───────────────┘ └─│
    │_SU               ││                                ┌─┐                                           │
    │                  ││────────────────────────────────┘ └───────────────────────────────────────────│
    │cpu_reset         ││                                                                ┌───────┐     │
    │                  ││────────────────────────────────────────────────────────────────┘       └─────│
    │                  ││──────────────┬─────────┬─────────┬─────────────┬─────────────────────────────│
    │a_data            ││ 00           │23       │57       │12           │06                           │
    │                  ││──────────────┴─────────┴─────────┴─────────────┴─────────────────────────────│
    │                  ││──────────────────────┬─────────┬─────────────────────────────────────────────│
    │b_data            ││ 00                   │34       │45                                           │
    │                  ││──────────────────────┴─────────┴─────────────────────────────────────────────│
    │flags_carry       ││                                                                              │
    │                  ││──────────────────────────────────────────────────────────────────────────────│
    │flags_zero        ││                                                                              │
    │                  ││──────────────────────────────────────────────────────────────────────────────│
    │                  ││──────────┬───────┬─────────┬─────────┬───────┬─────┬───────────────────────┬─│
    │opcode            ││ 0        │1      │2        │3        │4      │5    │7                      │0│
    │                  ││──────────┴───────┴─────────┴─────────┴───────┴─────┴───────────────────────┴─│
    │                  ││────────────────────────────────────────────────────────────┬─────────────────│
    │output_data       ││ 00                                                         │06               │
    │                  ││────────────────────────────────────────────────────────────┴─────────────────│
    │                  ││──────┬───┬───────┬─────────┬─────────┬───────┬─────┬─┬───┬───────┬─────────┬─│
    │pc_data           ││ 0    │1  │2      │3        │4        │5      │6    │7│6  │7      │0        │1│
    │                  ││──────┴───┴───────┴─────────┴─────────┴───────┴─────┴─┴───┴───────┴─────────┴─│
    │ready             ││──────────────────────────────────────────────────────────────────────────────│
    │                  ││                                                                              │
    │                  ││──────────────┬─────────┬─────────┬─────────────┬─────────────────────────────│
    │A                 ││ 00           │23       │57       │12           │06                           │
    │                  ││──────────────┴─────────┴─────────┴─────────────┴─────────────────────────────│
    │                  ││────────┬───┬───────┬─────────┬─────────┬─────────┬───────────────────────┬───│
    │ADDR              ││ 0      │1  │2      │3        │4        │5        │6                      │0  │
    │                  ││────────┴───┴───────┴─────────┴─────────┴─────────┴───────────────────────┴───│
    │                  ││──────────────────────┬─────────┬─────────────────────────────────────────────│
    │B                 ││ 00                   │34       │45                                           │
    │                  ││──────────────────────┴─────────┴─────────────────────────────────────────────│
    │                  ││──────┬───┬───────┬─────────┬─────────┬───────┬─────┬─┬───┬───────┬─────────┬─│
    │PC                ││ 0    │1  │2      │3        │4        │5      │6    │7│6  │7      │0        │1│
    │                  ││──────┴───┴───────┴─────────┴─────────┴───────┴─────┴─┴───┴───────┴─────────┴─│
    │                  ││────┬───┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬───┬─┬───┬─┬─┬─────────────┬───│
    │bus               ││ 00 │01 │.│.│.│.│.│.│.│.│.│.│.│.│.│.│.│.│.│.│.│06 │.│06 │.│.│00           │01 │
    │                  ││────┴───┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴───┴─┴───┴─┴─┴─────────────┴───│
    └──────────────────┘└──────────────────────────────────────────────────────────────────────────────┘ |}]

let%expect_test "cpu_executor_test_ram" =
  let split_ram = false in
  let scope = Scope.create ~flatten_design:true () in
  let module Simulator = Cyclesim.With_interface (CpuExecutor.I) (CpuExecutor.O) in
  let rom = List.map (Bits.of_int ~width:Ram.bits) [ 0x01; 0x12; 0x23; 0x34; 0x45; 0x56; 0x76 ] in
  let waves, sim =
    CpuExecutor.create ~rom ~split_ram scope
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
  let step controls =
    List.iter set controls;
    cycles 1;
    List.iter clear controls
  in
  let c = inputs.control in
  let command c = List.iter step c in
  let _NOP =
    [
      [ c._MI; c._CO ] (*                              cycle 1 *);
      [ c._RO; c._II; c._CE ] (*                       cycle 2 *);
    ]
  in
  let _LDA =
    _NOP
    @ [
        [ c._IO; c._MI ] (*                              cycle 3 *);
        [ c._RO; c._AI ] (*                              cycle 4 *);
      ]
  in
  let _ADD =
    _NOP
    @ [
        [ c._IO; c._MI ] (*                              cycle 3 *);
        [ c._RO; c._BI ] (*                              cycle 4 *);
        [ c._EO; c._AI; c._FI ] (*                       cycle 5 *);
      ]
  in
  let _SUB =
    _NOP
    @ [
        [ c._IO; c._MI ] (*                              cycle 3 *);
        [ c._RO; c._BI ] (*                              cycle 4 *);
        [ c._EO; c._AI; c._SU; c._FI ] (*                cycle 5 *);
      ]
  in
  let _STA =
    _NOP
    @ [
        [ c._IO; c._MI ] (*                              cycle 3 *);
        [ c._AO; c._RI ] (*                              cycle 4 *);
      ]
  in
  let _LDI = _NOP @ [ [ c._IO; c._AI ] ] in
  let _JMP = _NOP @ [ [ c._IO; c._J ] ] in
  let _OUT = _NOP @ [ [ c._AO; c._OI ] ] in
  let rec wait n =
    let outputs = Cyclesim.outputs sim in
    let ready = !(outputs.CpuExecutor.O.state.ready) |> Bits.to_int in
    if ready = 0 then (
      cycles 1;
      succ n |> wait)
    else if n = 0 then 0
    else n - 1
  in
  let start_cycle = wait 0 in
  command _NOP;
  command _LDA;
  command _ADD;
  command _SUB;
  command _STA;
  command _LDI;
  command _JMP;
  command _OUT;
  cycles 2;
  set inputs.cpu_reset;
  cycles 4;
  clear inputs.cpu_reset;
  command _NOP;
  command _LDA;
  Hardcaml_waveterm.Waveform.print ~start_cycle ~display_height:76 ~display_width:100 ~wave_width:0 waves;
  [%expect {|
    ┌Signals───────────┐┌Waves─────────────────────────────────────────────────────────────────────────┐
    │clock             ││┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐│
    │                  ││ └┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└│
    │reset             ││                                                                              │
    │                  ││──────────────────────────────────────────────────────────────────────────────│
    │_AI               ││            ┌─┐       ┌─┐       ┌─┐           ┌─┐                             │
    │                  ││────────────┘ └───────┘ └───────┘ └───────────┘ └─────────────────────────────│
    │_AO               ││                                        ┌─┐               ┌─┐                 │
    │                  ││────────────────────────────────────────┘ └───────────────┘ └─────────────────│
    │_BI               ││                    ┌─┐       ┌─┐                                             │
    │                  ││────────────────────┘ └───────┘ └─────────────────────────────────────────────│
    │_BO               ││                                                                              │
    │                  ││──────────────────────────────────────────────────────────────────────────────│
    │_CE               ││    ┌─┐ ┌─┐     ┌─┐       ┌─┐       ┌─┐     ┌─┐   ┌─┐   ┌─┐               ┌─┐ │
    │                  ││────┘ └─┘ └─────┘ └───────┘ └───────┘ └─────┘ └───┘ └───┘ └───────────────┘ └─│
    │_CO               ││  ┌─┐ ┌─┐     ┌─┐       ┌─┐       ┌─┐     ┌─┐   ┌─┐   ┌─┐               ┌─┐ ┌─│
    │                  ││──┘ └─┘ └─────┘ └───────┘ └───────┘ └─────┘ └───┘ └───┘ └───────────────┘ └─┘ │
    │_EO               ││                      ┌─┐       ┌─┐                                           │
    │                  ││──────────────────────┘ └───────┘ └───────────────────────────────────────────│
    │_FI               ││                      ┌─┐       ┌─┐                                           │
    │                  ││──────────────────────┘ └───────┘ └───────────────────────────────────────────│
    │_II               ││    ┌─┐ ┌─┐     ┌─┐       ┌─┐       ┌─┐     ┌─┐   ┌─┐   ┌─┐               ┌─┐ │
    │                  ││────┘ └─┘ └─────┘ └───────┘ └───────┘ └─────┘ └───┘ └───┘ └───────────────┘ └─│
    │_IO               ││          ┌─┐     ┌─┐       ┌─┐       ┌─┐     ┌─┐   ┌─┐                       │
    │                  ││──────────┘ └─────┘ └───────┘ └───────┘ └─────┘ └───┘ └───────────────────────│
    │_J                ││                                                    ┌─┐                       │
    │                  ││────────────────────────────────────────────────────┘ └───────────────────────│
    │_MI               ││  ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐   ┌─┐ ┌─┐   ┌─┐ ┌─┐ ┌─┐   ┌─┐   ┌─┐               ┌─┐ ┌─│
    │                  ││──┘ └─┘ └─┘ └─┘ └─┘ └───┘ └─┘ └───┘ └─┘ └─┘ └───┘ └───┘ └───────────────┘ └─┘ │
    │_OI               ││                                                          ┌─┐                 │
    │                  ││──────────────────────────────────────────────────────────┘ └─────────────────│
    │_RI               ││                                        ┌─┐                                   │
    │                  ││────────────────────────────────────────┘ └───────────────────────────────────│
    │_RO               ││    ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐   ┌─┐ ┌─┐   ┌─┐     ┌─┐   ┌─┐   ┌─┐               ┌─┐ │
    │                  ││────┘ └─┘ └─┘ └─┘ └─┘ └───┘ └─┘ └───┘ └─────┘ └───┘ └───┘ └───────────────┘ └─│
    │_SU               ││                                ┌─┐                                           │
    │                  ││────────────────────────────────┘ └───────────────────────────────────────────│
    │cpu_reset         ││                                                                ┌───────┐     │
    │                  ││────────────────────────────────────────────────────────────────┘       └─────│
    │                  ││──────────────┬─────────┬─────────┬─────────────┬─────────────────────────────│
    │a_data            ││ 00           │23       │57       │12           │02                           │
    │                  ││──────────────┴─────────┴─────────┴─────────────┴─────────────────────────────│
    │                  ││──────────────────────┬─────────┬─────────────────────────────────────────────│
    │b_data            ││ 00                   │34       │45                                           │
    │                  ││──────────────────────┴─────────┴─────────────────────────────────────────────│
    │flags_carry       ││                                                                              │
    │                  ││──────────────────────────────────────────────────────────────────────────────│
    │flags_zero        ││                                                                              │
    │                  ││──────────────────────────────────────────────────────────────────────────────│
    │                  ││──────────┬───────┬─────────┬─────────┬───────┬─────┬───────────────────────┬─│
    │opcode            ││ 0        │1      │2        │3        │4      │1    │7                      │0│
    │                  ││──────────┴───────┴─────────┴─────────┴───────┴─────┴───────────────────────┴─│
    │                  ││────────────────────────────────────────────────────────────┬─────────────────│
    │output_data       ││ 00                                                         │02               │
    │                  ││────────────────────────────────────────────────────────────┴─────────────────│
    │                  ││──────┬───┬───────┬─────────┬─────────┬───────┬─────┬─┬───┬───────┬─────────┬─│
    │pc_data           ││ 0    │1  │2      │3        │4        │5      │6    │7│6  │7      │0        │1│
    │                  ││──────┴───┴───────┴─────────┴─────────┴───────┴─────┴─┴───┴───────┴─────────┴─│
    │ready             ││  ┌───────────────────────────────────────────────────────────────────────────│
    │                  ││──┘                                                                           │
    │                  ││──────────────┬─────────┬─────────┬─────────────┬─────────────────────────────│
    │A                 ││ 00           │23       │57       │12           │02                           │
    │                  ││──────────────┴─────────┴─────────┴─────────────┴─────────────────────────────│
    │                  ││────────┬───┬───────┬─────────┬─────────┬─────────┬───────────────────────┬───│
    │ADDR              ││ 0      │1  │2      │3        │4        │5        │6                      │0  │
    │                  ││────────┴───┴───────┴─────────┴─────────┴─────────┴───────────────────────┴───│
    │                  ││──────────────────────┬─────────┬─────────────────────────────────────────────│
    │B                 ││ 00                   │34       │45                                           │
    │                  ││──────────────────────┴─────────┴─────────────────────────────────────────────│
    │                  ││──────┬───┬───────┬─────────┬─────────┬───────┬─────┬─┬───┬───────┬─────────┬─│
    │PC                ││ 0    │1  │2      │3        │4        │5      │6    │7│6  │7      │0        │1│
    │                  ││──────┴───┴───────┴─────────┴─────────┴───────┴─────┴─┴───┴───────┴─────────┴─│
    │                  ││────┬───┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬───┬─┬─┬─────────────┬───│
    │bus               ││ 00 │01 │.│.│.│.│.│.│.│.│.│.│.│.│.│.│.│.│.│.│.│.│.│.│06 │.│.│00           │01 │
    │                  ││────┴───┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴───┴─┴─┴─────────────┴───│
    └──────────────────┘└──────────────────────────────────────────────────────────────────────────────┘ |}]
