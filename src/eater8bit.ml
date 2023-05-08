open Hardcaml

(*
#require "ppx_jane";;
*)

module Register = struct
  let bits = 8

  module I = struct
    type 'a t = { clock : 'a; reset : 'a; w_en : 'a; i_data : 'a [@bits bits] } [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t = { data : 'a [@bits bits] } [@@deriving sexp_of, hardcaml]
  end

  let create scope i =
    let ( -- ) = Scope.naming scope in
    let spec = Reg_spec.create ~clock:i.I.clock ~clear:i.I.reset () in
    let register = Signal.reg_fb ~enable:i.I.w_en ~width:bits ~f:(fun _ -> i.I.i_data) spec -- "register" in
    { O.data = register }
end

let register_test =
  let scope = Scope.create ~flatten_design:true () in
  let module Simulator = Cyclesim.With_interface (Register.I) (Register.O) in
  let waves, sim =
    Register.create scope |> Simulator.create ~config:Cyclesim.Config.trace_all |> Hardcaml_waveterm.Waveform.create
  in
  let inputs = Cyclesim.inputs sim in
  let set wire = wire := Bits.vdd in
  let clear wire = wire := Bits.gnd in
  let data v = inputs.i_data := Bits.of_int ~width:Register.bits v in
  let cycles n =
    for _ = 0 to n do
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
  Hardcaml_waveterm.Waveform.print ~display_height:18 ~display_width:80 ~wave_width:0 waves

module Operation = struct
  module Code = struct
    type t = Sub | Add [@@deriving sexp_of, enumerate]
  end
end

module Alu = struct
  let bits = 8

  module Code = struct
    type t = Sub | Add [@@deriving sexp_of, compare, enumerate]
  end

  include Enum.Make_enums (Code)

  module I = struct
    type 'a t = { op : 'a Binary.t; [@rtlmangle true] a : 'a; [@bits bits] b : 'a [@bits bits] }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t = { data : 'a; [@bits bits] carry : 'a; zero : 'a } [@@deriving sexp_of, hardcaml]
  end

  let create scope i =
    ignore scope;
    let open Signal in
    let a = succ bits |> Signal.uresize i.I.a in
    let b = succ bits |> Signal.uresize i.I.b in
    let add = a +: b in
    let sub = a -: b in
    let result = Binary.Of_signal.match_ i.I.op [ (Add, add); (Sub, sub) ] in
    let carry = msb result in
    let data = lsbs result in
    let zero = data ==:. 0 in
    { O.data; carry; zero }
end

let alu_test =
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
  Hardcaml_waveterm.Waveform.print ~display_height:18 ~display_width:80 ~wave_width:1 waves

module Memory = struct
  let bits = 8
  let size = 16
  let bits_addr = size - 1 |> Bits.num_bits_to_represent

  module I = struct
    type 'a t = { clock : 'a; w_en : 'a; w_data : 'a; [@bits bits] addr : 'a [@bits bits_addr] }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t = { data : 'a [@bits bits] } [@@deriving sexp_of, hardcaml]
  end

  let memory i =
    let write_port =
      { Signal.write_clock = i.I.clock; write_address = i.addr; write_enable = i.w_en; write_data = i.w_data }
    in
    Signal.memory ~write_port ~read_address:i.I.addr size

  let create scope i =
    ignore scope;
    let memory =
      let write_port =
        { Signal.write_clock = i.I.clock; write_address = i.addr; write_enable = i.w_en; write_data = i.w_data }
      in
      Signal.memory ~write_port ~read_address:i.I.addr size
    in
    { O.data = memory }
end

let memory_test =
  let scope = Scope.create ~flatten_design:true () in
  let module Simulator = Cyclesim.With_interface (Memory.I) (Memory.O) in
  let waves, sim = Memory.create scope |> Simulator.create |> Hardcaml_waveterm.Waveform.create in
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
  Hardcaml_waveterm.Waveform.print ~display_height:18 ~display_width:80 ~wave_width:1 waves
