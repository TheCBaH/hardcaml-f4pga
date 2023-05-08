open Hardcaml

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

module Alu = struct
  let bits = 8

  module I = struct
    type 'a t = { op : 'a; a : 'a; [@bits bits] b : 'a [@bits bits] } [@@deriving sexp_of, hardcaml]
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
    let result = mux2 i.I.op add sub in
    let carry = msb result in
    let data = lsbs result in
    let zero = data ==:. 0 in
    { O.data; carry; zero }
end

let alu_test =
  let scope = Scope.create ~flatten_design:true () in
  let module Simulator = Cyclesim.With_interface (Alu.I) (Alu.O) in
  let waves, sim =
    Alu.create scope |> Simulator.create ~config:Cyclesim.Config.trace_all |> Hardcaml_waveterm.Waveform.create
  in
  let inputs = Cyclesim.inputs sim in
  let set w v = w := Bits.of_int ~width:Alu.bits v in
  let alu op a b =
    set inputs.a a;
    set inputs.b b;
    inputs.op := Bits.of_int ~width:1 op;
    Cyclesim.cycle sim
  in
  alu 1 0x2 0x4;
  alu 0 0x6 0x1;
  alu 0 0x3 0x3;
  alu 0 0x1 0x3;
  alu 1 0x70 0x80;
  alu 1 0xF0 0x10;
  alu 1 0xF0 0x20;
  Hardcaml_waveterm.Waveform.print ~display_height:18 ~display_width:80 ~wave_width:1 waves
