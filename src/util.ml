open Hardcaml

let pulse ~scope ~reset ~clock ~length () =
  assert (length > 0);
  let bits = Base.Int.ceil_log2 length in
  let open Signal in
  let (--) = Scope.naming scope in
  let count = wire bits -- "count" in
  let max = length - 1 in
  let result = count <>:. max in
  let count_succ = mux2 result (count +:. 1) count -- "count_succ" in
  (* let next = mux2 reset (zero bits) count_succ in *)
  let spec = Reg_spec.create ~clock ~clear:reset () in
  count <== reg spec count_succ;
  result

module Pulse = struct
  module I = struct
    type 'a t = { clock : 'a [@bits 1]; reset : 'a [@bits 1] [@rtlsuffix "_"]; } [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t = { pulse : 'a [@bits 1] } [@@deriving sexp_of, hardcaml]
  end

  let create ~length (scope : Scope.t) (input : Signal.t I.t) =
    { O.pulse = pulse ~scope ~reset:input.reset ~clock:input.clock ~length () }

  let hierarchical ~length scope input =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"pulse" (create ~length) input
end

let pulse_test =
  let scope = Scope.create ~flatten_design:true () in
  let module Simulator = Cyclesim.With_interface (Pulse.I) (Pulse.O) in
  let config = {Cyclesim.Config.default with is_internal_port = Some (fun s -> Signal.names s |> List.exists (String.starts_with ~prefix:"count") )} in
  let waves, sim = Pulse.create ~length:4 scope |> Simulator.create ~config |> Hardcaml_waveterm.Waveform.create in
  let inputs = Cyclesim.inputs sim in
  let cycles n =
    for _ = 0 to n do
      Cyclesim.cycle sim
    done
  in
  let set wire = wire := Bits.vdd in
  let clear wire =  wire := Bits.gnd in
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
  Hardcaml_waveterm.Waveform.print ~display_height:18 ~display_width:80 ~wave_width:0 waves;
  Cyclesim.circuit sim

let pwm ~scope ~clock ~reset ~enable ~base ~value () =
  assert (base > 0);
  let bits = Base.Int.ceil_log2 base in
  let open Signal in
  let (--) = Scope.naming scope in
  let count = wire bits -- "count" in
  let count_succ =
    let incr = count +:. 1 in
    if base = 1 lsl bits then incr else mux2 (count <>:. base - 1) incr (zero bits)
  in
  let count_succ = mux2 enable count_succ count in
  let spec = Reg_spec.create ~clock ~clear:reset () in
  count <== reg spec count_succ;
  let value = uresize value bits in
  let control = count <: value &: ~:reset in
  control

module Pwm = struct
  module I = struct
    type 'a t = { clock : 'a [@bits 1];
    reset : 'a [@bits 1] [@rtlsuffix "_"];
    enable : 'a [@bits 1];
    value: 'a [@bits 8];
   } [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t = { control : 'a [@bits 1] } [@@deriving sexp_of, hardcaml]
  end

  let create ~base scope (input : Signal.t I.t) =
    { O.control = pwm ~scope ~reset:input.reset ~clock:input.clock ~enable:input.enable ~value:input.value ~base () }

  let hierarchical ~base scope input =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"pulse" (create ~base) input
end

let pwm_test_1 =
  let scope = Scope.create ~flatten_design:true () in
  let module Simulator = Cyclesim.With_interface (Pwm.I) (Pwm.O) in
  let config = {Cyclesim.Config.default with is_internal_port = Some (fun s -> Signal.names s |> List.exists (String.starts_with ~prefix:"count") )} in
  let waves, sim = Pwm.create ~base:4 scope |> Simulator.create ~config |> Hardcaml_waveterm.Waveform.create in
  let cycles n =
    for _ = 0 to n do
      Cyclesim.cycle sim
    done
  in
  let inputs = Cyclesim.inputs sim in
  let set wire = wire := Bits.vdd in
  let clear wire =  wire := Bits.gnd in
  let set_value v =
    inputs.value := Bits.of_int ~width:(Bits.width !(inputs.value)) v in
  set_value 2;
  cycles 1;
  set inputs.enable;
  cycles 5;
  set inputs.reset;
  cycles 2;
  clear inputs.reset;
  cycles 2;
  set_value 1;
  cycles 5;
  set_value 3;
  cycles 5;
  set_value 0;
  cycles 1;
  set_value 3;
  cycles 2;
  Hardcaml_waveterm.Waveform.print ~display_height:16 ~display_width:84 ~wave_width:0 waves

let pwm_test_2 =
  let scope = Scope.create ~flatten_design:true () in
  let module Simulator = Cyclesim.With_interface (Pwm.I) (Pwm.O) in
  let waves, sim = Pwm.create ~base:4 scope |> Simulator.create |> Hardcaml_waveterm.Waveform.create in
  let cycles n =
    for _ = 0 to n do
      Cyclesim.cycle sim
    done
  in
  let inputs = Cyclesim.inputs sim in
  let set wire = wire := Bits.vdd in
  let clear wire =  wire := Bits.gnd in
  let set_value v =
    inputs.value := Bits.of_int ~width:(Bits.width !(inputs.value)) v in
  set inputs.enable;
  set_value 2;
  cycles 4;
  set inputs.reset;
  cycles 1;
  clear inputs.reset;
  cycles 1;
  set_value 1;
  cycles 5;
  set_value 0;
  cycles 5;
  set_value 3;
  cycles 7;
  Hardcaml_waveterm.Waveform.print ~display_height:14 ~display_width:84 ~wave_width:0 waves
