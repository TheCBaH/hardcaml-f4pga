open Hardcaml

let pulse ?scope ~reset ~clock ~length () =
  let scope = Option.value ~default:(Scope.create ()) scope in
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
  let pulse = Pulse.create ~length:4 scope in
  let config = {Cyclesim.Config.default with is_internal_port = Some (fun s -> Signal.names s |> List.exists (String.starts_with ~prefix:"count") )} in
  let sim = Simulator.create ~config pulse in
  let waves, sim = Hardcaml_waveterm.Waveform.create sim in
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
