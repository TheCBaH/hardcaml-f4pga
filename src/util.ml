open Hardcaml

let pulse ~reset ~clock ~length =
  assert (length > 0);
  let bits = Base.Int.ceil_log2 length in
  let open Signal in
  let count = wire bits in
  let max = length - 1 in
  let result = count <>:. max in
  let count_succ = mux2 result (count +:. 1) count in
  (* let next = mux2 reset (zero bits) count_succ in *)
  let spec = Reg_spec.create ~clock ~clear:reset () in
  count <== reg spec count_succ;
  result

let pulse_test =
  let _clock = "clock" in
  let _reset = "_reset" in
  let clock = Signal.input _clock 1 in
  let reset = Signal.input _reset 1 in
  let pulse = pulse ~reset ~clock ~length:4 in
  let circuit = Circuit.create_exn ~name:"pulse" [ Signal.output "pulse" pulse ] in
  let waves, sim = Hardcaml_waveterm.Waveform.create (Cyclesim.create circuit) in
  let cycles n =
    for _ = 0 to n do
      Cyclesim.cycle sim
    done
  in
  let set wire = Cyclesim.in_port sim wire := Bits.vdd in
  let clear wire = Cyclesim.in_port sim wire := Bits.gnd in
  cycles 1;
  set _reset;
  cycles 1;
  clear _reset;
  cycles 4;
  set _reset;
  cycles 3;
  clear _reset;
  cycles 4;
  set _reset;
  cycles 1;
  clear _reset;
  cycles 1;
  set _reset;
  cycles 1;
  clear _reset;
  cycles 5;
  Hardcaml_waveterm.Waveform.print ~display_height:8 ~display_width:80 ~wave_width:0 waves

module Pulse = struct
  module I = struct
    type 'a t = { clock : 'a [@bits 1]; reset : 'a [@bits 1]; } [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t = { pulse : 'a [@bits 1] } [@@deriving sexp_of, hardcaml]
  end

  let create ~length (_scope : Scope.t) (input : Signal.t I.t) =
    { O.pulse = pulse ~reset:input.reset ~clock:input.clock ~length }

  let hierarchical ~length scope input =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"pulse" (create ~length) input
end
