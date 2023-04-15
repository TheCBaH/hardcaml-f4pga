open Hardcaml
open Clock

let build_clock () =
  let scope = Scope.create () in
  let output_mode = Rtl.Output_mode.To_file "clock.v" in
  let circuit =
    let reset = Signal.input "reset" 1 in
    let clock = { clock = 100_000_000; wire = Signal.input "clock" 1 } in
    let anode, segment = clock_top ~clock ~reset ~refresh:1000 ~tick:100 in
    Circuit.create_exn ~name:"clock" [ Signal.output "anode" anode; Signal.output "segment" segment ] in
  Rtl.output ~output_mode ~database:(Scope.circuit_database scope) Verilog circuit

let build_reset () =
  let scope = Scope.create () in
  let output_mode = Rtl.Output_mode.To_file "reset.v" in
  let circuit =
    let activate = Signal.input "activate" 1 in
    let clock = Signal.input "clock" 1 in
    let reset = pulse ~reset:activate ~clock ~length:2 in
    Circuit.create_exn ~name:"reset" [ Signal.output "reset" reset] in
  Rtl.output ~output_mode ~database:(Scope.circuit_database scope) Verilog circuit

let () =
    build_clock ();
    build_reset ();
