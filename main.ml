open Hardcaml
open Clock

let to_file name = "_build/" ^ name ^ ".v" |> Rtl.Output_mode.To_file

let build_clock () =
  let scope = Scope.create () in
  let name = "clock" in
  let output_mode = to_file name in
  let circuit =
    let reset = Signal.input "reset" 1 in
    let clock = { clock = 100_000_000; wire = Signal.input "clock" 1 } in
    let anode, segment = clock_top ~clock ~reset ~refresh:1000 ~tick:100 in
    Circuit.create_exn ~name [ Signal.output "anode" anode; Signal.output "segment" segment ]
  in
  Rtl.output ~output_mode ~database:(Scope.circuit_database scope) Verilog circuit

let build_reset () =
  let scope = Scope.create () in
  let name = "reset" in
  let output_mode = to_file name in
  let circuit =
    let activate = Signal.input "activate" 1 in
    let clock = Signal.input "clock" 1 in
    let reset = pulse ~reset:activate ~clock ~length:2 in
    Circuit.create_exn ~name [ Signal.output "reset" reset ]
  in
  Rtl.output ~output_mode ~database:(Scope.circuit_database scope) Verilog circuit

let build_led () =
  let scope = Scope.create () in
  let name = "led" in
  let output_mode = to_file name in
  let circuit =
    let clock = Signal.input "clock" 1 in
    let reset = Signal.input "reset" 1 in
    let value = Signal.input "value" 8 in
    let control = Led.pwm ~reset ~clock ~base:256 ~value in
    Circuit.create_exn ~name [ Signal.output "control" control ]
  in
  Rtl.output ~output_mode ~database:(Scope.circuit_database scope) Verilog circuit

let () =
  build_clock ();
  build_reset ();
  build_led ()
