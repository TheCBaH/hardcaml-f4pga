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
    let anode, segment, dot = clock_top ~clock ~reset ~refresh:1000 ~tick:100 in
    Circuit.create_exn ~name [ Signal.output "anode" anode; Signal.output "segment" segment; Signal.output "dot" dot ]
  in
  Rtl.output ~output_mode ~database:(Scope.circuit_database scope) Verilog circuit

let build_reset () =
  let scope = Scope.create () in
  let name = "reset" in
  let circuit = Reset.hierarchical scope |> ResetCircuit.create_exn ~name in
  let output_mode = to_file name in
  Rtl.output ~output_mode ~database:(Scope.circuit_database scope) Verilog circuit

let build_led () =
  let scope = Scope.create () in
  let name = "led_top" in
  let module TopCircuit = Circuit.With_interface (Led.LedTop.I) (Led.LedTop.O) in
  let circuit = Led.LedTop.hierarchical ~clock_freq:10_000_000 scope |> TopCircuit.create_exn ~name in
  let output_mode = to_file name in
  if false then (
    let scope = Scope.create ~flatten_design:true () in
    let circuit = Led.LedTop.hierarchical ~clock_freq:10_000_000 scope |> TopCircuit.create_exn ~name in
    let waves, sim = Hardcaml_waveterm.Waveform.create (Cyclesim.create circuit) in
    for _i = 0 to 1000 do
      Cyclesim.cycle sim
    done;
    Hardcaml_waveterm_interactive.run waves);
  Rtl.output ~output_mode ~database:(Scope.circuit_database scope) Verilog circuit

let () =
  (* build_clock (); *)
  build_reset ();
  if false then build_led ();
