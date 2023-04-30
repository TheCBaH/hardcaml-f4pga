open Hardcaml
open Clock

let to_directory = Rtl.Output_mode.In_directory "_build"

let build_clock () =
  let scope = Scope.create () in
  let name = "clock" in
  let output_mode = to_directory in
  let circuit =
    let reset = Signal.input "reset" 1 in
    let clock = { clock = 100_000_000; wire = Signal.input "clock" 1 } in
    let anode, segment, dot = clock_top ~clock ~reset ~refresh:1000 ~tick:100 in
    Circuit.create_exn ~name [ Signal.output "anode" anode; Signal.output "segment" segment; Signal.output "dot" dot ]
  in
  Rtl.output ~output_mode ~database:(Scope.circuit_database scope) Verilog circuit

let build_reset () =
  let scope = Scope.create () in
  let name = "reset_top" in
  let circuit = Reset.hierarchical scope |> ResetCircuit.create_exn ~name in
  let output_mode = to_directory in
  Rtl.output ~output_mode ~database:(Scope.circuit_database scope) Verilog circuit

let build_led () =
  let scope = Scope.create () in
  let name = "led_top" in
  let clock_freq = 100_000_000 in
  if true then (
    let scope = Scope.create ~auto_label_hierarchical_ports:true ~flatten_design:true () in
    let simulator circuit =
      let module Simulator = Cyclesim.With_interface (Led.LedTop.I) (Led.LedTop.O) in
      Simulator.create ~config:Cyclesim.Config.trace_all circuit in
    let waves, sim = Led.LedTop.create ~clock_freq scope |> simulator |> Hardcaml_waveterm.Waveform.create in
(*
    let scope = Scope.create ~flatten_design:true () in
    let circuit = Led.LedTop.hierarchical ~clock_freq scope |> TopCircuit.create_exn ~name in
    let cyclesim circuit = Cyclesim.create ~config:Cyclesim.Config.trace_all circuit in

    let verilator circuit = Hardcaml_verilator.create ~config:Cyclesim.Config.trace_all ~clock_names:["clock"] ~verbose:true circuit in
    let simulator = if false then verilator else cyclesim in
    (*
    let c = Hardcaml_c.Cyclesim.create in
    ignore c;
    *)
    let waves, sim = Hardcaml_waveterm.Waveform.create (simulator circuit) in
*)
    for _i = 0 to 10_000 do
      Cyclesim.cycle sim
    done;
    Hardcaml_waveterm_interactive.run waves);
  let module TopCircuit = Circuit.With_interface (Led.LedTop.I) (Led.LedTop.O) in
  let circuit = Led.LedTop.hierarchical ~clock_freq scope |> TopCircuit.create_exn ~name in
  let output_mode = to_directory in
  Rtl.output ~output_mode ~database:(Scope.circuit_database scope) Verilog circuit

let () =
  build_clock ();
  build_reset ();
  build_led ()
