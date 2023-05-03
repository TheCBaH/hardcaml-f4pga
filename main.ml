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

let simulate_led ?cycles () =
  let cycles = Option.value ~default:100_000 cycles in
  let scope = Scope.create ~auto_label_hierarchical_ports:true ~flatten_design:true () in
  let simulator circuit =
    if cycles > 100_000 then
      let module Simulator = Hardcaml_verilator.With_interface (Led.LedTop.I) (Led.LedTop.O) in
      Simulator.create ~clock_names:[ "clock" ] ~config:Cyclesim.Config.trace_all ~verbose:true circuit
    else
      let module Simulator = Cyclesim.With_interface (Led.LedTop.I) (Led.LedTop.O) in
      Simulator.create ~config:Cyclesim.Config.trace_all circuit
  in
  let waves, sim =
    let clock_freq, refresh, tick = (20_000, 10_000, 40) in
    Led.LedTop.create ~clock_freq ~refresh ~tick scope |> simulator |> Hardcaml_waveterm.Waveform.create
  in
  for _i = 0 to cycles do
    Cyclesim.cycle sim
  done;
  Hardcaml_waveterm_interactive.run waves

let build_led () =
  let scope = Scope.create () in
  let name = "led_top" in
  let clock_freq = 100_000_000 in
  let refresh = 10_000 in
  let tick = 1 in
  let module TopCircuit = Circuit.With_interface (Led.LedTop.I) (Led.LedTop.O) in
  let circuit = Led.LedTop.hierarchical ~clock_freq ~refresh ~tick scope |> TopCircuit.create_exn ~name in
  let output_mode = to_directory in
  Rtl.output ~output_mode ~database:(Scope.circuit_database scope) Verilog circuit

let main () =
  let cycles = ref 0 in
  let circuit = ref "led" in
  let mode = ref "" in
  let options =
    [
      ("-cycles", Arg.Set_int cycles, "cycles to run simulator");
      ("-circuit", Arg.Set_string circuit, "circuit to simulate");
    ]
  in
  Arg.parse options (fun s -> mode := s) "run rtl generator";
  match !mode with
  | "simulator" ->
      let cycles = if !cycles > 0 then Some !cycles else None in
      simulate_led ?cycles ()
  | "" ->
      build_clock ();
      build_reset ();
      build_led ()
  | _ -> "Not supported mode " ^ !mode |> failwith

let () = main ()
