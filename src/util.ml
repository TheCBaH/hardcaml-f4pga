open Hardcaml

let pulse ~scope ~reset ~clock ~length () =
  assert (length > 0);
  let bits = Base.Int.ceil_log2 length in
  let open Signal in
  let ( -- ) = Scope.naming scope in
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
    type 'a t = { clock : 'a; reset : 'a [@rtlsuffix "_"] } [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t = { pulse : 'a } [@@deriving sexp_of, hardcaml]
  end

  let create ~length (scope : Scope.t) (input : Signal.t I.t) =
    { O.pulse = pulse ~scope ~reset:input.reset ~clock:input.clock ~length () }

  let hierarchical ~length scope input =
    let name = Printf.sprintf "pulse_%u" length in
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name (create ~length) input
end

let pulse_test =
  let scope = Scope.create ~flatten_design:true () in
  let module Simulator = Cyclesim.With_interface (Pulse.I) (Pulse.O) in
  let config =
    {
      Cyclesim.Config.default with
      is_internal_port = Some (fun s -> Signal.names s |> List.exists (String.starts_with ~prefix:"count"));
    }
  in
  let waves, sim = Pulse.create ~length:4 scope |> Simulator.create ~config |> Hardcaml_waveterm.Waveform.create in
  let inputs = Cyclesim.inputs sim in
  let cycles n =
    for _ = 0 to n do
      Cyclesim.cycle sim
    done
  in
  let set wire = wire := Bits.vdd in
  let clear wire = wire := Bits.gnd in
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

let counter_with_carry ?(base = 10) ?bits ~reset ~increment ~clock () =
  let base_bits = Base.Int.ceil_log2 base in
  let bits = Option.value ~default:base_bits bits in
  assert (bits >= base_bits);
  let spec = Reg_spec.create ~clock ~clear:reset () in
  let open Signal in
  let count_next = wire bits in
  let limit = base - 1 in
  let count = reg spec count_next in
  let cary = increment &: (count ==:. limit) in
  let incr = mux2 increment (count +:. 1) count in
  let next = if base = 1 lsl bits then incr else mux2 cary (zero bits) incr in
  count_next <== next;
  (count, cary)

module type Integer = sig
  val value : int
end

module Counter (Bits : Integer) = struct
  module I = struct
    type 'a t = { clock : 'a; enable : 'a; reset : 'a [@rtlsuffix "_"] } [@@deriving sexp_of, hardcaml]
  end

  let bits = Bits.value

  module O = struct
    type 'a t = { count : 'a; [@bits bits] cary : 'a } [@@deriving sexp_of, hardcaml]
  end

  let create ?base (_scope : Scope.t) (input : Signal.t I.t) =
    let count, cary = counter_with_carry ~bits ~reset:input.reset ~increment:input.enable ~clock:input.clock ?base () in
    { O.count; cary }

  let hierarchical ?base scope input =
    let module H = Hierarchy.In_scope (I) (O) in
    let name = Printf.sprintf "counter_%u" Bits.value in
    let name = match base with Some base -> Printf.sprintf "%s_%u" name base | None -> name in
    H.hierarchical ~scope ~name (create ?base) input
end

let counter_with_carry_test_1 =
  let scope = Scope.create ~flatten_design:true () in
  let module CounterBits = struct
    let value = 3
  end in
  let module Counter = Counter (CounterBits) in
  let module Simulator = Cyclesim.With_interface (Counter.I) (Counter.O) in
  let waves, sim = Counter.create ~base:5 scope |> Simulator.create |> Hardcaml_waveterm.Waveform.create in
  let inputs = Cyclesim.inputs sim in
  let set wire = wire := Bits.vdd in
  let clear wire = wire := Bits.gnd in
  let cycles n =
    for _ = 0 to n do
      Cyclesim.cycle sim
    done
  in
  cycles 2;
  set inputs.enable;
  cycles 5;
  clear inputs.enable;
  cycles 3;
  set inputs.enable;
  cycles 7;
  clear inputs.enable;
  cycles 1;
  set inputs.enable;
  cycles 2;
  clear inputs.enable;
  cycles 4;
  set inputs.enable;
  cycles 7;
  Hardcaml_waveterm.Waveform.print ~display_height:14 ~display_width:100 ~wave_width:0 waves

let counter_with_carry_test_2 =
  let scope = Scope.create ~flatten_design:true () in
  let module CounterBits = struct
    let value = 4
  end in
  let module Counter = Counter (CounterBits) in
  let module Simulator = Cyclesim.With_interface (Counter.I) (Counter.O) in
  let circuit input =
    let count0 = Counter.create scope input in
    let count1 = Counter.create scope { input with enable = count0.cary } in
    let open Signal in
    count0.count -- "count0" |> ignore;
    count0.cary -- "cary0" |> ignore;
    count1
  in
  let waves, sim = circuit |> Simulator.create ~config:Cyclesim.Config.trace_all |> Hardcaml_waveterm.Waveform.create in
  let inputs = Cyclesim.inputs sim in
  let set wire = wire := Bits.vdd in
  let clear wire = wire := Bits.gnd in
  let cycles n =
    for _ = 0 to n do
      Cyclesim.cycle sim
    done
  in
  cycles 2;
  set inputs.enable;
  cycles 5;
  clear inputs.enable;
  cycles 3;
  set inputs.enable;
  cycles 12;
  clear inputs.enable;
  cycles 1;
  set inputs.enable;
  cycles 2;
  set inputs.reset;
  cycles 2;
  clear inputs.reset;
  cycles 4;
  Hardcaml_waveterm.Waveform.print ~display_height:18 ~display_width:100 ~wave_width:0 waves

let trigger_gen ?divider ?target ?(exact = true) ?enable ?clock_freq ~reset ~clock scope =
  let enable = Option.value ~default:Signal.vdd enable in
  let always_enable = Signal.is_const enable && Bits.equal Bits.vdd (Signal.const_value enable) in
  let divider =
    match (always_enable, target, divider, clock_freq) with
    | _, None, Some divider, _ -> divider
    | true, Some target, None, Some clock -> clock / target
    | _ -> assert false
  in
  assert (divider > 0);
  let bits = Base.Int.ceil_log2 divider in
  let divider = if exact then divider else 1 lsl bits in
  let limit = divider - 1 in
  if divider = 1 then clock
  else
    let open Signal in
    let ( -- ) = Scope.naming scope in
    let spec = Reg_spec.create ~clock ~clear:reset () in
    let count = wire bits -- "count" in
    let cary = enable &: (count ==:. limit) in
    let incr = mux2 enable (count +:. 1) count in
    let next = if divider = 1 lsl bits then incr else mux2 cary (zero bits) incr in
    count <== reg spec next;
    cary

module Trigger = struct
  module I = struct
    type 'a t = { clock : 'a; reset : 'a [@rtlsuffix "_"] } [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t = { pulse : 'a } [@@deriving sexp_of, hardcaml]
  end

  let create ?clock_freq ?divider ?target ?exact (scope : Scope.t) (input : Signal.t I.t) =
    { O.pulse = trigger_gen ?divider ?target ?exact ?clock_freq ~reset:input.reset ~clock:input.clock scope }

  let hierarchical ~clock_freq ?divider ?target ?exact scope input =
    let module H = Hierarchy.In_scope (I) (O) in
    let name =
      Printf.sprintf "trigger_%u_%u_%u_%B" clock_freq (Option.value ~default:0 divider) (Option.value ~default:0 target)
        (Option.value ~default:false exact)
    in
    H.hierarchical ~scope ~name (create ~clock_freq ?divider ?target ?exact) input
end

let trigger_test =
  let scope = Scope.create ~flatten_design:true () in
  let module Simulator = Cyclesim.With_interface (Trigger.I) (Trigger.O) in
  let waves, sim =
    Trigger.create ~clock_freq:10 ~target:2 scope
    |> Simulator.create ~config:Cyclesim.Config.trace_all
    |> Hardcaml_waveterm.Waveform.create
  in
  let inputs = Cyclesim.inputs sim in
  let set wire = wire := Bits.vdd in
  let clear wire = wire := Bits.gnd in
  let cycles n =
    for _ = 0 to n do
      Cyclesim.cycle sim
    done
  in
  cycles 7;
  set inputs.reset;
  cycles 2;
  clear inputs.reset;
  cycles 8;
  set inputs.reset;
  cycles 2;
  clear inputs.reset;
  cycles 10;
  Hardcaml_waveterm.Waveform.print ~display_height:11 ~display_width:88 ~wave_width:0 waves

module TriggerWithEnable = struct
  module I = struct
    type 'a t = { clock : 'a; enable : 'a; reset : 'a [@rtlsuffix "_"] } [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t = { pulse : 'a } [@@deriving sexp_of, hardcaml]
  end

  let create ~divider (scope : Scope.t) (input : Signal.t I.t) =
    { O.pulse = trigger_gen ~divider ~enable:input.enable ~reset:input.reset ~clock:input.clock scope }

  let hierarchical ~divider scope input =
    let module H = Hierarchy.In_scope (I) (O) in
    let name = Printf.sprintf "trigger_with_enable_%u" divider in
    H.hierarchical ~scope ~name (create ~divider) input
end

let trigger_with_enable_test =
  let scope = Scope.create ~flatten_design:true () in
  let module Simulator = Cyclesim.With_interface (TriggerWithEnable.I) (TriggerWithEnable.O) in
  let waves, sim =
    TriggerWithEnable.create ~divider:3 scope
    |> Simulator.create ~config:Cyclesim.Config.trace_all
    |> Hardcaml_waveterm.Waveform.create
  in
  let inputs = Cyclesim.inputs sim in
  let set wire = wire := Bits.vdd in
  let clear wire = wire := Bits.gnd in
  let cycles n =
    for _ = 0 to n do
      Cyclesim.cycle sim
    done
  in
  cycles 1;
  set inputs.enable;
  cycles 5;
  set inputs.reset;
  cycles 2;
  clear inputs.reset;
  cycles 5;
  clear inputs.enable;
  cycles 2;
  set inputs.enable;
  cycles 1;
  clear inputs.enable;
  cycles 1;
  set inputs.reset;
  cycles 1;
  clear inputs.reset;
  cycles 1;
  set inputs.enable;
  cycles 15;
  Hardcaml_waveterm.Waveform.print ~display_height:14 ~display_width:88 ~wave_width:0 waves

module Pwm (W : Integer) = struct
  let base = W.value
  let bits = Base.Int.ceil_log2 base

  module I = struct
    type 'a t = { count : 'a; [@bits bits] value : 'a [@bits bits] } [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t = { control : 'a } [@@deriving sexp_of, hardcaml]
  end

  let create _scope (i : Signal.t I.t) =
    assert (base > 0);
    let open Signal in
    let value = uresize i.value bits in
    { O.control = i.count <: value }

  let hierarchical scope input =
    let module H = Hierarchy.In_scope (I) (O) in
    let name = Printf.sprintf "pwm_%u" base in
    H.hierarchical ~scope ~name create input

  module WithCounter = struct
    module Bits = struct
      let value = bits
    end

    module Counter = Counter (Bits)

    module I = struct
      type 'a t = { clock : 'a; enable : 'a; reset : 'a; [@rtlsuffix "_"] value : 'a [@bits bits] }
      [@@deriving sexp_of, hardcaml]
    end

    let create scope (input : Signal.t I.t) =
      let counter =
        Counter.hierarchical ~base scope { clock = input.clock; enable = input.enable; reset = input.reset }
      in
      create scope { count = counter.count; value = input.value }

    let hierarchical scope (input : Signal.t I.t) =
      let name = Printf.sprintf "pwm_counter_%u" base in
      let module H = Hierarchy.In_scope (I) (O) in
      H.hierarchical ~scope ~name create input
  end
end

let pwm_test_1 =
  let module Int4 : Integer = struct
    let value = 4
  end in
  let module Pwm = Pwm (Int4) in
  let scope = Scope.create ~flatten_design:true () in
  let module Simulator = Cyclesim.With_interface (Pwm.WithCounter.I) (Pwm.O) in
  let waves, sim = Pwm.WithCounter.create scope |> Simulator.create |> Hardcaml_waveterm.Waveform.create in
  let cycles n =
    for _ = 0 to n do
      Cyclesim.cycle sim
    done
  in
  let inputs = Cyclesim.inputs sim in
  let set wire = wire := Bits.vdd in
  let clear wire = wire := Bits.gnd in
  let set_value v = inputs.value := Bits.of_int ~width:(Bits.width !(inputs.value)) v in
  set_value 0;
  cycles 1;
  set inputs.enable;
  cycles 1;
  set inputs.reset;
  cycles 1;
  clear inputs.reset;
  set_value 2;
  cycles 2;
  set_value 1;
  cycles 5;
  set_value 3;
  cycles 5;
  set_value 0;
  cycles 1;
  set_value 3;
  cycles 8;
  Hardcaml_waveterm.Waveform.print ~display_height:14 ~display_width:80 ~wave_width:0 waves

let pwm_test_2 =
  let scope = Scope.create ~flatten_design:true () in
  let module Int4 : Integer = struct
    let value = 4
  end in
  let module Pwm = Pwm (Int4) in
  let module Simulator = Cyclesim.With_interface (Pwm.WithCounter.I) (Pwm.O) in
  let waves, sim = Pwm.WithCounter.create scope |> Simulator.create |> Hardcaml_waveterm.Waveform.create in
  let cycles n =
    for _ = 0 to n do
      Cyclesim.cycle sim
    done
  in
  let inputs = Cyclesim.inputs sim in
  let set wire = wire := Bits.vdd in
  let clear wire = wire := Bits.gnd in
  let set_value v = inputs.value := Bits.of_int ~width:(Bits.width !(inputs.value)) v in
  set inputs.enable;
  set_value 0;
  cycles 1;
  set inputs.reset;
  cycles 1;
  set_value 2;
  clear inputs.reset;
  cycles 1;
  set_value 1;
  cycles 5;
  set_value 0;
  cycles 5;
  set_value 3;
  cycles 7;
  Hardcaml_waveterm.Waveform.print ~display_height:14 ~display_width:84 ~wave_width:0 waves