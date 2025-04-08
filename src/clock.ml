open Hardcaml

type digit = { data : Signal.t; enable : Signal.t; dot : Signal.t }

let display scope ~clock ~digits ~next ~reset =
  let digits_count = List.length digits in
  assert (digits_count > 0);
  let spec = Reg_spec.create ~clock ~clear:reset () in
  let open Signal in
  let digit = Base.Int.ceil_log2 digits_count |> wire in
  let anode =
    List.mapi
      (fun n d ->
        let enable = ~:(~:reset &: d.enable &: (digit ==:. n)) in
        wireof enable)
      digits
    |> concat_lsb
  in
  let data = mux digit (List.map (fun d -> d.data) digits) in
  let segment = Util.SegmentEncoder.create scope { digit = data } in
  let dot = mux digit (List.map (fun d -> ~:(d.dot)) digits) in
  let digit_next = mux2 next (mux2 (digit ==:. digits_count - 1) (width digit |> zero) (digit +:. 1)) digit in
  digit <== reg spec digit_next;
  (anode, segment.segment, dot)

let multi_counter ?base ?bits ~digits ~reset ~increment ~clock () =
  let rec make counters increment left =
    if left == 0 then List.rev counters
    else
      let count, cary = Util.counter_with_carry ?base ?bits ~reset ~increment ~clock () in
      make (count :: counters) cary (left - 1)
  in
  make [] increment digits

let clock_top ~clock_freq ~clock ~reset ~refresh ~tick =
  let open Signal in
  let scope = Scope.create () in
  let tick = Util.Trigger.create ~clock_freq ~target:tick scope { reset; clock } in
  let digits = multi_counter ~base:10 ~increment:tick.pulse ~clock ~reset ~digits:4 () in
  let digits =
    List.mapi
      (fun i d ->
        let dot = if i = 2 then vdd else gnd in
        { data = d; enable = vdd; dot })
      digits
  in
  let refresh = Util.Trigger.create ~clock_freq ~target:refresh ~exact:false scope { reset; clock } in
  let anode, segment, dot = display scope ~clock ~digits ~reset ~next:refresh.pulse in
  (anode, segment, dot)

module Reset = struct
  module I = struct
    type 'a t = { clock : 'a; [@bits 1] activate : 'a [@bits 1] } [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t = { reset : 'a [@bits 1] } [@@deriving sexp_of, hardcaml]
  end

  let create (scope : Scope.t) (input : Signal.t I.t) =
    let pulse = Util.Pulse.hierarchical scope ~length:32 { Util.Pulse.I.clock = input.clock; reset = input.activate } in
    { O.reset = pulse.Util.Pulse.O.pulse }

  let hierarchical scope input =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"reset" create input
end

module Clock = struct
  module I = struct
    type 'a t = { clock : 'a; [@bits 1] reset : 'a [@bits 1] } [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t = { anode : 'a; [@bits 4] segment : 'a; [@bits 8] dot : 'a } [@@deriving sexp_of, hardcaml]
  end

  let create (_scope : Scope.t) (input : Signal.t I.t) =
    let clock_freq = 100_000_000 in
    let anode, segment, dot = clock_top ~clock_freq ~clock:input.clock ~reset:input.reset ~refresh:1000 ~tick:100 in
    { O.anode; segment; dot }
end

module ClockCircuit = Circuit.With_interface (Clock.I) (Clock.O)
module ResetCircuit = Circuit.With_interface (Reset.I) (Reset.O)
