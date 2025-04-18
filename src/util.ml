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

let counter_with_carry ?base ?bits ?reset_value ~reset ~increment ~clock () =
  let reset_value = Option.value ~default:0 reset_value in
  let base, bits =
    match (base, bits) with
    | Some base, _ ->
        let base_bits = Base.Int.ceil_log2 base in
        let bits = Option.value ~default:base_bits bits in
        assert (bits >= base_bits);
        (base, bits)
    | None, Some bits ->
        let base = 1 lsl bits in
        (base, bits)
    | None, None -> assert false
  in
  let open Signal in
  let spec, reset =
    if reset_value = 0 then (Reg_spec.create ~clock ~clear:reset (), fun x -> x)
    else (Reg_spec.create ~clock (), fun x -> mux2 reset (of_int ~width:bits reset_value) x)
  in
  let count_next = wire bits in
  let limit = base - 1 in
  let count = reg spec count_next in
  let cary = increment &: (count ==:. limit) in
  let incr = mux2 increment (count +:. 1) count in
  let next = if base = 1 lsl bits then incr else mux2 cary (zero bits) incr in
  let next = reset next in
  count_next <== next;
  (count, cary)

module type Integer = sig
  val value : int
end

let integer v : (module Integer) =
  (module struct
    let value = v
  end)

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

  module Counter = Counter ((val integer bits))

  module WithCounter = struct
    module I = struct
      type 'a t = { counter : 'a Counter.I.t; value : 'a [@bits bits] } [@@deriving sexp_of, hardcaml]
    end

    let create scope (input : Signal.t I.t) =
      let counter = Counter.hierarchical ~base scope input.counter in
      create scope { count = counter.count; value = input.value }

    let hierarchical scope (input : Signal.t I.t) =
      let name = Printf.sprintf "pwm_counter_%u" base in
      let module H = Hierarchy.In_scope (I) (O) in
      H.hierarchical ~scope ~name create input
  end
end

module SegmentEncoder = struct
  module I = struct
    type 'a t = { digit : 'a [@bits 4] } [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t = { segment : 'a [@bits 7] } [@@deriving sexp_of, hardcaml]
  end

  let create scope i =
    ignore scope;
    let display =
      [
        ('0', "1000000");
        ('1', "1111001");
        ('2', "0100100");
        ('3', "0110000");
        ('4', "0011001");
        ('5', "0010010");
        ('6', "0000010");
        ('7', "1111000");
        ('8', "0000000");
        ('9', "0010000");
        ('A', "0001000");
        ('b', "0000011");
        ('C', "1000110");
        ('d', "0100001");
        ('E', "0000110");
        ('F', "0001110");
      ]
    in
    let open Signal in
    let segment = mux i.I.digit (List.map (fun (_, s) -> of_string ("7'b" ^ s)) display) in
    { O.segment }
end
