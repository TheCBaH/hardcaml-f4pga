open Hardcaml


let segment_encode ~digit =
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
  let segment = mux digit (List.map (fun (_, s) -> of_string ("7'b" ^ s)) display) in
  segment

let segment_encode_test =
  let _digit = "digit" in
  let digit = Signal.input _digit 4 in
  let segment = segment_encode ~digit in
  let _segment = "segment" in
  let circuit = Circuit.create_exn ~name:"segment_encode" [ Signal.output _segment segment ] in
  let waves, sim = Hardcaml_waveterm.Waveform.create (Cyclesim.create circuit) in
  let set wire n =
    Cyclesim.in_port sim wire := Bits.of_int ~width:4 n;
    Cyclesim.cycle sim
  in
  set _digit 0;
  set _digit 1;
  set _digit 8;
  set _digit 0xb;
  set _digit 0xF;
  let display_rules = Hardcaml_waveterm.Display_rule.[ port_name_is _segment ~wave_format:Bit; default ] in
  Hardcaml_waveterm.Waveform.print ~display_rules ~display_height:8 ~display_width:80 ~wave_width:4 waves

type digit = { data : Signal.t; enable : Signal.t; dot : Signal.t }

let display ~clock ~digits ~next ~reset =
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
  let segment = segment_encode ~digit:data in
  let dot = mux digit (List.map (fun d -> ~:(d.dot)) digits) in
  let digit_next = mux2 next (mux2 (digit ==:. digits_count - 1) (width digit |> zero) (digit +:. 1)) digit in
  digit <== reg spec digit_next;
  (anode, segment, dot)

let display_test =
  let _digit_0 = "digit_0" in
  let _digit_1 = "digit_1" in
  let _dot = "dot" in
  let _enable = "enable" in
  let _clock = "clock" in
  let _reset = "reset" in
  let _segment = "segment" in
  let _anode = "anode" in
  let digit_0 = Signal.input _digit_0 4 in
  let digit_1 = Signal.input _digit_1 4 in
  let clock = Signal.input _clock 1 in
  let enable = Signal.input _enable 1 in
  let dot = Signal.input _dot 1 in
  let digits = [ { data = digit_0; enable; dot }; { data = digit_1; enable; dot } ] in
  let reset = Signal.input _reset 1 in
  let anode, segment, dot = display ~clock ~digits ~reset ~next:enable in
  let circuit =
    Circuit.create_exn ~name:"display"
      [ Signal.output _anode anode; Signal.output _segment segment; Signal.output "_dot" dot ]
  in
  let waves, sim = Hardcaml_waveterm.Waveform.create (Cyclesim.create circuit) in
  let set wire = Cyclesim.in_port sim wire := Bits.vdd in
  (* let clear wire = Cyclesim.in_port sim wire := Bits.gnd in *)
  let cycles n =
    for _ = 0 to n do
      Cyclesim.cycle sim
    done
  in
  set _enable;
  Cyclesim.in_port sim _digit_0 := Bits.of_int ~width:4 1;
  Cyclesim.in_port sim _digit_1 := Bits.of_int ~width:4 2;
  cycles 2;
  set _dot;
  Cyclesim.in_port sim _digit_0 := Bits.of_int ~width:4 8;
  Cyclesim.in_port sim _digit_1 := Bits.of_int ~width:4 7;
  cycles 2;
  let display_rules =
    Hardcaml_waveterm.Display_rule.[ port_name_is_one_of [ _segment; _anode ] ~wave_format:Bit; default ]
  in
  Hardcaml_waveterm.Waveform.print ~display_rules ~display_height:20 ~display_width:67 ~wave_width:3 waves

let multi_counter ?base ?bits ~digits ~reset ~increment ~clock () =
  let rec make counters increment left =
    if left == 0 then List.rev counters
    else
      let count, cary = Util.counter_with_carry ?base ?bits ~reset ~increment ~clock () in
      make (count :: counters) cary (left - 1)
  in
  make [] increment digits

let multi_counter_test =
  let _clock = "clock" in
  let _reset = "_reset" in
  let reset = Signal.input _reset 1 in
  let clock = Signal.input _clock 1 in
  let digits = multi_counter ~base:4 ~increment:Signal.vdd ~clock ~reset ~digits:2 () in
  let circuit =
    List.mapi (fun n -> Printf.sprintf "digit_%u" n |> Signal.output) digits |> Circuit.create_exn ~name:"multi_counter"
  in
  let waves, sim = Hardcaml_waveterm.Waveform.create (Cyclesim.create circuit) in
  let cycles n =
    for _ = 0 to n do
      Cyclesim.cycle sim
    done
  in
  cycles 12;
  Cyclesim.in_port sim _reset := Bits.vdd;
  cycles 2;
  Cyclesim.in_port sim _reset := Bits.gnd;
  cycles 12;
  Hardcaml_waveterm.Waveform.print ~display_height:12 ~display_width:80 ~wave_width:0 waves

let clock_top ~clock_freq ~clock ~reset ~refresh ~tick =
  let open Signal in
  let scope = Scope.create () in
  let tick = Util.Trigger.create ~clock_freq ~target:tick scope { reset; clock} in
  let digits = multi_counter ~increment:tick.pulse ~clock ~reset ~digits:4 () in
  let digits =
    List.mapi
      (fun i d ->
        let dot = if i = 2 then vdd else gnd in
        { data = d; enable = vdd; dot })
      digits
  in
  let refresh =
    Util.Trigger.create ~clock_freq ~target:refresh ~exact:false scope { reset; clock }
  in
  let anode, segment, dot = display ~clock ~digits ~reset ~next:refresh.pulse in
  (anode, segment, dot)

let clock_top_test =
  let _clock = "clock" in
  let _reset = "_reset" in
  let _dot = "dot" in
  let _segment = "segment" in
  let _anode = "anode" in
  let clock_freq = 2000 in
  let clock = Signal.input _clock 1 in
  let reset = Signal.input _reset 1 in
  let anode, segment, dot = clock_top ~clock_freq ~clock ~reset ~refresh:1000 ~tick:500 in
  let circuit =
    Circuit.create_exn ~name:"clock_top"
      [ Signal.output _anode anode; Signal.output _segment segment; Signal.output _dot dot ]
  in
  let waves, sim = Hardcaml_waveterm.Waveform.create (Cyclesim.create circuit) in
  let cycles n =
    for _ = 0 to n do
      Cyclesim.cycle sim
    done
  in
  cycles 16;
  let display_rules =
    Hardcaml_waveterm.Display_rule.[ port_name_is_one_of [ _segment; _anode ] ~wave_format:Bit; default ]
  in
  Hardcaml_waveterm.Waveform.print ~display_rules ~display_height:14 ~display_width:80 ~wave_width:1 waves

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
