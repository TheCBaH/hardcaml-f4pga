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
  Hardcaml_waveterm.Waveform.print ~display_height:10 ~display_width:80 ~wave_width:0 waves

let counter_with_carry ?(base = 10) ?bits ~reset ~increment ~clock () =
  let base_bits = Base.Int.ceil_log2 base in
  let bits = Option.value ~default:base_bits bits in
  assert (bits >= base_bits);
  let spec = Reg_spec.create ~clock () in
  let open Signal in
  let count_next = wire bits in
  let limit = base - 1 in
  let count = reg spec count_next in
  let cary = increment &: (count ==:. limit) in
  count_next <== mux2 (cary |: reset) (zero (Signal.width count_next)) (mux2 increment (count +:. 1) count);
  (count, cary)

let counter_with_carry_test_1 =
  let _clock = "clock" in
  let _increment = "increment" in
  let _reset = "_reset" in
  let clock = Signal.input _clock 1 in
  let increment = Signal.input _increment 1 in
  let reset = Signal.input _reset 1 in
  let count, carry = counter_with_carry ~increment ~reset ~base:5 ~bits:3 ~clock () in
  let circuit =
    Circuit.create_exn ~name:"counter_with_carry" [ Signal.output "carry" carry; Signal.output "count" count ]
  in
  let waves, sim = Hardcaml_waveterm.Waveform.create (Cyclesim.create circuit) in
  let cycles n =
    for _ = 0 to n do
      Cyclesim.cycle sim
    done
  in
  cycles 2;
  Cyclesim.in_port sim _increment := Bits.vdd;
  cycles 5;
  Cyclesim.in_port sim _increment := Bits.gnd;
  cycles 3;
  Cyclesim.in_port sim _increment := Bits.vdd;
  cycles 7;
  Cyclesim.in_port sim _increment := Bits.gnd;
  cycles 1;
  Cyclesim.in_port sim _increment := Bits.vdd;
  cycles 2;
  Cyclesim.in_port sim _reset := Bits.vdd;
  cycles 4;
  Cyclesim.in_port sim _reset := Bits.gnd;
  cycles 7;
  Hardcaml_waveterm.Waveform.print ~display_height:14 ~display_width:100 ~wave_width:0 waves;
  let output_mode = Rtl.Output_mode.To_file "counter.v" in
  Rtl.output ~output_mode Verilog circuit

let counter_with_carry_test_2 =
  let _clock = "clock" in
  let _increment = "increment" in
  let _reset = "_reset" in
  let clock = Signal.input _clock 1 in
  let increment = Signal.input _increment 1 in
  let reset = Signal.input _reset 1 in
  let count0, carry0 = counter_with_carry ~increment ~reset ~clock () in
  let count1, _ = counter_with_carry ~increment:carry0 ~reset ~clock () in
  let circuit =
    Circuit.create_exn ~name:"counter_with_carry"
      [ Signal.output "carry0" carry0; Signal.output "count0" count0; Signal.output "count1" count1 ]
  in
  let waves, sim = Hardcaml_waveterm.Waveform.create (Cyclesim.create circuit) in
  let set wire = Cyclesim.in_port sim wire := Bits.vdd in
  let clear wire = Cyclesim.in_port sim wire := Bits.gnd in
  let cycles n =
    for _ = 0 to n do
      Cyclesim.cycle sim
    done
  in
  cycles 2;
  set _increment;
  cycles 5;
  clear _increment;
  cycles 3;
  set _increment;
  cycles 12;
  clear _increment;
  cycles 1;
  set _increment;
  cycles 2;
  set _reset;
  cycles 2;
  clear _reset;
  cycles 4;
  Hardcaml_waveterm.Waveform.print ~display_height:16 ~display_width:100 ~wave_width:0 waves

type clock = { clock : int; wire : Signal.t }

let clock_gen ~target ~reset ~clock =
  let divider = clock.clock / target in
  let limit = divider - 1 in
  let bits = Base.Int.ceil_log2 divider in
  let open Signal in
  let spec = Reg_spec.create ~clock:clock.wire ~clear:reset () in
  let count = wire bits in
  let pulse = count ==:. limit in
  let next = mux2 pulse (zero bits) (count +:. 1) in
  count <== reg spec next;
  pulse

let clock_gen_test =
  let _clock = "clock" in
  let _reset = "_reset" in
  let clock = { clock = 10; wire = Signal.input _clock 1 } in
  let reset = Signal.input _reset 1 in
  let pulse = clock_gen ~clock ~reset ~target:2 in
  let circuit = Circuit.create_exn ~name:"clock_gen" [ Signal.output "pulse" pulse ] in
  let waves, sim = Hardcaml_waveterm.Waveform.create (Cyclesim.create circuit) in
  let set wire = Cyclesim.in_port sim wire := Bits.vdd in
  let clear wire = Cyclesim.in_port sim wire := Bits.gnd in
  let cycles n =
    for _ = 0 to n do
      Cyclesim.cycle sim
    done
  in
  cycles 10;
  set _reset;
  cycles 2;
  clear _reset;
  cycles 15;
  Hardcaml_waveterm.Waveform.print ~display_height:8 ~display_width:80 ~wave_width:0 waves

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
  let dot = mux digit (List.map (fun d -> d.dot) digits) in
  let display = ~:dot @: segment in
  let digit_next = mux2 next (mux2 (digit ==:. digits_count - 1) (width digit |> zero) (digit +:. 1)) digit in
  digit <== reg spec digit_next;
  (anode, display)

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
  let anode, segment = display ~clock ~digits ~reset ~next:enable in
  let circuit = Circuit.create_exn ~name:"display" [ Signal.output _anode anode; Signal.output _segment segment ] in
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
  Cyclesim.in_port sim _digit_0 := Bits.of_int ~width:4 8;
  Cyclesim.in_port sim _digit_1 := Bits.of_int ~width:4 7;
  cycles 2;
  let display_rules =
    Hardcaml_waveterm.Display_rule.[ port_name_is_one_of [ _segment; _anode ] ~wave_format:Bit; default ]
  in
  Hardcaml_waveterm.Waveform.print ~display_rules ~display_height:10 ~display_width:80 ~wave_width:4 waves

let multi_counter ?base ?bits ~digits ~reset ~increment ~clock () =
  let rec make counters increment left =
    if left == 0 then List.rev counters
    else
      let count, cary = counter_with_carry ?base ?bits ~reset ~increment ~clock () in
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

let clock_top ~clock ~reset ~refresh ~tick =
  let open Signal in
  let tick = clock_gen ~clock ~reset ~target:tick in
  let digits = multi_counter ~increment:tick ~clock:clock.wire ~reset ~digits:4 () in
  let digits =
    List.mapi
      (fun i d ->
        let dot = if i = 2 then vdd else gnd in
        { data = d; enable = vdd; dot })
      digits
  in
  let refresh = clock_gen ~clock ~reset ~target:refresh in
  let anode, segment = display ~clock:clock.wire ~digits ~reset ~next:refresh in
  (anode, segment)

let clock_top_test =
  let _clock = "clock" in
  let _reset = "_reset" in
  let _segment = "segment" in
  let _anode = "anode" in
  let clock = { clock = 2000; wire = Signal.input _clock 1 } in
  let reset = Signal.input _reset 1 in
  let anode, segment = clock_top ~clock ~reset ~refresh:1000 ~tick:500 in
  let circuit = Circuit.create_exn ~name:"clock_top" [ Signal.output _anode anode; Signal.output _segment segment ] in
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
  Hardcaml_waveterm.Waveform.print ~display_rules ~display_height:12 ~display_width:80 ~wave_width:2 waves

module Reset = struct
  module I = struct
    type 'a t = { clock : 'a; [@bits 1] activate : 'a [@bits 1] } [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t = { reset : 'a [@bits 1] } [@@deriving sexp_of, hardcaml]
  end

  let create (_scope : Scope.t) (input : Signal.t I.t) =
    { O.reset = pulse ~reset:input.activate ~clock:input.clock ~length:2 }

  let hierarchical scope input =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"reset" create input
end

module Clock = struct
  module I = struct
    type 'a t = { clock : 'a; [@bits 1] reset : 'a [@bits 1] } [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t = { anode: 'a; [@bits 4] segment: 'a; [@bits 8]  } [@@deriving sexp_of, hardcaml]
  end
  let create (_scope : Scope.t) (input : Signal.t I.t) =
    let clock = { clock = 100_000_000; wire = input.clock } in
    let anode, segment = clock_top ~clock ~reset:input.reset ~refresh:1000 ~tick:100 in
    { O.anode ; segment}
end

module ClockCircuit = Circuit.With_interface (Clock.I) (Clock.O)
module ResetCircuit = Circuit.With_interface (Reset.I) (Reset.O)

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
