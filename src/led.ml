open Hardcaml

let pwm_impl ~clock ~reset ~enable ~base ~value =
  assert (base > 0);
  let bits = Base.Int.ceil_log2 base in
  let open Signal in
  let count = wire bits in
  let count_succ =
    let incr = count +:. 1 in
    if base = 1 lsl bits then incr else mux2 (count <>:. base - 1) incr (zero bits)
  in
  let count_succ = mux2 enable count_succ count in
  let spec = Reg_spec.create ~clock ~clear:reset () in
  count <== reg spec count_succ;
  let value = uresize value bits in
  let control = count <: value &: ~:reset in
  (control, count)

let pwm ~clock ~reset ~enable ~base ~value = pwm_impl ~clock ~reset ~enable ~base ~value |> fst

let pwm_impl_test =
  let _clock = "clock" in
  let _enable = "enable " in
  let _reset = "_reset" in
  let _value = "value" in
  let clock = Signal.input _clock 1 in
  let enable = Signal.input _enable 1 in
  let reset = Signal.input _reset 1 in
  let value = Signal.input _value 2 in
  let control, count = pwm_impl ~reset ~clock ~enable ~base:5 ~value in
  let circuit = Circuit.create_exn ~name:"pwm_impl" [ Signal.output "control" control; Signal.output "count" count ] in
  let waves, sim = Hardcaml_waveterm.Waveform.create (Cyclesim.create circuit) in
  let cycles n =
    for _ = 0 to n do
      Cyclesim.cycle sim
    done
  in
  let set wire = Cyclesim.in_port sim wire := Bits.vdd in
  let clear wire = Cyclesim.in_port sim wire := Bits.gnd in
  let set_value v = Cyclesim.in_port sim _value := Bits.of_int ~width:(Signal.width value) v in
  set_value 2;
  cycles 1;
  set _enable;
  cycles 5;
  set _reset;
  cycles 2;
  clear _reset;
  cycles 2;
  set_value 1;
  cycles 5;
  set_value 3;
  cycles 5;
  set_value 0;
  cycles 1;
  set_value 3;
  cycles 2;
  Hardcaml_waveterm.Waveform.print ~display_height:16 ~display_width:84 ~wave_width:0 waves

let pwm_test =
  let _clock = "clock" in
  let _enable = "enable " in
  let _reset = "_reset" in
  let _value = "value" in
  let clock = Signal.input _clock 1 in
  let enable = Signal.input _enable 1 in
  let reset = Signal.input _reset 1 in
  let value = Signal.input _value 2 in
  let control = pwm ~reset ~clock ~enable ~base:4 ~value in
  let circuit = Circuit.create_exn ~name:"pwm" [ Signal.output "control" control ] in
  let waves, sim = Hardcaml_waveterm.Waveform.create (Cyclesim.create circuit) in
  let cycles n =
    for _ = 0 to n do
      Cyclesim.cycle sim
    done
  in
  let set wire = Cyclesim.in_port sim wire := Bits.vdd in
  let clear wire = Cyclesim.in_port sim wire := Bits.gnd in
  let set_value v = Cyclesim.in_port sim _value := Bits.of_int ~width:(Signal.width value) v in
  set _enable;
  set_value 2;
  cycles 4;
  set _reset;
  cycles 1;
  clear _reset;
  cycles 1;
  set_value 1;
  cycles 5;
  set_value 0;
  cycles 5;
  set_value 3;
  cycles 7;
  Hardcaml_waveterm.Waveform.print ~display_height:14 ~display_width:84 ~wave_width:0 waves

type led_in = { value : int; wire : Signal.t }

let level_control_float ~max ~levels ~level =
  assert (levels > 1);
  let step_float = float_of_int max /. float_of_int (levels - 1) in
  let value = float_of_int level *. step_float in
  int_of_float value

let level_control_test control =
  let test max levels level =
    control ~max ~levels ~level |> Printf.printf "max:%u levels:%u level:%u value:%u\n%!" max levels level
  in
  let levels = [ 15; 14; 11; 8; 4; 1; 0 ] in
  List.iter (test 255 16) levels;
  List.iter (test 25 16) levels;
  List.iter (test 7 16) levels

let level_control_float_test = level_control_test level_control_float

let level_control_int ~max ~levels ~level ~scale =
  assert (levels > 1);
  let step_float = float_of_int max /. float_of_int (levels - 1) in
  let step_int = int_of_float (step_float *. float_of_int scale) in
  let value = level * step_int / scale in
  value

let level_control_int_test = level_control_test (level_control_int ~scale:1)
let level_control_int_test_scaled_4 = level_control_test (level_control_int ~scale:4)
let level_control_int_test_scaled_16 = level_control_test (level_control_int ~scale:16)
let bits_of_constant c = Bits.num_bits_to_represent c

let bits_of_constant_test =
  let test c = bits_of_constant c |> Printf.printf "constant:%u bits:%u\n%!" c in
  test 0;
  test 1;
  test 2;
  test 3;
  test 4;
  test 7;
  test 8

let level_control_bit ~max ~levels ~level ~scale =
  let scale_bits = Base.Int.ceil_log2 scale in
  let scale = 1 lsl scale_bits in
  assert (levels > 1);
  let step_float = float_of_int max /. float_of_int (levels - 1) in
  let step_int = int_of_float (step_float *. float_of_int scale) in
  let step_bits = bits_of_constant step_int |> Stdlib.max scale_bits in
  let open Bits in
  let step = of_int ~width:step_bits step_int in
  (* let level_bits = width level in *)
  let value_scaled = level *: step in
  let value = drop_bottom value_scaled scale_bits in
  bits_of_constant max |> uresize value

let level_control_bit_test =
  let test scale max levels level =
    let level_bits = bits_of_constant level in
    let level = Bits.of_int ~width:level_bits level in
    let value = level_control_bit ~max ~levels ~level ~scale in
    Printf.printf "scale:%u max:%u levels:%u level:%u value:%u:%u\n%!" scale max levels (Bits.to_int level)
      (Bits.to_int value) (Bits.width value)
  in
  List.iter
    (fun scale ->
      let levels = [ 15; 14; 11; 8; 4; 1; 0 ] in
      List.iter (test scale 255 16) levels;
      List.iter (test scale 25 16) levels;
      List.iter (test scale 7 16) levels)
    [ 1; 4; 16 ]

let level_control ~max ~levels ~level ~scale =
  let scale_bits = Base.Int.ceil_log2 scale in
  let scale = 1 lsl scale_bits in
  assert (levels > 1);
  let step_float = float_of_int max /. float_of_int (levels - 1) in
  let step_int = int_of_float (step_float *. float_of_int scale) in
  let step_bits = bits_of_constant step_int |> Stdlib.max scale_bits in
  let open Signal in
  let step = of_int ~width:step_bits step_int in
  let value_scaled = level *: step in
  let value = drop_bottom value_scaled scale_bits in
  bits_of_constant max |> uresize value

let level_control_test =
  let _level = "level" in
  let _value = "value" in
  let levels = 16 in
  let test scale max =
    let level = Base.Int.ceil_log2 levels |> Signal.input _level in
    let value = level_control ~levels ~level ~max ~scale in
    let circuit = Circuit.create_exn ~name:"level_control" [ Signal.output _value value ] in
    let waves, sim = Hardcaml_waveterm.Waveform.create (Cyclesim.create circuit) in
    List.iter
      (fun v ->
        Cyclesim.in_port sim _level := Bits.of_int ~width:(Signal.width level) v;
        Cyclesim.cycle sim)
      [ 15; 14; 11; 9; 8; 7; 4; 1; 0 ];
    let display_rules =
      Hardcaml_waveterm.Display_rule.[ port_name_is_one_of [ _level; _value ] ~wave_format:Unsigned_int; default ]
    in
    Hardcaml_waveterm.Waveform.print ~display_rules ~display_height:8 ~display_width:80 ~wave_width:2 waves
  in
  List.iter
    (fun scale ->
      List.iter
        (fun max ->
          Printf.printf "scale:%u max:%u\n" scale max;
          test max scale)
        [ 4; 16 ])
    [ 255; 25; 7 ]

let pwm_control ~clock ~reset ~enable ~base ~max ~levels ~level =
  let scale = base / max in
  let value = level_control ~max ~levels ~level ~scale in
  let control = pwm ~clock ~reset ~enable ~base ~value in
  control

let pwm_control_test =
  let _clock = "clock" in
  let _enable = "enable " in
  let _reset = "_reset" in
  let _level = "level" in
  let clock = Signal.input _clock 1 in
  let enable = Signal.input _enable 1 in
  let reset = Signal.input _reset 1 in
  let levels = 4 in
  let base = 8 in
  let level = Bits.num_bits_to_represent levels |> Signal.input _level in
  let control = pwm_control ~reset ~clock ~enable ~base ~max:6 ~levels ~level in
  let circuit = Circuit.create_exn ~name:"pwm_control" [ Signal.output "control" control ] in
  let waves, sim = Hardcaml_waveterm.Waveform.create (Cyclesim.create circuit) in
  let cycles n =
    for _ = 0 to n do
      Cyclesim.cycle sim
    done
  in
  let set wire = Cyclesim.in_port sim wire := Bits.vdd in
  let set_level v =
    Cyclesim.in_port sim _level := Bits.of_int ~width:(Signal.width level) v;
    cycles base
  in
  set _enable;
  set_level 1;
  set_level 2;
  set_level 3;
  Hardcaml_waveterm.Waveform.print ~display_height:14 ~display_width:84 ~wave_width:0 waves

module Color = struct
  type t = { red : int; green : int; blue : int }
end

module Control = struct
  type t = { red : Signal.t; blue : Signal.t; green : Signal.t }
end
