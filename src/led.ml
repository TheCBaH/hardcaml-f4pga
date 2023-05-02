open Hardcaml

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

let pwm_control ~scope ~clock ~reset ~enable ~base ~max ~levels ~level =
  let scale = base / max in
  let value = level_control ~max ~levels ~level ~scale in
  let control = Util.pwm ~scope ~clock ~reset ~enable ~base ~value () in
  control

module PwmControl (Levels : Util.Integer) = struct
  let bits = Levels.value - 1 |> Bits.num_bits_to_represent

  module I = struct
    type 'a t = { clock : 'a; enable : 'a; reset : 'a; [@rtlsuffix "_"] level : 'a [@bits bits] }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t = { control : 'a } [@@deriving sexp_of, hardcaml]
  end

  let create ~base ~max scope (input : Signal.t I.t) =
    {
      O.control =
        pwm_control ~scope ~clock:input.clock ~reset:input.reset ~enable:input.enable ~base ~max ~levels:Levels.value
          ~level:input.level;
    }

  let hierarchical ~base ~max scope input =
    let module H = Hierarchy.In_scope (I) (O) in
    let name = Printf.sprintf "pwm_control_%u_%u_%u" Levels.value base max in
    H.hierarchical ~scope ~name (create ~base ~max) input
end

let pwm_control_test =
  let module Levels = struct
    let value = 4
  end in
  let scope = Scope.create ~flatten_design:true () in
  let module PwmControl = PwmControl (Levels) in
  let module Simulator = Cyclesim.With_interface (PwmControl.I) (PwmControl.O) in
  let base = 8 in
  let waves, sim = PwmControl.create ~max:6 ~base scope |> Simulator.create |> Hardcaml_waveterm.Waveform.create in
  let cycles n =
    for _ = 0 to n do
      Cyclesim.cycle sim
    done
  in
  let inputs = Cyclesim.inputs sim in
  let set wire = wire := Bits.vdd in
  let set_level v =
    inputs.level := Bits.of_int ~width:(Bits.width !(inputs.level)) v;
    2 * base |> cycles
  in
  set inputs.enable;
  set_level 1;
  set_level 2;
  set_level 3;
  Hardcaml_waveterm.Waveform.print ~display_height:14 ~display_width:100 ~wave_width:0 waves

module Color = struct
  type t = { blue : int; green : int; red : int }
end

module Led (Levels : Util.Integer) = struct
  let bits = Levels.value - 1 |> Bits.num_bits_to_represent

  module Level = struct
    type 'a t = { l_blue : 'a; [@bits bits] l_green : 'a; [@bits bits] l_red : 'a [@bits bits] }
    [@@deriving sexp_of, hardcaml]
  end

  module I = struct
    type 'a t = { clock : 'a; enable : 'a; reset : 'a; [@rtlsuffix "_"] level : 'a Level.t }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t = { blue : 'a; green : 'a; red : 'a } [@@deriving sexp_of, hardcaml]
  end

  module PwmControl = PwmControl (Levels)

  let create ~base ~color scope (input : Signal.t I.t) =
    let control color level =
      let pwm_in = { PwmControl.I.reset = input.reset; level; clock = input.clock; enable = input.enable } in
      let pwm_out = PwmControl.hierarchical ~base ~max:color scope pwm_in in
      pwm_out.PwmControl.O.control
    in
    {
      O.blue = control color.Color.blue input.level.l_blue;
      green = control color.Color.green input.level.l_green;
      red = control color.Color.red input.level.l_red;
    }

  let hierarchical ~base ~color scope input =
    let module H = Hierarchy.In_scope (I) (O) in
    let name = Printf.sprintf "led_%u_%u_%u_%u" base color.Color.blue color.green color.red in
    H.hierarchical ~scope ~name (create ~base ~color) input
end

let led_control_test =
  let module Levels = struct
    let value = 4
  end in
  let scope = Scope.create ~flatten_design:true () in
  let module Led = Led (Levels) in
  let module Simulator = Cyclesim.With_interface (Led.I) (Led.O) in
  let base = 8 in
  let color = { Color.red = 2; Color.blue = 4; Color.green = 7 } in
  let waves, sim = Led.create ~color ~base scope |> Simulator.create |> Hardcaml_waveterm.Waveform.create in
  let cycles n =
    for _ = 0 to n do
      Cyclesim.cycle sim
    done
  in
  let inputs = Cyclesim.inputs sim in
  let set wire = wire := Bits.vdd in
  let set_level v =
    List.iter
      (fun i -> i := Bits.of_int ~width:(Bits.width !i) v)
      [ inputs.level.l_blue; inputs.level.l_green; inputs.level.l_red ];
    2 * base |> cycles
  in
  set inputs.enable;
  set_level 1;
  set_level 4;
  set_level 7;
  Hardcaml_waveterm.Waveform.print ~display_height:23 ~display_width:100 ~wave_width:0 waves

module LedTop = struct
  module I = struct
    type 'a t = { clock : 'a; reset : 'a [@rtlsuffix "_"] } [@@deriving sexp_of, hardcaml]
  end

  module Levels = struct
    let value = 16
  end

  module Led = Led (Levels)
  module O = Led.O

  let create ~clock_freq ~refresh ~tick scope input =
    let base = 256 in
    let reset = input.I.reset in
    let _10kHz =
      Clock.Trigger.hierarchical ~clock_freq ~target:refresh scope
        { Clock.Trigger.I.reset = input.I.reset; clock = input.I.clock }
    in
    let _1Hz =
      let divider = refresh / tick in
      Clock.TriggerWithEnable.hierarchical ~clock_freq ~divider scope
        { Clock.TriggerWithEnable.I.reset = input.I.reset; clock = input.I.clock; enable = _10kHz.pulse }
    in
    let orchid = { Color.red = 218; green = 112; blue = 214 } in
    let level, _ = Clock.counter_with_carry ~base:Levels.value ~reset ~increment:_1Hz.pulse ~clock:input.I.clock () in
    Led.create ~base ~color:orchid scope
      {
        Led.I.clock = input.I.clock;
        enable = _10kHz.pulse;
        reset;
        level = { Led.Level.l_blue = level; l_green = level; l_red = level };
      }

  let hierarchical ~clock_freq ~refresh ~tick scope input =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"led" (create ~clock_freq ~refresh ~tick) input
end
