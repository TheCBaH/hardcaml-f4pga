open Hardcaml

let level_control_float ~max ~levels ~level =
  assert (levels > 1);
  let step_float = float_of_int max /. float_of_int (levels - 1) in
  let value = float_of_int level *. step_float in
  int_of_float value

let level_control_int ~max ~levels ~level ~scale =
  assert (levels > 1);
  let step_float = float_of_int max /. float_of_int (levels - 1) in
  let step_int = int_of_float (step_float *. float_of_int scale) in
  let value = level * step_int / scale in
  value

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

module PwmControl (Levels : Util.Integer) (PwmBase : Util.Integer) = struct
  let bits = Levels.value - 1 |> Bits.num_bits_to_represent

  module Pwm = Util.Pwm (PwmBase)

  module I = struct
    type 'a t = { count : 'a; [@bits Pwm.bits] level : 'a [@bits bits] } [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t = { control : 'a } [@@deriving sexp_of, hardcaml]
  end

  let create ~max scope (input : Signal.t I.t) =
    if max == 0 then { O.control = Signal.gnd }
    else (
      assert (max > 0);
      let scale = PwmBase.value + (max / if max == 0 then 1 else max) in
      let ( -- ) = Scope.naming scope in
      let value = level_control ~max ~levels:Levels.value ~level:input.level ~scale -- "value" in
      assert (Pwm.bits >= Signal.width value);
      let value = Signal.uresize value Pwm.bits in
      let pwm = Pwm.hierarchical scope { Pwm.I.count = input.count; value } in
      { O.control = pwm.control })

  let hierarchical ~max scope input =
    let module H = Hierarchy.In_scope (I) (O) in
    let name = Printf.sprintf "pwm_control_%u_%u_%u" Levels.value PwmBase.value max in
    H.hierarchical ~scope ~name (create ~max) input

  module Counter = Pwm.Counter

  module WithCounter = struct
    module I = struct
      type 'a t = { counter : 'a Counter.I.t; level : 'a [@bits bits] } [@@deriving sexp_of, hardcaml]
    end

    let create ~max scope (input : Signal.t I.t) =
      let counter = Counter.hierarchical ~base:PwmBase.value scope input.counter in
      create ~max scope { count = counter.count; level = input.level }
  end
end

let pwm_control_test =
  let (module Levels) = Util.integer 4 in
  let (module Base) = Util.integer 8 in
  let scope = Scope.create ~flatten_design:true () in
  let module PwmControl = PwmControl (Levels) (Base) in
  let module Simulator = Cyclesim.With_interface (PwmControl.WithCounter.I) (PwmControl.O) in
  let base = 8 in
  let waves, sim =
    PwmControl.WithCounter.create ~max:6 scope
    |> Simulator.create ~config:Cyclesim.Config.trace_all
    |> Hardcaml_waveterm.Waveform.create
  in
  let cycles n =
    for _ = 1 to n do
      Cyclesim.cycle sim
    done
  in
  let inputs = Cyclesim.inputs sim in
  let set wire = wire := Bits.vdd in
  let set_level v =
    inputs.level := Bits.of_int ~width:(Bits.width !(inputs.level)) v;
    2 * base |> cycles
  in
  set inputs.counter.enable;
  set_level 1;
  set_level 2;
  set_level 3;
  Hardcaml_waveterm.Waveform.print ~display_height:18 ~display_width:100 ~wave_width:0 waves

module Color = struct
  type t = { blue : int; green : int; red : int }
end

module Led (Levels : Util.Integer) (PwmBase : Util.Integer) = struct
  module PwmControl = PwmControl (Levels) (PwmBase)

  module Level = struct
    type 'a t = {
      l_blue : 'a; [@bits PwmControl.bits]
      l_green : 'a; [@bits PwmControl.bits]
      l_red : 'a; [@bits PwmControl.bits]
    }
    [@@deriving sexp_of, hardcaml]
  end

  module I = struct
    type 'a t = { count : 'a; [@bits PwmControl.Pwm.bits] level : 'a Level.t } [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t = { blue : 'a; green : 'a; red : 'a } [@@deriving sexp_of, hardcaml]
  end

  let create ~color scope (input : Signal.t I.t) =
    let control color level =
      let pwm_out = PwmControl.hierarchical ~max:color scope { count = input.count; level } in
      pwm_out.PwmControl.O.control
    in
    {
      O.blue = control color.Color.blue input.level.l_blue;
      green = control color.Color.green input.level.l_green;
      red = control color.Color.red input.level.l_red;
    }

  let hierarchical ~color scope input =
    let module H = Hierarchy.In_scope (I) (O) in
    let name = Printf.sprintf "led_%u_%u_%u_%u_%u" Levels.value PwmBase.value color.Color.blue color.green color.red in
    H.hierarchical ~scope ~name (create ~color) input

  module Counter = PwmControl.Counter

  module WithCounter = struct
    module I = struct
      type 'a t = { counter : 'a Counter.I.t; level : 'a Level.t } [@@deriving sexp_of, hardcaml]
    end

    let create ~color scope (input : Signal.t I.t) =
      let counter = Counter.hierarchical ~base:PwmBase.value scope input.counter in
      create ~color scope { count = counter.count; level = input.level }
  end
end

let led_control_test =
  let (module Levels) = Util.integer 4 in
  let (module Base) = Util.integer 8 in
  let scope = Scope.create ~flatten_design:true () in
  let module Led = Led (Levels) (Base) in
  let module Simulator = Cyclesim.With_interface (Led.WithCounter.I) (Led.O) in
  let base = 8 in
  let color = { Color.red = 2; Color.blue = 4; Color.green = 7 } in
  let waves, sim = Led.WithCounter.create ~color scope |> Simulator.create |> Hardcaml_waveterm.Waveform.create in
  let cycles n =
    for _ = 1 to n do
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
  set inputs.counter.enable;
  set_level 1;
  set_level 2;
  set_level 3;
  Hardcaml_waveterm.Waveform.print ~display_height:23 ~display_width:100 ~wave_width:0 waves

module LedStatic = struct
  module I = struct
    type 'a t = { clock : 'a; reset : 'a; [@rtlsuffix "_"] enable : 'a } [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t = { blue : 'a; green : 'a; red : 'a } [@@deriving sexp_of, hardcaml]
  end

  let create ~blue ~green ~red scope (input : Signal.t I.t) =
    let base = blue + green + red in
    let counter, _ =
      Util.counter_with_carry ~base ~clock:input.I.clock ~reset:input.I.reset ~increment:input.I.enable ()
    in
    let open Signal in
    let ( -- ) = Scope.naming scope in
    counter -- "counter" |> ignore;
    let w_red = counter <:. red in
    let w_green = ~:w_red &: counter <:. red + green in
    let w_blue = ~:(w_green |: w_red) &: counter <:. red + green + blue in
    { O.red = w_red; green = w_green; blue = w_blue }
end

module LedTop = struct
  module I = struct
    type 'a t = { clock : 'a; reset : 'a [@rtlsuffix "_"] } [@@deriving sexp_of, hardcaml]
  end

  module Levels = (val Util.integer 16)
  module Led = Led (Levels) ((val Util.integer 256))

  module O = struct
    type 'a t = {
      blue_0 : 'a;
      green_0 : 'a;
      red_0 : 'a;
      blue_1 : 'a;
      green_1 : 'a;
      red_1 : 'a;
      blue_2 : 'a;
      green_2 : 'a;
      red_2 : 'a;
    }
    [@@deriving sexp_of, hardcaml]
  end

  let create ~clock_freq ~refresh ~tick scope input =
    let reset = input.I.reset in
    let _10kHz =
      Util.Trigger.hierarchical ~clock_freq ~target:refresh scope { reset = input.reset; clock = input.clock }
    in
    let _1Hz =
      let divider = refresh / tick in
      Util.TriggerWithEnable.hierarchical ~divider scope
        { reset = input.reset; clock = input.clock; enable = _10kHz.pulse }
    in
    let module Counter = Util.Counter ((val Util.integer Led.PwmControl.bits)) in
    let level =
      Counter.hierarchical ~base:Levels.value scope
        { Counter.I.reset = input.I.reset; enable = _1Hz.pulse; clock = input.I.clock }
    in
    let teal = { Color.red = 0; green = 128; blue = 128 } in
    let tomato = { Color.red = 255; green = 8; blue = 8 (* 71 *) } in
    let purple = { Color.red = 255; green = 0 (* 105 *); blue = 255 } in
    let counter = Led.Counter.hierarchical scope { Led.Counter.I.clock = input.clock; reset; enable = _10kHz.pulse } in
    let const =
      let _ = Signal.width level.count |> Signal.ones in
      let const = if false then Signal.of_int ~width:(Signal.width level.count) 4 else level.count in
      { Led.Level.l_blue = const; l_green = const; l_red = const }
    in
    let level = { Led.Level.l_blue = level.count; l_green = level.count; l_red = level.count } in
    let led_0 = Led.create ~color:teal scope { Led.I.count = counter.count; level } in
    let led_1 = Led.create ~color:tomato scope { Led.I.count = counter.count; level = const } in
    let led_2 = Led.create ~color:purple scope { Led.I.count = counter.count; level = const } in
    {
      O.blue_0 = led_0.Led.O.blue;
      green_0 = led_0.green;
      red_0 = led_0.red;
      O.blue_1 = led_1.Led.O.blue;
      green_1 = led_1.green;
      red_1 = led_1.red;
      O.blue_2 = led_2.Led.O.blue;
      green_2 = led_2.green;
      red_2 = led_2.red;
    }

  let hierarchical ~clock_freq ~refresh ~tick scope input =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"led" (create ~clock_freq ~refresh ~tick) input
end
