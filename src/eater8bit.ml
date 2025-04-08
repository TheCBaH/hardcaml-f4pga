open Hardcaml

module Register = struct
  let bits = 8

  module I = struct
    type 'a t = { clock : 'a; reset : 'a; w_en : 'a; w_data : 'a [@bits bits] } [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t = { data : 'a [@bits bits] } [@@deriving sexp_of, hardcaml]
  end

  let create scope i =
    let ( -- ) = Scope.naming scope in
    let spec = Reg_spec.create ~clock:i.I.clock ~clear:i.I.reset () in
    let register = Signal.reg_fb ~enable:i.I.w_en ~width:bits ~f:(fun _ -> i.I.w_data) spec -- "register" in
    { O.data = register }
end

module Alu = struct
  let bits = 8

  module Code = struct
    type t = Add | Sub [@@deriving sexp_of, compare, enumerate]
  end

  include Enum.Make_enums (Code)

  module I = struct
    type 'a t = { op : 'a Binary.t; [@rtlmangle true] a : 'a; [@bits bits] b : 'a [@bits bits] }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t = { data : 'a; [@bits bits] carry : 'a; zero : 'a } [@@deriving sexp_of, hardcaml]
  end

  let create scope i =
    ignore scope;
    let open Signal in
    let a = succ bits |> Signal.uresize i.I.a in
    let b = succ bits |> Signal.uresize i.I.b in
    let add = a +: b in
    let sub = a -: b in
    let result = Binary.Of_signal.match_ i.I.op [ (Add, add); (Sub, sub) ] in
    let carry = msb result in
    let data = lsbs result in
    let zero = data ==:. 0 in
    { O.data; carry; zero }
end

module Ram = struct
  let bits = 8
  let size = 16
  let bits_addr = size - 1 |> Bits.num_bits_to_represent

  module I = struct
    type 'a t = { clock : 'a; w_en : 'a; w_data : 'a; [@bits bits] addr : 'a [@bits bits_addr] }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t = { data : 'a [@bits bits] } [@@deriving sexp_of, hardcaml]
  end

  let memory i =
    let write_port =
      { Signal.write_clock = i.I.clock; write_address = i.addr; write_enable = i.w_en; write_data = i.w_data }
    in
    Signal.memory ~write_port ~read_address:i.I.addr size

  let create scope i =
    ignore scope;
    let memory =
      let write_port =
        { Signal.write_clock = i.I.clock; write_address = i.addr; write_enable = i.w_en; write_data = i.w_data }
      in
      Signal.memory ~write_port ~read_address:i.I.addr size
    in
    { O.data = memory }
end

module InitializedMemory = struct
  module I = struct
    type 'a t = { bus : 'a Ram.I.t; reset : 'a [@rtlsuffix "_"] } [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t = { bus : 'a Ram.O.t; ready : 'a } [@@deriving sexp_of, hardcaml]
  end

  let create ~rom scope i =
    let rom_len = List.length rom in
    assert (rom_len > 0);
    assert (rom_len <= Ram.size);
    let open Signal in
    let ( -- ) = Scope.naming scope in
    let spec = Reg_spec.create ~clock:i.I.bus.clock ~clear:i.I.reset () in
    let addr_next = Bits.num_bits_to_represent rom_len |> wire in
    let rom_done_next = wire 1 -- "rom_done" in
    let addr = reg spec addr_next -- "rom_addr" in
    let rom_done = reg spec rom_done_next -- "rom_done" in
    let ready = addr ==:. rom_len - 1 in
    addr_next <== mux2 ready addr (addr +:. 1);
    rom_done_next <== ready;
    let rom = List.map (fun v -> Bits.to_constant v |> of_constant) rom in
    let data = mux addr rom in
    let sel rom ram = mux2 rom_done ram rom in
    let write =
      {
        Ram.I.clock = i.I.bus.clock;
        w_en = sel vdd i.I.bus.w_en;
        addr = sel (uresize addr Ram.bits_addr) i.I.bus.addr;
        w_data = sel data i.I.bus.w_data;
      }
    in
    let memory = Ram.create scope write in
    { O.bus = memory; ready = rom_done }
end

module MemoryWithRom = struct
  module I = Ram.I
  module O = Ram.O

  let create ~rom scope i =
    let rom_len = List.length rom in
    assert (rom_len <= Ram.size);
    let rom_bits = Bits.address_bits_for rom_len in
    let open Signal in
    let memory = Ram.create scope i in
    let rom = List.map (fun v -> Bits.to_constant v |> of_constant) rom in
    let data =
      if rom_len = 1 lsl rom_bits then mux i.I.addr rom
      else if rom_len = 0 then memory.O.data
      else mux i.I.addr (rom @ [ memory.O.data ])
    in
    { O.data }
end

module Pc = struct
  let bits = Ram.bits_addr

  module I = struct
    type 'a t = { clock : 'a; enable : 'a; reset : 'a; w_en : 'a; w_data : 'a [@bits bits] }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t = { data : 'a [@bits bits] } [@@deriving sexp_of, hardcaml]
  end

  let create scope i =
    ignore scope;
    let spec = Reg_spec.create ~clock:i.I.clock ~clear:i.I.reset () in
    let open Signal in
    let register =
      reg_fb ~width:bits
        ~f:(fun prev ->
          let next = prev +:. 1 in
          mux2 i.I.enable next prev |> mux2 i.I.w_en i.I.w_data)
        spec
    in
    { O.data = register }
end

(*
module Output = struct
  module I = Register.I

  module O = struct
    type 'a t = {
      high : 'a Util.SegmentEncoder.O.t; [@rtlmangle true]
      low : 'a Util.SegmentEncoder.O.t; [@rtlmangle true]
    }
    [@@deriving sexp_of, hardcaml]
  end

  let create scope i =
    let reg = Register.create scope i in
    let high, low = Signal.split_in_half_msb reg.Register.O.data in
    {
      O.high = Util.SegmentEncoder.create scope { Util.SegmentEncoder.I.digit = high };
      low = Util.SegmentEncoder.create scope { Util.SegmentEncoder.I.digit = low };
    }
end

let output_test =
  let scope = Scope.create ~flatten_design:true () in
  let module Simulator = Cyclesim.With_interface (Output.I) (Output.O) in
  let waves, sim = Output.create scope |> Simulator.create |> Hardcaml_waveterm.Waveform.create in
  let inputs = Cyclesim.inputs sim in
  let cycles n =
    for _ = 1 to n do
      Cyclesim.cycle sim
    done
  in
  let set wire = wire := Bits.vdd in
  let clear wire = wire := Bits.gnd in
  let data v =
    inputs.w_data := Bits.of_int ~width:Register.bits v;
    cycles 1
  in
  cycles 1;
  set inputs.w_en;
  data 0x11;
  data 0x22;
  clear inputs.w_en;
  data 0x33;
  set inputs.w_en;
  cycles 1;
  data 0x81;
  cycles 1;
  let display_rules =
    Hardcaml_waveterm.Display_rule.
      [ Re.Posix.re "(segment)" |> Re.Posix.compile |> port_name_matches ~wave_format:Bit; default ]
  in
  Hardcaml_waveterm.Waveform.print ~display_rules ~display_height:18 ~display_width:80 ~wave_width:3 waves
*)

module Output = Register

module Instruction = struct
  module I = Register.I

  module O = struct
    type 'a t = { code : 'a; [@bits 4] addr : 'a [@bits Ram.bits_addr] } [@@deriving sexp_of, hardcaml]
  end

  let create scope i =
    let reg = Register.create scope i in
    let code, addr = Signal.split_in_half_msb reg.Register.O.data in
    { O.code; addr }
end

module MemoryAddr = struct
  module I = Register.I

  module O = struct
    type 'a t = { data : 'a [@bits Ram.bits_addr] } [@@deriving sexp_of, hardcaml]
  end

  let create scope i =
    let reg = Register.create scope i in
    { O.data = Signal.uresize reg.data Ram.bits_addr }
end

module Flags = struct
  module I = struct
    type 'a t = { clock : 'a; reset : 'a; w_en : 'a; w_carry : 'a; w_zero : 'a } [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t = { carry : 'a; zero : 'a } [@@deriving sexp_of, hardcaml]
  end

  let create scope i =
    ignore scope;
    let spec = Reg_spec.create ~clock:i.I.clock ~clear:i.I.reset () in
    let open Signal in
    let flag v = reg_fb ~width:1 ~enable:i.I.w_en ~f:(fun _ -> v) spec in
    { O.carry = flag i.I.w_carry; zero = flag i.I.w_zero }
end

module CpuExecutor = struct
  module Control = struct
    type 'a t = {
      _HLT : 'a; (* Halt clock *)
      _MI : 'a; (* Memory address register in *)
      _RI : 'a; (* RAM data in *)
      _RO : 'a; (* RAM data out *)
      _IO : 'a; (* Instruction register out *)
      _II : 'a; (* Instruction register in *)
      _AI : 'a; (* A register in *)
      _AO : 'a; (* A register out *)
      _EO : 'a; (* ALU out *)
      _SU : 'a; (* ALU substract *)
      _BI : 'a; (* B register in *)
      _BO : 'a; (* B register out *)
      _OI : 'a; (* Output register in *)
      _CE : 'a; (* Program counter enable *)
      _CO : 'a; (* Program counter out *)
      _J : 'a; (* Jump (Program counter in) *)
      _FI : 'a; (* Flags in *)
    }
    [@@deriving sexp_of, hardcaml]
  end

  module I = struct
    type 'a t = { clock : 'a; enable : 'a; reset : 'a; cpu_reset : 'a; control : 'a Control.t }
    [@@deriving sexp_of, hardcaml]
  end

  module State = struct
    type 'a t = { opcode : 'a; [@bits 4] ready : 'a; flags : 'a Flags.O.t [@rtlmangle true] }
    [@@deriving sexp_of, hardcaml]
  end

  module Internal = struct
    type 'a t = {
      a : 'a Register.O.t; [@rtlmangle true]
      b : 'a Register.O.t; [@rtlmangle true]
      pc : 'a Pc.O.t; [@rtlmangle true]
    }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t = { output : 'a Output.O.t; [@rtlmangle true] state : 'a State.t; internal : 'a Internal.t }
    [@@deriving sexp_of, hardcaml]
  end

  let create ~rom ?(split_ram = true) scope i =
    let open Signal in
    let ( -- ) = Scope.naming scope in
    let clock = i.I.clock in
    let reset = i.I.reset in
    let w_data = wire Ram.bits -- "bus" in
    let memory_addr = MemoryAddr.create scope { Register.I.clock; reset; w_en = i.I.control._MI; w_data } in
    let memory, ready =
      let bus = { Ram.I.clock; w_en = i.control._RI; addr = memory_addr.data; w_data } in
      if split_ram then
        let memory = MemoryWithRom.create ~rom scope bus in
        (memory, Signal.vdd)
      else
        let memory = InitializedMemory.(create ~rom scope { bus; reset }) in
        (memory.bus, memory.ready)
    in
    let output = Output.create scope { Output.I.clock; reset; w_en = i.control._OI; w_data } in
    let a = Register.create scope { Register.I.clock; reset; w_en = i.control._AI; w_data } in
    let b = Register.create scope { Register.I.clock; reset; w_en = i.control._BI; w_data } in
    let pc =
      Pc.create scope
        {
          Pc.I.clock;
          reset = i.reset |: i.cpu_reset;
          enable = i.control._CE;
          w_en = i.control._J;
          w_data = uresize w_data Ram.bits_addr;
        }
    in
    let instruction = Instruction.create scope { clock; reset; w_en = i.control._II; w_data } in
    let alu =
      let op = Alu.Binary.Of_signal.of_raw i.control._SU in
      Alu.create scope { Alu.I.op; a = a.Register.O.data; b = b.Register.O.data }
    in
    let flags = Flags.create scope { clock; reset; w_en = i.control._FI; w_carry = alu.carry; w_zero = alu.zero } in
    let bus = Always.Variable.wire ~default:(zero Ram.bits) in
    a.data -- "A" |> ignore;
    b.data -- "B" |> ignore;
    pc.data -- "PC" |> ignore;
    memory_addr.data -- "ADDR" |> ignore;
    Always.(
      compile
        [
          when_ i.control._AO [ bus <-- a.data ];
          when_ i.control._BO [ bus <-- b.data ];
          when_ i.control._CO [ bus <-- uresize pc.data Ram.bits ];
          when_ i.control._EO [ bus <-- alu.data ];
          when_ i.control._IO [ bus <-- uresize instruction.addr Ram.bits ];
          when_ i.control._RO [ bus <-- memory.data ];
        ]);
    w_data <== bus.value;
    let state = { State.opcode = instruction.code; flags; ready } in
    let internal = { Internal.a; b; pc } in
    { O.output; state; internal }
end

module Counter (Max : Util.Integer) = struct
  let bits = Max.value - 1 |> Bits.num_bits_to_represent

  module I = struct
    type 'a t = { clock : 'a; enable : 'a; clear : 'a; reset : 'a } [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t = { data : 'a [@bits bits] } [@@deriving sexp_of, hardcaml]
  end

  let create scope i =
    ignore scope;
    let spec = Reg_spec.create ~clock:i.I.clock ~clear:i.I.reset () in
    let open Signal in
    let counter =
      reg_fb ~enable:i.I.enable ~width:bits
        ~f:(fun prev ->
          let next = prev +:. 1 in
          let carry = if Max.value = 1 lsl bits then gnd else next ==:. Max.value in
          let zero = zero bits in
          let clear = carry |: i.I.clear in
          mux2 clear zero next)
        spec
    in
    { O.data = counter }
end

module Isa = struct
  module Control = struct
    type t =
      | HLT (* Halt clock *)
      | MI (* Memory address register in *)
      | RI (* RAM data in *)
      | RO (* RAM data out *)
      | IO (* Instruction register out *)
      | II (* Instruction register in *)
      | AI (* A register in *)
      | AO (* A register out *)
      | EO (* ALU out *)
      | SU (* ALU substract *)
      | BI (* B register in *)
      | BO (* B register out *)
      | OI (* Output register in *)
      | CE (* Program counter enable *)
      | CO (* Program counter out *)
      | J (* Jump (Program counter in) *)
      | FI (* Flags in *)
    [@@deriving compare, enumerate]

    let count = List.length all
    let of_int = List.nth all
    let to_int a = Base.List.findi_exn all ~f:(fun _ b -> 0 == compare a b) |> fst
  end

  module Ucode = struct
    type cycle = Control.t list
    type t = cycle list

    let code_of_cycle cycle =
      List.map Control.to_int cycle |> List.map (fun n -> 1 lsl n) |> List.fold_left (fun b n -> b lor n) 0

    let code t = List.map code_of_cycle t
  end

  module Flag = struct
    type t = Z | C [@@deriving compare, enumerate]

    let count = List.length all
  end

  type t = { code : int; name : string; ucode : Ucode.t; flags : Flag.t list }

  let commands = ref []

  let ucode_nop =
    Control.
      [
        [ MI; CO ] (*                                  cycle 1 *);
        [ RO; II; CE ] (*                              cycle 2 *);
      ]

  let make ~name ~code ?flags ucode =
    let flags = Option.value ~default:[] flags in
    { code; name; flags; ucode = ucode_nop @ ucode }

  module Instruction = struct
    type args = Zero | One of int
    type inst = { code : int; args : args }

    let arg_bits = Ram.bits_addr
    let code_bits = Ram.bits - Ram.bits_addr
    let make ~code ~arg = (code lsl arg_bits) lor arg
  end

  let register ~name ~code ?flags ucode =
    let cmd = make ~name ~code ?flags ucode in
    commands := cmd :: !commands;
    ()

  let no_op name code ucode =
    register ~name ~code ucode;
    Instruction.{ code; args = Zero }

  let single_op ?flags name code ucode =
    register ~name ~code ?flags ucode;
    fun arg -> Instruction.{ code; args = One arg }

  let nop = no_op "NOP" 0b0000 []

  let lda =
    single_op "LDA" 0b0001
      Control.
        [
          [ IO; MI ] (*                                  cycle 3 *);
          [ RO; AI ] (*                                  cycle 4 *);
        ]

  let add =
    single_op "ADD" 0b0010
      Control.
        [
          [ IO; MI ] (*                                  cycle 3 *);
          [ RO; BI ] (*                                  cycle 4 *);
          [ EO; AI; FI ] (*                              cycle 5 *);
        ]

  let sub =
    single_op "SUB" 0b0011
      Control.
        [
          [ IO; MI ] (*                                  cycle 3 *);
          [ RO; BI ] (*                                  cycle 4 *);
          [ EO; AI; SU; FI ] (*                          cycle 5 *);
        ]

  let sta =
    single_op "STA" 0b0100
      Control.
        [
          [ IO; MI ] (*                                  cycle 3 *);
          [ AO; RI ] (*                                  cycle 4 *);
        ]

  let ldi = single_op "LDI" 0b0101 Control.[ [ IO; AI ] ]
  let jmp = single_op "JMP" 0b0110 Control.[ [ IO; J ] ]
  let jc = single_op ~flags:[ Flag.C ] "JC" 0b0111 Control.[ [ IO; J ] ]
  let jz = single_op ~flags:[ Flag.Z ] "JZ" 0b1000 Control.[ [ IO; J ] ]
  let out = no_op "OUT" 0b1110 Control.[ [ AO; OI ] ]
  let hlt = no_op "HLT" 0b1111 Control.[ [ HLT ] ]

  let assembler l =
    List.map
      (fun i ->
        let d = match i.Instruction.args with Zero -> 0 | One n -> n in
        Instruction.make ~code:i.code ~arg:d)
      l

  let bits, ucode_bits, max_cycles, ucode =
    let bits = List.length Control.all in
    let all = List.map (fun c -> (c.code, (c.flags, Ucode.code c.ucode))) !commands in
    let max_cycles =
      List.map (fun (_, (_, ucode)) -> List.length ucode) all |> Base.List.max_elt ~compare |> Option.get
    in
    let ucode_bits = max_cycles - 1 |> Bits.num_bits_to_represent in
    let max_ucode = 1 lsl ucode_bits in
    let ops_bits = 4 in
    let ops = 1 lsl ops_bits in
    let ucode =
      let flags = [ []; [ Flag.C ]; [ Flag.Z ]; [ Flag.C; Flag.Z ] ] in
      let ucode_flag flag =
        List.init ops (fun op ->
            let flags, ucode = List.assoc_opt op all |> Option.value ~default:([], []) in
            let ucode =
              match flags with
              | [] -> ucode
              | [ f ] -> if List.mem f flag then ucode else Ucode.code ucode_nop
              | _ -> assert false
            in
            let len = List.length ucode in
            let tail = List.init (max_ucode - len) (fun _ -> 0) in
            ucode @ tail)
        |> List.flatten
      in
      List.map ucode_flag flags |> List.flatten
    in
    (bits, ucode_bits, max_cycles, ucode)
end

module CpuControl = struct
  module I = struct
    type 'a t = { clock : 'a; enable : 'a; reset : 'a; cpu_reset : 'a; cpu : 'a CpuExecutor.State.t }
    [@@deriving sexp_of, hardcaml]
  end

  module O = CpuExecutor.Control

  let create scope i =
    let module Counter = Counter ((val Util.integer Isa.max_cycles)) in
    let open Signal in
    let spec = Reg_spec.create ~clock:i.I.clock ~clear:i.I.reset () in
    let ( -- ) = Scope.naming scope in
    let halt_next = wire 1 in
    let halt = reg spec halt_next -- "halt" in
    let counter_enable = ~:halt &: i.enable |: i.cpu_reset in
    let counter =
      Counter.create scope
        { Counter.I.clock = i.I.clock; reset = i.reset; enable = counter_enable; clear = i.cpu_reset }
    in
    let ( -- ) = Scope.naming scope in
    let cycle = counter.data -- "cycle" in
    let state = concat_msb [ i.I.cpu.flags.zero; i.I.cpu.flags.carry; i.I.cpu.opcode; cycle ] in
    let ucode = List.map (Signal.of_int ~width:Isa.bits) Isa.ucode |> mux state in
    let control c = Isa.Control.to_int c |> bit ucode in
    let halt = halt |: control HLT in
    let halt = halt &: ~:(i.I.cpu_reset) in
    halt_next <== halt;
    Isa.Control.
      {
        O._HLT = halt_next;
        _MI = control MI;
        _RI = control RI;
        _RO = control RO;
        _IO = control IO;
        _II = control II;
        _AI = control AI;
        _AO = control AO;
        _EO = control EO;
        _SU = control SU;
        _BI = control BI;
        _BO = control BI;
        _OI = control OI;
        _CE = control CE;
        _CO = control CO;
        _J = control J;
        _FI = control FI;
      }
end

module Cpu = struct
  module I = struct
    type 'a t = { clock : 'a; enable : 'a; cpu_reset : 'a; reset : 'a } [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t = {
      output : 'a Output.O.t; [@rtlmangle true]
      ready : 'a;
      internal : 'a CpuExecutor.Internal.t;
      flags : 'a Flags.O.t;
    }
    [@@deriving sexp_of, hardcaml]
  end

  let create ~rom ~split_ram scope i =
    let open Signal in
    let state =
      {
        CpuExecutor.State.opcode = wire Isa.Instruction.code_bits;
        ready = wire 1;
        flags = { Flags.O.zero = wire 1; carry = wire 1 };
      }
    in
    let clock = i.I.clock in
    let control =
      let enable = i.enable &: state.ready in
      CpuControl.(create scope { I.clock; enable; reset = i.reset; cpu_reset = i.cpu_reset; cpu = state })
    in
    let executor =
      CpuExecutor.(
        create ~rom ~split_ram scope { I.clock; enable = i.enable; reset = i.reset; cpu_reset = i.cpu_reset; control })
    in
    let open CpuExecutor.O in
    state.opcode <== executor.state.opcode;
    state.flags.zero <== executor.state.flags.zero;
    state.flags.carry <== executor.state.flags.carry;
    state.ready <== executor.state.ready;
    { O.output = executor.output; ready = executor.state.ready; internal = executor.internal; flags = state.flags }
end

module type CpuConfig = sig
  val rom : int list
  val split_ram : bool
end

let cpu_config ~rom ~split_ram : (module CpuConfig) =
  (module struct
    let rom = rom
    let split_ram = split_ram
  end)

let rom_prepare ~process code =
  let write addr data = List.mapi (fun n a -> if n = addr then data else a) in
  let extend n l =
    let len = List.length l in
    let zeros = List.init (n - len) (fun _ -> 2) in
    l @ zeros
  in
  code |> extend Ram.size |> process ~write
