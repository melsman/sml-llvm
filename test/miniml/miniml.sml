(* A Simple ML-like language ported to Standard ML from OCaml; see
     http://groups.google.com/group/fa.caml/msg/5aee553df34548e2
*)
    
datatype prim = Add | Sub | Leq

datatype expr = 
         Int of int 
       | Var of string 
       | BinOp of prim * expr * expr 
       | If of expr * expr * expr 
       | Apply of expr * expr 

datatype defn = 
         LetRec of string * string * expr 
    
structure L = LlvmCore

fun ty c = L.i64_type c 

type state = 
    { context: L.llcontext,
      func: L.llvalue,
      blk: L.llbasicblock,
      vars: (string * L.llvalue) list } 

fun bb (state:state) = L.builder_at_end (#context state) (#blk state)

fun new_block (state:state) name = L.append_block (#context state) name (#func state)

fun find (state:state) v = 
    case List.find (fn y => v = #1 y) (#vars state) of
      SOME v => #2 v
    | NONE => raise Fail ("Unknown variable " ^ v) 

fun cont (v, state) dest_blk = 
  let val _ = L.build_br dest_blk (bb state)
  in (v, state)
  end

fun expr (state:state) =
 fn Int n => (L.const_int (ty (#context state)) n, state)
  | Var x => (find state x, state)
  | BinOp(p, f, g) => 
      let val (f, state) = expr state f
          val (g, state) = expr state g
          val (build, name) =
              case p of
                Add => (L.build_add, "add")
              | Sub => (L.build_sub, "sub")
              | Leq => (L.build_icmp L.Icmp.Sle, "leq")
      in (build f g name (bb state), state)
      end
  | If(p, t, f) => 
      let val t_blk = new_block state "pass"
          val f_blk = new_block state "fail"
          val k_blk = new_block state "cont"
          val (cond, state) = expr state p
          val _ = L.build_cond_br cond t_blk f_blk (bb state)
          val state = {func=(#func state),blk=t_blk,vars=(#vars state),context=(#context state)}
          val (t, state) = cont (expr state t) k_blk
          val state = {func=(#func state),blk=f_blk,vars=(#vars state),context=(#context state)}
          val (f, state) = cont (expr state f) k_blk
      in (L.build_phi [(t, t_blk), (f, f_blk)] "join" (bb state), state)
      end
  | Apply(f, arg) => 
      let val (f, state) = expr state f
          val (arg, state) = expr state arg 
      in (L.build_call f [arg] "apply" (bb state), state) 
      end

fun defn context m (LetRec(f, arg, body), vars) = 
    let val t = ty context
        val fty = L.function_type t [t]
        val func = L.define_function f fty m
        val vars' = (arg, L.param func 0) :: (f, func) :: vars
        val state = {func=func, blk = L.entry_block func, vars=vars', context=context}
        val (body, state) = expr state body
        val _ = L.build_ret body (bb state)
    in (f, func) :: vars
    end

fun int c n = L.const_int (ty c) n 

fun mk_module (program,run) =
  let val c = L.create_context()
      val m = L.create_module c "themodule"
      val string = L.pointer_type (L.i8_type c)
      val printf = L.declare_function "printf" (L.var_arg_function_type (ty c) [string]) m
      val main = L.define_function "main" (L.function_type (ty c) []) m
      val blk = L.entry_block main
      val bb = L.builder_at_end c blk
      fun str s = L.define_global "buf" (L.const_stringz c s) m
      val int_spec = L.build_gep (str "Running MiniML function...\nResult = %d\n") [int c 0, int c 0] "int_spec" bb
      val vars = List.foldl (defn c m) [] program
      val state = {func=main,blk=blk,vars=vars,context=c}
      val (n, _) = expr state run
      val _ = L.build_call printf [int_spec, n] "" bb
      val _ = L.build_ret (int c 0) bb
  in (m,c,main)
  end

fun compile prog filename = 
    let val (m,c,_) = mk_module prog
    in if not (L.write_bitcode_file m filename) then ()
       else (L.dispose_module m;
             L.dispose_context c)
    end

structure GV = L.GenericValue
structure EE = L.ExecutionEngine

fun run create_engine prog =
    let val (m,c,main) = mk_module prog
        val ee = create_engine m
    in EE.run_function main [] ee; ()
    end

local
  fun add a b = BinOp(Add,a,b)
  fun sub a b = BinOp(Sub,a,b)
  fun iff c a b = If(c,a,b)
  fun leq a b = BinOp(Leq,a,b)
  fun i n = Int n
  fun $ s = Var s
  fun apply n b = Apply(Var n,b)
in
val ex1 =
    ([LetRec("fib", "n", iff (leq ($"n") (i 2))
                             (i 1)
                             (add (apply ("fib") (sub ($"n") (i 1)))
                                  (apply ("fib") (sub ($"n") (i 2)))))
     ],
     apply "fib" (i 40))
end

val () =
    case CommandLine.arguments() of
      ["-jit"] => (EE.initialize_native_target(); run (fn m => EE.create_jit m 3) ex1)
    | ["-interp"] => run EE.create_interpreter ex1
    | [filename] => compile ex1 filename
    | _ => print ("Usage: " ^ CommandLine.name() ^ " <file>\n")
