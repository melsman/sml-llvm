(* Simple unit tests for the MLKit llvm bindings *)

(* Auxiliary functions for test cases *)

signature UTEST = sig
  val start : string -> string -> unit
  val finish : unit -> unit
  val tst : string -> (unit -> bool) -> unit
end

structure UTest : UTEST = struct

  val counts = {ok=ref 0, wrong=ref 0, exn=ref 0}
  fun incr l =
      let val r = l counts
      in r := !r + 1
      end
  fun ok() = (incr #ok; "OK")
  fun wrong() = (incr #wrong; "WRONG")
  fun exn() = (incr #exn; "EXN")
  fun check f = (if f () then ok() else wrong()) handle e => (exn() ^ General.exnMessage e)

(*      
  fun range (from, to) p = 
      let open Int32
      in (from > to) orelse (p from) andalso (range (from+1, to) p)
      end

  fun checkrange bounds = check o range bounds
*)

  fun tst0 s s' = print (s ^ "    \t" ^ s' ^ "\n")
  fun tst s f = tst0 s (check f)
      
(*  fun tstrange s bounds = (tst s) o range bounds *)

  val data : (string*string) option ref = ref NONE
  fun start f s =
      (data := SOME (f,s);
       #ok counts := 0;
       #wrong counts := 0;
       #exn counts := 0;
       print ("[File " ^ f ^ ": Testing " ^ s ^ "...]\n"))

  fun finish () =
      let val ok = ! (#ok counts)
          val wrong = ! (#wrong counts)
          val exn =  ! (#exn counts)
      in
        case !data of
          NONE => print "[Test not properly started]\n"
        | SOME (f,s) =>
          (print ("[Finished testing file " ^ f ^ " - " ^ s ^ "]\n");
           if wrong = 0 andalso exn = 0 then
             print ("[All " ^ Int.toString ok ^ " tests succeeded]\n")
           else
             print ("[Tests failed - [ok: " ^ Int.toString ok ^ ", wrong: " ^ Int.toString wrong ^ ", exn: " ^ Int.toString exn ^ "]\n")
          )
      end
end

local
  val is_debug = false
in
fun debug s = if is_debug then print s
              else ()
end

open UTest

val () = UTest.start "unittest.sml" "structure LlvmCore"

structure L = LlvmCore

fun ty c = L.i64_type c 

val _ = tst "context_create" (fn () => (L.create_context(); true))
val _ = tst "context_create2" (fn () => let val c1 = L.create_context()
                                             val c2 = L.create_context()
                                         in c1 <> c2
                                         end)
val _ = tst "context_dispose" (fn () => let val c = L.create_context()
                                             val () = L.dispose_context c
                                         in true
                                         end)
(* seg fault
val _ = tst "context_dispose2" (fn () => let val c = L.create_context()
                                              val () = L.dispose_context c
                                              val () = L.dispose_context c
                                         in true
                                         end)
*)

val _ = tst "context_global" (fn () => (L.global_context(); true))

fun tstc s f =
    tst s (fn () => let val c = L.create_context()
                        val r = f c
                        val () = L.dispose_context c
                    in r
                    end)

val _ = tstc "module_create" (fn c => (L.create_module c "MyModule"; true))
val _ = tstc "module_create2" (fn c => (L.create_module c "My Module"; true))
val _ = tstc "module_create3" (fn c => (L.create_module c "A"; L.create_module c "A"; true))
val _ = tstc "module_dispose" (fn c => let val m = L.create_module c "A"
                                           val () = L.dispose_module m
                                       in true
                                       end)
val _ = tstc "module_context" (fn c => let val m = L.create_module c "A"
                                           val c' = L.module_context m
                                           val r = c = c'
                                           val () = L.dispose_module m
                                       in r
                                       end)

(* Types *)

fun tst_itype (s, f, n) =
    tstc ("type_" ^ s) (fn c => let val t = f c
                                in L.integer_bitwidth t = n andalso L.integer_type c n = t
                                end)

val () = List.app tst_itype [("i1", L.i1_type, 1),
                             ("i8", L.i8_type, 8),
                             ("i16", L.i16_type, 16),
                             ("i32", L.i32_type, 32),
                             ("i64", L.i64_type, 64)
                            ]

fun pr_ty(ty:L.lltype): unit = prim("@mlkit_llvm_print_type", ty)

val _ = tstc "type_fun" (fn c => let val i32 = L.i32_type c
                                     (*val () = pr_ty i32*)
                                     val i16 = L.i16_type c
                                     val t = L.function_type i32 []
                                     val r = L.return_type t                                    
                                 in r = i32 andalso r <> i16 andalso not(L.is_var_arg t)
                                 end)

val _ = tstc "type_var_arg_fun" (fn c => let val i32 = L.i32_type c
                                             val i16 = L.i16_type c
                                             val t = L.var_arg_function_type i32 []
                                             val r = L.return_type t                                    
                                         in r = i32 andalso r <> i16 andalso L.is_var_arg t
                                         end)
(*MEMO: is_var_arg on non-function types *)

val _ = tstc "type_pointer" (fn c => let val i32 = L.i32_type c
                                         val pi32 = L.pointer_type i32
                                     in pi32 <> i32
                                     end)

val _ = tstc "const_int" (fn c => let val i32 = L.i32_type c
                                      val v = L.const_int i32 456
                                      val t = L.type_of v
                                  in t = i32
                                  end)

val _ = tstc "const_string" (fn c => let val v = L.const_string c "hello"
                                         val t = L.type_of v
                                         val te = L.element_type t
                                         val sz = L.array_length t
                                     in te = L.i8_type c andalso sz = 5
                                     end)

val _ = tstc "const_stringz" (fn c => let val v = L.const_stringz c "hello"
                                          val t = L.type_of v
                                          val te = L.element_type t
                                         val sz = L.array_length t
                                      in te = L.i8_type c andalso sz = 6
                                      end)

val _ = tstc "define_global_string" (fn c => let val m = L.create_module c "A"
                                                 val v = L.const_string c "hello"
                                                 val g = L.define_global "MyGlobalString" v m
                                                 val t = L.type_of g
                                             in t = L.pointer_type (L.type_of v)
                                             end)
val _ = tstc "define_global_int" (fn c => let val m = L.create_module c "A"
                                              val i32 = L.i32_type c
                                              val v = L.const_int i32 456
                                              val g = L.define_global "MyGlobalInt" v m
                                              val t = L.type_of g
                                          in t = L.pointer_type i32
                                          end)

val _ = tstc "declare_function" (fn c => let val m = L.create_module c "A"
                                             val i32 = L.i32_type c
                                             val t = L.function_type i32 [i32]
                                             val v = L.declare_function "myf" t m
                                             val t2 = L.type_of v
                                          in t2 = L.pointer_type t
                                          end)

val _ = tstc "define_function" (fn c => let val m = L.create_module c "A"
                                             val i32 = L.i32_type c
                                             val t = L.function_type i32 [i32]
                                             val v = L.declare_function "myf" t m
                                             val t2 = L.type_of v
                                          in t2 = L.pointer_type t
                                          end)

(* MEMO: test function param *)

(* Basic Blocks *)

fun tstf0 s tf f =
    tstc s (fn c => let val m = L.create_module c "A"
                        val t = tf c
                        val v = L.define_function "myf" t m
                        val r = f (c,m,t,v)
                        val () = L.dispose_module m
                    in r
                    end)

fun tf_i2i c =
    let val i32 = L.i32_type c in L.function_type i32 [i32] end

fun tf_ii2i c =
    let val i32 = L.i32_type c in L.function_type i32 [i32,i32] end

fun tf_dd2d c =
    let val d = L.double_type c in L.function_type d [d,d] end

fun tf_i2d c = L.function_type (L.double_type c) [L.i32_type c]
fun tf_d2i c = L.function_type (L.i32_type c) [L.double_type c]

fun tstf s f = tstf0 s tf_i2i f

val _ = tstf "entry_block" (fn (c,m,t,f) => 
                               let val b = L.entry_block f
                               in true
                               end)

val _ = tstf "append_block" (fn (c,m,t,f) => 
                               let val b = L.append_block c "newblock" f
                               in true
                               end)

val _ = tstf "delete_block" (fn (c,m,t,f) => 
                                let val b = L.append_block c "newblock" f
                                    val () = L.delete_block b
                                in true
                                end)

(* Builders *)

val _ = tstf "builder_at_end" (fn (c,m,t,f) => 
                                let val b = L.append_block c "newblock" f
                                    val bb = L.builder_at_end c b
                                    val () = L.delete_block b
                                in true
                                end)

(* Instructions *)

fun tstb0 s tf f =
    tstf0 s tf (fn (c,m,t,vf) => 
                   let val b = L.entry_block vf
                       val bb = L.builder_at_end c b
                       val r = f(c,m,t,vf,b,bb)
                       val () = L.delete_block b
                   in r
                   end)

fun tstb s f = tstb0 s tf_i2i f  

val _ = tstb "build_ret_void" (fn (c,m,t,f,b,bb) =>
                                  let val v = L.build_ret_void bb
                                  in true
                                  end)

val _ = tstb "build_ret" (fn (c,m,t,f,b,bb) =>
                             let val i32 = L.i32_type c
                                 val v = L.const_int i32 454
                                 val v2 = L.build_ret v bb
                             in true
                             end)

val _ = tstb "build_br" (fn (c,m,t,f,b,bb) =>
                             let val v = L.build_br b bb
                             in true
                             end)

val _ = tstb "build_cond_br" (fn (c,m,t,f,b,bb) =>
                                 let val i32 = L.i32_type c
                                     val v = L.const_int i32 454
                                     val vc = L.build_icmp L.Icmp.Eq v v "cond" bb
                                     val b1 = L.append_block c "b1" f
                                     val b2 = L.append_block c "b2" f
                                     val v2 = L.build_cond_br vc b1 b2 bb
                                 in true
                                 end)

fun tstbin s binop =
    tstb s (fn (c,m,t,f,b,bb) =>
               let val i32 = L.i32_type c
                   val v = L.const_int i32 454
                   val v2 = binop v v "res" bb
                   val v3 = L.build_ret v2 bb
               in true
               end)

fun tstun s unop =
    tstb s (fn (c,m,t,f,b,bb) =>
               let val i32 = L.i32_type c
                   val v = L.const_int i32 454
                   val v2 = unop v "res" bb
                   val v3 = L.build_ret v2 bb
               in true
               end)

val _ = tstbin "build_add" L.build_add
val _ = tstbin "build_sub" L.build_sub
val _ = tstbin "build_mul" L.build_mul
val _ = tstbin "build_shl" L.build_shl
val _ = tstbin "build_lshr" L.build_lshr
val _ = tstbin "build_ashr" L.build_ashr
val _ = tstbin "build_and" L.build_and
val _ = tstbin "build_or" L.build_or
val _ = tstbin "build_xor" L.build_xor
val _ = tstun "build_neg" L.build_neg
val _ = tstun "build_not" L.build_not

val _ = tstb "build_gep" (fn (c,m,t,f,b,bb) =>
                             let val i32 = L.i32_type c
                                 val v = L.const_int i32 454
                                 val v2 = L.build_gep v [] "res" bb
                             in true
                             end)

val _ = 
    let open L.Icmp
    in List.app
           (fn (s,c) => tstbin ("build_icmp_" ^ s) (L.build_icmp c))
           [("Eq",Eq),("Ne",Ne),("Ugt",Ugt),("Uge",Uge),("Ult",Ult),
            ("Ule",Ule),("Sgt",Sgt),("Sge",Sge),("Slt",Slt),("Sle",Sle)]
    end

fun tstbinf s binop =
    tstb s (fn (c,m,t,f,b,bb) =>
               let val d = L.double_type c
                   val v = L.const_float d 34.23
                   val v2 = binop v v "res" bb
                   val v3 = L.build_ret v2 bb
               in true
               end)

fun tstunf s unop =
    tstb s (fn (c,m,t,f,b,bb) =>
               let val d = L.double_type c
                   val v = L.const_float d 34.23
                   val v2 = unop v "res" bb
                   val v3 = L.build_ret v2 bb
               in true
               end)

val _ = 
    let open L.Fcmp
    in List.app
           (fn (s,c) => tstbinf ("build_fcmp_" ^ s) (L.build_fcmp c))
           [("False",False),("Oeq",Oeq),("Ogt",Ogt),("Oge",Oge),("Olt",Olt),
            ("Ole",Ole),("One",One),("Ord",Ord),("Uno",Uno),
            ("Ueq",Ueq),("Ugt",Ugt),("Uge",Uge),("Ult",Ult),
            ("Ule",Ule),("Une",Une),("True",True)]
    end

val _ = tstbinf "build_fadd" L.build_fadd
val _ = tstbinf "build_fsub" L.build_fsub
val _ = tstbinf "build_fmul" L.build_fmul
val _ = tstbinf "build_fdiv" L.build_fdiv
val _ = tstbinf "build_frem" L.build_frem
val _ = tstunf "build_fneg" L.build_fneg

val _ = tstb "build_phi" (fn (c,m,t,f,b,bb) =>
                             let val i32 = L.i32_type c
                                 val v = L.const_int i32 454
                                 val v1 = L.build_add v v "v1" bb
                                 val b1 = L.append_block c "b1" f
                                 val b2 = L.append_block c "b2" f
                                 val bb2 = L.builder_at_end c b2
                                 val _ = L.build_br b1 bb
                                 val v2 = L.build_mul v v "v2" bb2
                                 val _ = L.build_br b1 bb2
                                 val bb1 = L.builder_at_end c b1
                                 val v3 = L.build_phi [(v1,b),(v2,b2)] "phi" bb1
                             in true
                             end)

val _ = tstb "build_call" (fn (c,m,t,f,b,bb) =>
                             let val i32 = L.i32_type c
                                 val v1 = L.const_int i32 454                                
                                 val v = L.build_call f [v1] "res" bb
                             in true
                             end)

val _ = tstb "write_bitcode_file" (fn (c,m,t,f,b,bb) =>
                                      let val i32 = L.i32_type c
                                          val v0 = L.param f 0
                                          val v = L.const_int i32 454
                                          val v2 = L.build_mul v0 v "v2" bb
                                          val _ = L.build_ret v2 bb
                                      in L.write_bitcode_file m "test.bc"
                                      end)

val _ = tstc "string_of_lltype" (fn c => let val i32 = L.i32_type c
                                             val d = L.double_type c
                                             val pd = L.pointer_type d
                                         in List.all (fn (t,s) => L.string_of_lltype t = s)
                                                     [(i32,"i32"),
                                                      (d,"double"),
                                                      (pd,"double*")
                                                     ]
                                         end)

structure GV = L.GenericValue
structure EE = L.ExecutionEngine

val _ = tstb "execution_engine"
             (fn (c,m,t,f,b,bb) =>
                 let val i32 = L.i32_type c
                     val v0 = L.param f 0
                     val v1 = L.const_int i32 45
                     val v2 = L.build_mul v0 v1 "v2" bb
                     val _ = L.build_ret v2 bb
                     val ee = EE.create m
                     val arg = GV.of_int i32 23
                 in case L.ExecutionEngine.find_function "myf" ee of
                      SOME f2 =>
                      let val r = EE.run_function f [arg] ee
                          val res = GV.as_int r = 45 * 23 andalso f = f2
                          (* val () = EE.dispose ee *)
                      in res
                      end
                    | NONE => raise Fail "cannot find function"
                 end)

fun pp_t s t =
    print (s ^ ": " ^ L.string_of_lltype t ^ "\n")

fun tst_ii2i (p,create_engine) (s, g, a0, a1, e) =
    tstb0 (p ^ "-" ^ s) tf_ii2i 
         (fn (c,m,t,f,b,bb) =>
             let val i32 = L.i32_type c
                 val v0 = L.param f 0
                 val v1 = L.param f 1
                 val v2 = g v0 v1 "v2" bb
                 val _ = L.build_ret v2 bb
                 val ee = create_engine m
                 val arg0 = GV.of_int i32 a0
                 val arg1 = GV.of_int i32 a1
                 val r = EE.run_function f [arg0,arg1] ee
             in GV.as_int r = e
             end)

fun unary (s,f,a,r) =
    (s, fn v0 => fn _ => f v0, a, 333, r)

val test_cases_ii2i =
    [("add1",L.build_add,2,3,5),
     ("add2",L.build_add,2,~3,~1),
     ("add3",L.build_add,2,0,2),
     ("sub1",L.build_sub,2,3,~1),
     ("sub2",L.build_sub,2,~3,5),
     ("sub3",L.build_sub,2,0,2),
     ("mul1",L.build_mul,2,3,6),
     ("mul2",L.build_mul,2,~3,~6),
     ("mul3",L.build_mul,2,0,0),
     ("shl1",L.build_shl,2,1,4),
     ("shl2",L.build_shl,5,2,20),
     ("shl3",L.build_shl,323,0,323),
     ("lshr1",L.build_lshr,5,1,2),
     ("lshr2",L.build_lshr,27,2,6),
     ("lshr3",L.build_lshr,28,0,28),
     ("ashr1",L.build_ashr,5,1,2),
     ("ashr2",L.build_ashr,27,2,6),
     ("ashr3",L.build_ashr,28,0,28),
     ("and1",L.build_and,2,1,0),
     ("and2",L.build_and,5,4,4),
     ("and3",L.build_and,7,9,1),
     ("or1",L.build_or,2,1,3),
     ("or2",L.build_or,5,2,7),
     ("or3",L.build_or,5,1,5),
     ("xor1",L.build_xor,2,1,3),
     ("xor2",L.build_xor,5,2,7),
     ("xor3",L.build_xor,5,1,4),
     ("not",fn v0 => fn v1 => fn n => fn bb =>
                                         let val tmp = L.build_not v0 "tmp" bb
                                         in L.build_and tmp v1 n bb
                                         end, 5, 7, 2),
     unary ("neg1",L.build_neg,5,~5),
     unary ("neg2",L.build_neg,~5,5),
     unary ("neg3",L.build_neg,0,0)
    ]

fun run_tsts tst cases =
    (List.app (tst ("interp", EE.create_interpreter)) cases;
     List.app (tst ("hybrid", EE.create)) cases;
     List.app (tst ("jit", fn m => EE.create_jit m 3)) cases)

val _ = EE.initialize_native_target()
val _ = run_tsts tst_ii2i test_cases_ii2i

fun eq (a:real) b =
    let val r = a <= b andalso b <= a
        val () = debug (Real.toString a ^ " == " ^ Real.toString b ^ " = " ^ Bool.toString r ^ "\n")
    in r
    end

fun tst_dd2d (p,create_engine) (s, g, a0, a1, e) =
    tstb0 (p ^ "-" ^ s) tf_dd2d 
         (fn (c,m,t,f,b,bb) =>
             let val d = L.double_type c
                 val v0 = L.param f 0
                 val v1 = L.param f 1
                 val v2 = g v0 v1 "v2" bb
                 val _ = L.build_ret v2 bb
                 val ee = create_engine m
                 val arg0 = GV.of_float d a0
                 val arg1 = GV.of_float d a1
                 val r = EE.run_function f [arg0,arg1] ee
             in eq (GV.as_float d r) e
             end)

fun funary (s,f,a,r) =
    (s, fn v0 => fn _ => f v0, a, 333.0, r)

val test_cases_dd2d =
    [("fadd1",L.build_fadd,2.0,3.0,5.0),
     ("fadd2",L.build_fadd,2.0,~3.0,~1.0),
     ("fadd3",L.build_fadd,2.0,0.0,2.0),
     ("fsub1",L.build_fsub,2.0,3.0,~1.0),
     ("fsub2",L.build_fsub,2.0,~3.0,5.0),
     ("fsub3",L.build_fsub,2.0,0.0,2.0),
     ("fmul1",L.build_fmul,2.0,3.0,6.0),
     ("fmul2",L.build_fmul,2.0,~3.0,~6.0),
     ("fmul3",L.build_fmul,2.0,0.0,0.0),
     ("fdiv1",L.build_fdiv,6.0,3.0,2.0),
     ("fdiv2",L.build_fdiv,8.0,~0.5,~16.0),
     ("fdiv3",L.build_fdiv,0.0,2.0,0.0),
     ("frem1",L.build_frem,6.0,3.0,0.0),
     ("frem2",L.build_frem,8.0,3.0,2.0),
     ("frem3",L.build_frem,0.0,2.0,0.0),
     funary ("fneg1",L.build_fneg,5.0,~5.0),
     funary ("fneg2",L.build_fneg,~5.0,5.0),
     funary ("fneg3",L.build_fneg,0.0,0.0)
    ]

val _ = run_tsts tst_dd2d test_cases_dd2d

fun tst_i2d (p,create_engine) (s, g, a, e) =
    tstb0 (p ^ "-" ^ s) tf_i2d 
         (fn (c,m,t,f,b,bb) =>
             let val d = L.double_type c
                 val i32 = L.i32_type c
                 val v0 = L.param f 0
                 val v2 = g c v0 "v2" bb
                 val _ = L.build_ret v2 bb
                 val ee = create_engine m
                 val arg = GV.of_int i32 a
                 val r = EE.run_function f [arg] ee
             in eq (GV.as_float d r) e
             end)

val test_cases_i2d =
    [("sitofp1", fn c => fn v => L.build_sitofp v (L.double_type c), 3, 3.0),
(*     ("sitofp2", fn c => fn v => L.build_sitofp v (L.double_type c), ~3, ~3.0), *)
     ("uitofp1", fn c => fn v => L.build_uitofp v (L.double_type c), 3, 3.0)
(*     ("uitofp2", fn c => fn v => L.build_uitofp v (L.double_type c), ~3, 3.0) *)
    ]

val _ = run_tsts tst_i2d test_cases_i2d

fun tst_d2i (p,create_engine) (s, g, a, e) =
    tstb0 (p ^ "-" ^ s) tf_d2i 
         (fn (c,m,t,f,b,bb) =>
             let val d = L.double_type c
                 val i32 = L.i32_type c
                 val v0 = L.param f 0
                 val v2 = g c v0 "v2" bb
                 val _ = L.build_ret v2 bb
                 val ee = create_engine m
                 val arg = GV.of_float d a
                 val r = EE.run_function f [arg] ee
             in GV.as_int r = e
             end)

val _ = run_tsts tst_d2i
        [("fptosi1", fn c => fn v => L.build_fptosi v (L.i32_type c), 3.0, 3),
         ("fptoui1", fn c => fn v => L.build_fptosi v (L.i32_type c), 3.0, 3)
        ]

val () = finish()                           
