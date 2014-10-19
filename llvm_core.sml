structure LlvmCore :> LLVM_CORE = struct

fun die s = raise Fail ("LlvmCore." ^ s)

type llcontext = foreignptr
type llmodule = foreignptr
type lltype = foreignptr
type llvalue = foreignptr
type lluse = foreignptr
type llbasicblock = foreignptr
type llbuilder = foreignptr
type llmemorybuffer = foreignptr

structure TypeKind = struct
  datatype t = Void | Half | Float | Double | X86fp80 | Fp128 | Ppc_fp128 | 
       Label | Integer | Function | Struct | Array | Pointer | Vector |
       Metadata

  val toString = fn Void => "Void" | Half => "Half" | Float => "Float" | Double => "Double" 
                  | X86fp80 => "X86fp80" | Fp128 => "Fp128" | Ppc_fp128 => "Ppc_fp128"
                  | Label => "Label" | Integer => "Integer" | Function => "Function"
                  | Struct => "Struct" | Array => "Array" | Pointer => "Pointer"
                  | Vector => "Vector" | Metadata => "Metadata"
  val fromInt =
      fn 0 => Void | 1 => Half | 2 => Float | 3 => Double | 4 => X86fp80 | 5 => Fp128
       | 6 => Ppc_fp128 | 7 => Label | 8 => Integer | 9 => Function | 10 => Struct
       | 11 => Array | 12 => Pointer | 13 => Vector | 14 => Metadata 
       | n => die ("TypeKind.fromInt: unknown value " ^ Int.toString n)
end

structure Linkage = struct
  datatype t = External | Available_externally | Link_once | Link_once_odr | 
       Weak | Weak_odr | Appending | Internal | Private | Dllimport |
       Dllexport | External_weak | Ghost | Common | Linker_private
end

structure Visibility = struct
  datatype t = Default | Hidden | Protected
end

structure CallConv = struct
  val c = 0
  val fast = 8
  val cold = 9
  val x86_stdcall = 64
  val x86_fastcall = 65
end

structure Attribute = struct
  datatype t = Zext | Sext | Noreturn | Inreg | Structret | Nounwind |
       Noalias | Byval | Nest | Readnone | Readonly | Noinline |
       Alwaysinline | Optsize | Ssp | Sspreq | Alignment of int | Nocapture |
       Noredzone | Noimplicitfloat | Naked | Inlinehint |
       Stackalignment of int | ReturnsTwice | UWTable | NonLazyBind
end

structure Icmp = struct
  datatype t = Eq | Ne | Ugt | Uge | Ult | Ule | Sgt | Sge | Slt | Sle
  val index =
   fn Eq => 0 | Ne => 1 | Ugt => 2 | Uge => 3 | Ult => 4 | Ule => 5 | Sgt => 6 | Sge => 7 | Slt => 8 | Sle => 9
end

structure Fcmp = struct
  datatype t = False | Oeq | Ogt | Oge | Olt | Ole | One | Ord | Uno |
       Ueq | Ugt | Uge | Ult | Ule | Une | True
  val index =
   fn False => 0 | Oeq => 1 | Ogt => 2 | Oge => 3 | Olt => 4 | Ole => 5 | One => 6 | Ord => 7 | Uno => 8 |
      Ueq => 9 | Ugt => 10 | Uge => 11 | Ult => 12 | Ule => 13 | Une => 14 | True => 15
end

structure Opcode  = struct
  datatype t = 
       (* not an instruction *)
       Invalid | 

       (* Terminator Instructions *)
       Ret | Br | Switch | IndirectBr | Invoke | Invalid2 |
       Unreachable |

       (* Standard Binary Operators *)
       Add | FAdd | Sub | FSub | Mul | FMul | UDiv | SDiv | FDiv |
       URem | SRem | FRem |

       (* Logical Operators *)
       Shl | LShr | AShr | And | Or | Xor |

       (* Memory Operators *)
       Alloca | Load | Store | GetElementPtr |

       (* Cast Operators *)
       Trunc | ZExt | SExt | FPToUI | FPToSI | UIToFP | SIToFP |
       FPTrunc | FPExt | PtrToInt | IntToPtr | BitCast |

       (* Other Operators *)
       ICmp | FCmp | PHI | Call | Select | UserOp1 | UserOp2 | VAArg |
       ExtractElement | InsertElement | ShuffleVector | ExtractValue |
       InsertValue | Fence | AtomicCmpXchg | AtomicRMW | Resume |
       LandingPad | Unwind
end

structure ValueKind = struct
  datatype t = NullValue | Argument | BasicBlock | InlineAsm | MDNode |
       MDString | BlockAddress | ConstantAggregateZero | ConstantArray |
       ConstantExpr | ConstantFP | ConstantInt | ConstantPointerNull |
       ConstantStruct | ConstantVector | Function | GlobalAlias |
       GlobalVariable | UndefValue | Instruction of Opcode.t
end

(*
datatype ('a, 'b) llpos = At_end of 'a | Before of 'b

datatype ('a, 'b) llrev_pos = At_start of 'a | After of 'b
*)

exception IoError of string

(*===-- Contexts ----------------------------------------------------------===*)
val create_context : unit -> llcontext = fn () => prim("@LLVMContextCreate",())
val dispose_context : llcontext -> unit = fn C => prim("@LLVMContextDispose", C)
val global_context : unit -> llcontext = fn () => prim("@LLVMGetGlobalContext",())
val mdkind_id : llcontext -> string -> int = fn C => fn Name => prim("@LLVMGetMDKindIDInContext",(C,Name,size Name))

(*===-- Modules -----------------------------------------------------------===*)
val create_module : llcontext -> string -> llmodule = fn C => fn n => prim("@LLVMModuleCreateWithNameInContext", (n,C))

val dispose_module : llmodule -> unit = fn m => prim("@LLVMDisposeModule", m)

(*
external target_triple: llmodule -> string = "llvm_target_triple"
external set_target_triple: string -> llmodule -> unit = "llvm_set_target_triple"
external data_layout: llmodule -> string = "llvm_data_layout"
external set_data_layout: string -> llmodule -> unit = "llvm_set_data_layout"
external dump_module : llmodule -> unit = "llvm_dump_module"
external set_module_inline_asm : llmodule -> string -> unit = "llvm_set_module_inline_asm"
*)

val module_context : llmodule -> llcontext = fn m => prim("@LLVMGetModuleContext", m)

(*===-- Types -------------------------------------------------------------===*)

val classify_type : lltype -> TypeKind.t =
    fn t => let val i : int = prim("@LLVMGetTypeKind", t)
            in TypeKind.fromInt i
            end
val type_context : lltype -> llcontext =
    fn t => prim("@LLVMGetTypeContext", t)
(*
external type_is_sized : lltype -> bool = "llvm_type_is_sized"
*)

(*--... Operations on integer types ........................................--*)
val i1_type : llcontext -> lltype = fn C => prim("@LLVMInt1TypeInContext",C)
val i8_type : llcontext -> lltype = fn C => prim("@LLVMInt8TypeInContext",C)
val i16_type : llcontext -> lltype = fn C => prim("@LLVMInt16TypeInContext",C)
val i32_type : llcontext -> lltype = fn C => prim("@LLVMInt32TypeInContext",C)
val i64_type : llcontext -> lltype = fn C => prim("@LLVMInt64TypeInContext",C)
val integer_type : llcontext -> int -> lltype = fn C => fn width => prim("@LLVMIntTypeInContext", (C,width))
val integer_bitwidth : lltype -> int = fn t => prim("@LLVMGetIntTypeWidth", t)

(*--... Operations on real types ...........................................--*)

val float_type : llcontext -> lltype =
 fn c => prim("@LLVMFloatTypeInContext", c)
val double_type : llcontext -> lltype =
 fn c => prim("@LLVMDoubleTypeInContext", c)
(*
external x86fp80_type : llcontext -> lltype = "llvm_x86fp80_type"
external fp128_type : llcontext -> lltype = "llvm_fp128_type"
external ppc_fp128_type : llcontext -> lltype = "llvm_ppc_fp128_type"
*)

(*--... Operations on function types .......................................--*)
val function_type : lltype -> lltype list -> lltype = fn t => fn l => prim("mlkit_llvm_function_type",(t,l))
val var_arg_function_type : lltype -> lltype list -> lltype = fn t => fn l => prim("mlkit_llvm_var_arg_function_type",(t,l))
val is_var_arg : lltype -> bool = fn t => prim("@LLVMIsFunctionVarArg",t)
val return_type : lltype -> lltype = fn t => prim("@LLVMGetReturnType",t)

(*
external param_types : lltype -> lltype array = "llvm_param_types"
*)

(*--... Operations on struct types .........................................--*)
(*
external struct_type : llcontext -> lltype array -> lltype = "llvm_struct_type"
external packed_struct_type : llcontext -> lltype array -> lltype
                            = "llvm_packed_struct_type"
external struct_name : lltype -> string option = "llvm_struct_name"
external named_struct_type : llcontext -> string -> lltype =
    "llvm_named_struct_type"
external struct_set_body : lltype -> lltype array -> bool -> unit =
    "llvm_struct_set_body"
external struct_element_types : lltype -> lltype array
                              = "llvm_struct_element_types"
external is_packed : lltype -> bool = "llvm_is_packed"
external is_opaque : lltype -> bool = "llvm_is_opaque"
*)

(*--... Operations on pointer, vector, and array types .....................--*)

(*
external array_type : lltype -> int -> lltype = "llvm_array_type"
*)
val pointer_type : lltype -> lltype = fn t => prim("@LLVMPointerType", (t,0))
(*
external qualified_pointer_type : lltype -> int -> lltype
                                = "llvm_qualified_pointer_type"
external vector_type : lltype -> int -> lltype = "llvm_vector_type"
*)
val element_type : lltype -> lltype = fn t => prim("@LLVMGetElementType", t)
val array_length : lltype -> int = fn t => prim("@LLVMGetArrayLength", t)

(*
external address_space : lltype -> int = "llvm_address_space"
external vector_size : lltype -> int = "llvm_vector_size"
*)

(*--... Operations on other types ..........................................--*)
(*
external void_type : llcontext -> lltype = "llvm_void_type"
external label_type : llcontext -> lltype = "llvm_label_type"
external type_by_name : llmodule -> string -> lltype option = "llvm_type_by_name"
external classify_value : llvalue -> ValueKind.t = "llvm_classify_value"
*)

(*===-- Values ------------------------------------------------------------===*)
val type_of : llvalue -> lltype = fn v => prim("@LLVMTypeOf", v)
(*
external value_name : llvalue -> string = "llvm_value_name"
external set_value_name : string -> llvalue -> unit = "llvm_set_value_name"
external dump_value : llvalue -> unit = "llvm_dump_value"
external replace_all_uses_with : llvalue -> llvalue -> unit
                               = "LLVMReplaceAllUsesWith"
*)

(*--... Operations on uses .................................................--*)
(*
external use_begin : llvalue -> lluse option = "llvm_use_begin"
external use_succ : lluse -> lluse option = "llvm_use_succ"
external user : lluse -> llvalue = "llvm_user"
external used_value : lluse -> llvalue = "llvm_used_value"

let iter_uses f v =
  let rec aux = function
    | None -> ()
    | Some u ->
        f u;
        aux (use_succ u)
  in
  aux (use_begin v)

let fold_left_uses f init v =
  let rec aux init u =
    match u with
    | None -> init
    | Some u -> aux (f init u) (use_succ u)
  in
  aux init (use_begin v)

let fold_right_uses f v init =
  let rec aux u init =
    match u with
    | None -> init
    | Some u -> f u (aux (use_succ u) init)
  in
  aux (use_begin v) init
*)

(*--... Operations on users ................................................--*)
(*
external operand : llvalue -> int -> llvalue = "llvm_operand"
external set_operand : llvalue -> int -> llvalue -> unit = "llvm_set_operand"
external num_operands : llvalue -> int = "llvm_num_operands"
*)

(*--... Operations on constants of (mostly) any type .......................--*)
(*
external is_constant : llvalue -> bool = "llvm_is_constant"
external const_null : lltype -> llvalue = "LLVMConstNull"
external const_all_ones : (*int|vec*)lltype -> llvalue = "LLVMConstAllOnes"
external const_pointer_null : lltype -> llvalue = "LLVMConstPointerNull"
external undef : lltype -> llvalue = "LLVMGetUndef"
external is_null : llvalue -> bool = "llvm_is_null"
external is_undef : llvalue -> bool = "llvm_is_undef"
external constexpr_opcode : llvalue -> Opcode.t = "llvm_constexpr_get_opcode"
*)

(*--... Operations on instructions .........................................--*)
(*
external has_metadata : llvalue -> bool = "llvm_has_metadata"
external metadata : llvalue -> int -> llvalue option = "llvm_metadata"
external set_metadata : llvalue -> int -> llvalue -> unit = "llvm_set_metadata"
external clear_metadata : llvalue -> int -> unit = "llvm_clear_metadata"
*)

(*--... Operations on metadata .......,.....................................--*)
(*
external mdstring : llcontext -> string -> llvalue = "llvm_mdstring"
external mdnode : llcontext -> llvalue array -> llvalue = "llvm_mdnode"
external get_mdstring : llvalue -> string option = "llvm_get_mdstring"
external get_named_metadata : llmodule -> string -> llvalue array = "llvm_get_namedmd"
*)

(*--... Operations on scalar constants .....................................--*)
val const_int : lltype -> int -> llvalue =
 fn t => fn i => prim("@mlkit_llvm_const_int",(t,i))
(*
external const_of_int64 : lltype -> Int64.t -> bool -> llvalue
                        = "llvm_const_of_int64"
external int64_of_const : llvalue -> Int64.t option
                        = "llvm_int64_of_const"
external const_int_of_string : lltype -> string -> int -> llvalue
                             = "llvm_const_int_of_string"
*)
val const_float : lltype -> real -> llvalue =
 fn t => fn d => prim("mlkit_llvm_const_float",(t,d))
(*
external const_float_of_string : lltype -> string -> llvalue
                               = "llvm_const_float_of_string"
*)

(*--... Operations on composite constants ..................................--*)
val const_string : llcontext -> string -> llvalue =
    fn C => fn s => prim("@LLVMConstStringInContext",(C,s,size s,1))
val const_stringz : llcontext -> string -> llvalue =
    fn C => fn s => prim("@LLVMConstStringInContext",(C,s,size s,0))
(*
external const_array : lltype -> llvalue array -> llvalue = "llvm_const_array"
external const_struct : llcontext -> llvalue array -> llvalue
                      = "llvm_const_struct"
external const_named_struct : lltype -> llvalue array -> llvalue
                      = "llvm_const_named_struct"
external const_packed_struct : llcontext -> llvalue array -> llvalue
                             = "llvm_const_packed_struct"
external const_vector : llvalue array -> llvalue = "llvm_const_vector"
*)

(*--... Constant expressions ...............................................--*)
(*
external align_of : lltype -> llvalue = "LLVMAlignOf"
external size_of : lltype -> llvalue = "LLVMSizeOf"
external const_neg : llvalue -> llvalue = "LLVMConstNeg"
external const_nsw_neg : llvalue -> llvalue = "LLVMConstNSWNeg"
external const_nuw_neg : llvalue -> llvalue = "LLVMConstNUWNeg"
external const_fneg : llvalue -> llvalue = "LLVMConstFNeg"
external const_not : llvalue -> llvalue = "LLVMConstNot"
external const_add : llvalue -> llvalue -> llvalue = "LLVMConstAdd"
external const_nsw_add : llvalue -> llvalue -> llvalue = "LLVMConstNSWAdd"
external const_nuw_add : llvalue -> llvalue -> llvalue = "LLVMConstNUWAdd"
external const_fadd : llvalue -> llvalue -> llvalue = "LLVMConstFAdd"
external const_sub : llvalue -> llvalue -> llvalue = "LLVMConstSub"
external const_nsw_sub : llvalue -> llvalue -> llvalue = "LLVMConstNSWSub"
external const_nuw_sub : llvalue -> llvalue -> llvalue = "LLVMConstNUWSub"
external const_fsub : llvalue -> llvalue -> llvalue = "LLVMConstFSub"
external const_mul : llvalue -> llvalue -> llvalue = "LLVMConstMul"
external const_nsw_mul : llvalue -> llvalue -> llvalue = "LLVMConstNSWMul"
external const_nuw_mul : llvalue -> llvalue -> llvalue = "LLVMConstNUWMul"
external const_fmul : llvalue -> llvalue -> llvalue = "LLVMConstFMul"
external const_udiv : llvalue -> llvalue -> llvalue = "LLVMConstUDiv"
external const_sdiv : llvalue -> llvalue -> llvalue = "LLVMConstSDiv"
external const_exact_sdiv : llvalue -> llvalue -> llvalue = "LLVMConstExactSDiv"
external const_fdiv : llvalue -> llvalue -> llvalue = "LLVMConstFDiv"
external const_urem : llvalue -> llvalue -> llvalue = "LLVMConstURem"
external const_srem : llvalue -> llvalue -> llvalue = "LLVMConstSRem"
external const_frem : llvalue -> llvalue -> llvalue = "LLVMConstFRem"
external const_and : llvalue -> llvalue -> llvalue = "LLVMConstAnd"
external const_or : llvalue -> llvalue -> llvalue = "LLVMConstOr"
external const_xor : llvalue -> llvalue -> llvalue = "LLVMConstXor"
external const_icmp : Icmp.t -> llvalue -> llvalue -> llvalue
                    = "llvm_const_icmp"
external const_fcmp : Fcmp.t -> llvalue -> llvalue -> llvalue
                    = "llvm_const_fcmp"
external const_shl : llvalue -> llvalue -> llvalue = "LLVMConstShl"
external const_lshr : llvalue -> llvalue -> llvalue = "LLVMConstLShr"
external const_ashr : llvalue -> llvalue -> llvalue = "LLVMConstAShr"
external const_gep : llvalue -> llvalue array -> llvalue = "llvm_const_gep"
external const_in_bounds_gep : llvalue -> llvalue array -> llvalue
                            = "llvm_const_in_bounds_gep"
external const_trunc : llvalue -> lltype -> llvalue = "LLVMConstTrunc"
external const_sext : llvalue -> lltype -> llvalue = "LLVMConstSExt"
external const_zext : llvalue -> lltype -> llvalue = "LLVMConstZExt"
external const_fptrunc : llvalue -> lltype -> llvalue = "LLVMConstFPTrunc"
external const_fpext : llvalue -> lltype -> llvalue = "LLVMConstFPExt"
external const_uitofp : llvalue -> lltype -> llvalue = "LLVMConstUIToFP"
external const_sitofp : llvalue -> lltype -> llvalue = "LLVMConstSIToFP"
external const_fptoui : llvalue -> lltype -> llvalue = "LLVMConstFPToUI"
external const_fptosi : llvalue -> lltype -> llvalue = "LLVMConstFPToSI"
external const_ptrtoint : llvalue -> lltype -> llvalue = "LLVMConstPtrToInt"
external const_inttoptr : llvalue -> lltype -> llvalue = "LLVMConstIntToPtr"
external const_bitcast : llvalue -> lltype -> llvalue = "LLVMConstBitCast"
external const_zext_or_bitcast : llvalue -> lltype -> llvalue
                             = "LLVMConstZExtOrBitCast"
external const_sext_or_bitcast : llvalue -> lltype -> llvalue
                             = "LLVMConstSExtOrBitCast"
external const_trunc_or_bitcast : llvalue -> lltype -> llvalue
                              = "LLVMConstTruncOrBitCast"
external const_pointercast : llvalue -> lltype -> llvalue
                           = "LLVMConstPointerCast"
external const_intcast : llvalue -> lltype -> llvalue = "LLVMConstIntCast"
external const_fpcast : llvalue -> lltype -> llvalue = "LLVMConstFPCast"
external const_select : llvalue -> llvalue -> llvalue -> llvalue
                      = "LLVMConstSelect"
external const_extractelement : llvalue -> llvalue -> llvalue
                              = "LLVMConstExtractElement"
external const_insertelement : llvalue -> llvalue -> llvalue -> llvalue
                             = "LLVMConstInsertElement"
external const_shufflevector : llvalue -> llvalue -> llvalue -> llvalue
                             = "LLVMConstShuffleVector"
external const_extractvalue : llvalue -> int array -> llvalue
                            = "llvm_const_extractvalue"
external const_insertvalue : llvalue -> llvalue -> int array -> llvalue
                           = "llvm_const_insertvalue"
external const_inline_asm : lltype -> string -> string -> bool -> bool ->
                            llvalue
                          = "llvm_const_inline_asm"
external block_address : llvalue -> llbasicblock -> llvalue = "LLVMBlockAddress"
*)

(*--... Operations on global variables, functions, and aliases (globals) ...--*)
(*
external global_parent : llvalue -> llmodule = "LLVMGetGlobalParent"
external is_declaration : llvalue -> bool = "llvm_is_declaration"
external linkage : llvalue -> Linkage.t = "llvm_linkage"
external set_linkage : Linkage.t -> llvalue -> unit = "llvm_set_linkage"
external section : llvalue -> string = "llvm_section"
external set_section : string -> llvalue -> unit = "llvm_set_section"
external visibility : llvalue -> Visibility.t = "llvm_visibility"
external set_visibility : Visibility.t -> llvalue -> unit = "llvm_set_visibility"
external alignment : llvalue -> int = "llvm_alignment"
external set_alignment : int -> llvalue -> unit = "llvm_set_alignment"
external is_global_constant : llvalue -> bool = "llvm_is_global_constant"
external set_global_constant : bool -> llvalue -> unit
                             = "llvm_set_global_constant"
*)

(*--... Operations on global variables .....................................--*)
(*
external declare_global : lltype -> string -> llmodule -> llvalue
                        = "llvm_declare_global"
external declare_qualified_global : lltype -> string -> int -> llmodule ->
                                    llvalue
                                  = "llvm_declare_qualified_global"
*)
val define_global : string -> llvalue -> llmodule -> llvalue =
    fn n => fn v => fn m => 
                       let val gv : llvalue = prim("@LLVMAddGlobal",(m,type_of v,n))
                           val () = prim("@LLVMSetInitializer",(gv,v))
                       in gv
                       end
(*
external define_qualified_global : string -> llvalue -> int -> llmodule ->
                                   llvalue
                                 = "llvm_define_qualified_global"
external lookup_global : string -> llmodule -> llvalue option
                       = "llvm_lookup_global"
external delete_global : llvalue -> unit = "llvm_delete_global"
external global_initializer : llvalue -> llvalue = "LLVMGetInitializer"
external set_initializer : llvalue -> llvalue -> unit = "llvm_set_initializer"
external remove_initializer : llvalue -> unit = "llvm_remove_initializer"
external is_thread_local : llvalue -> bool = "llvm_is_thread_local"
external set_thread_local : bool -> llvalue -> unit = "llvm_set_thread_local"
external global_begin : llmodule -> (llmodule, llvalue) llpos
                      = "llvm_global_begin"
external global_succ : llvalue -> (llmodule, llvalue) llpos
                     = "llvm_global_succ"
external global_end : llmodule -> (llmodule, llvalue) llrev_pos
                    = "llvm_global_end"
external global_pred : llvalue -> (llmodule, llvalue) llrev_pos
                     = "llvm_global_pred"

let rec iter_global_range f i e =
  if i = e then () else
  match i with
  | At_end _ -> raise (Invalid_argument "Invalid global variable range.")
  | Before bb ->
      f bb;
      iter_global_range f (global_succ bb) e

let iter_globals f m =
  iter_global_range f (global_begin m) (At_end m)

let rec fold_left_global_range f init i e =
  if i = e then init else
  match i with
  | At_end _ -> raise (Invalid_argument "Invalid global variable range.")
  | Before bb -> fold_left_global_range f (f init bb) (global_succ bb) e

let fold_left_globals f init m =
  fold_left_global_range f init (global_begin m) (At_end m)

let rec rev_iter_global_range f i e =
  if i = e then () else
  match i with
  | At_start _ -> raise (Invalid_argument "Invalid global variable range.")
  | After bb ->
      f bb;
      rev_iter_global_range f (global_pred bb) e

let rev_iter_globals f m =
  rev_iter_global_range f (global_end m) (At_start m)

let rec fold_right_global_range f i e init =
  if i = e then init else
  match i with
  | At_start _ -> raise (Invalid_argument "Invalid global variable range.")
  | After bb -> fold_right_global_range f (global_pred bb) e (f bb init)

let fold_right_globals f m init =
  fold_right_global_range f (global_end m) (At_start m) init
*)

(*--... Operations on aliases ..............................................--*)
(*
external add_alias : llmodule -> lltype -> llvalue -> string -> llvalue
                   = "llvm_add_alias"
*)

(*--... Operations on functions ............................................--*)
val declare_function : string -> lltype -> llmodule -> llvalue =
 fn n => fn t => fn m => prim("@mlkit_llvm_declare_function",(n,t,m))
val define_function : string -> lltype -> llmodule -> llvalue =
 fn n => fn t => fn m => prim("@mlkit_llvm_define_function",(n,t,m))
(*
external lookup_function : string -> llmodule -> llvalue option
                         = "llvm_lookup_function"
external delete_function : llvalue -> unit = "llvm_delete_function"
external is_intrinsic : llvalue -> bool = "llvm_is_intrinsic"
external function_call_conv : llvalue -> int = "llvm_function_call_conv"
external set_function_call_conv : int -> llvalue -> unit
                                = "llvm_set_function_call_conv"
external gc : llvalue -> string option = "llvm_gc"
external set_gc : string option -> llvalue -> unit = "llvm_set_gc"
external function_begin : llmodule -> (llmodule, llvalue) llpos
                        = "llvm_function_begin"
external function_succ : llvalue -> (llmodule, llvalue) llpos
                       = "llvm_function_succ"
external function_end : llmodule -> (llmodule, llvalue) llrev_pos
                      = "llvm_function_end"
external function_pred : llvalue -> (llmodule, llvalue) llrev_pos
                       = "llvm_function_pred"

let rec iter_function_range f i e =
  if i = e then () else
  match i with
  | At_end _ -> raise (Invalid_argument "Invalid function range.")
  | Before fn ->
      f fn;
      iter_function_range f (function_succ fn) e

let iter_functions f m =
  iter_function_range f (function_begin m) (At_end m)

let rec fold_left_function_range f init i e =
  if i = e then init else
  match i with
  | At_end _ -> raise (Invalid_argument "Invalid function range.")
  | Before fn -> fold_left_function_range f (f init fn) (function_succ fn) e

let fold_left_functions f init m =
  fold_left_function_range f init (function_begin m) (At_end m)

let rec rev_iter_function_range f i e =
  if i = e then () else
  match i with
  | At_start _ -> raise (Invalid_argument "Invalid function range.")
  | After fn ->
      f fn;
      rev_iter_function_range f (function_pred fn) e

let rev_iter_functions f m =
  rev_iter_function_range f (function_end m) (At_start m)

let rec fold_right_function_range f i e init =
  if i = e then init else
  match i with
  | At_start _ -> raise (Invalid_argument "Invalid function range.")
  | After fn -> fold_right_function_range f (function_pred fn) e (f fn init)

let fold_right_functions f m init =
  fold_right_function_range f (function_end m) (At_start m) init

external llvm_add_function_attr : llvalue -> int32 -> unit
                                = "llvm_add_function_attr"
external llvm_remove_function_attr : llvalue -> int32 -> unit
                                   = "llvm_remove_function_attr"
external llvm_function_attr : llvalue -> int32 = "llvm_function_attr"

let pack_attr (attr:Attribute.t) : int32 =
  match attr with
  Attribute.Zext                  -> Int32.shift_left 1l 0
    | Attribute.Sext              -> Int32.shift_left 1l 1
    | Attribute.Noreturn          -> Int32.shift_left 1l 2
    | Attribute.Inreg             -> Int32.shift_left 1l 3
    | Attribute.Structret         -> Int32.shift_left 1l 4
    | Attribute.Nounwind          -> Int32.shift_left 1l 5
    | Attribute.Noalias           -> Int32.shift_left 1l 6
    | Attribute.Byval             -> Int32.shift_left 1l 7
    | Attribute.Nest              -> Int32.shift_left 1l 8
    | Attribute.Readnone          -> Int32.shift_left 1l 9
    | Attribute.Readonly          -> Int32.shift_left 1l 10
    | Attribute.Noinline          -> Int32.shift_left 1l 11
    | Attribute.Alwaysinline      -> Int32.shift_left 1l 12
    | Attribute.Optsize           -> Int32.shift_left 1l 13
    | Attribute.Ssp               -> Int32.shift_left 1l 14
    | Attribute.Sspreq            -> Int32.shift_left 1l 15
    | Attribute.Alignment n       -> Int32.shift_left (Int32.of_int n) 16
    | Attribute.Nocapture         -> Int32.shift_left 1l 21
    | Attribute.Noredzone         -> Int32.shift_left 1l 22
    | Attribute.Noimplicitfloat   -> Int32.shift_left 1l 23
    | Attribute.Naked             -> Int32.shift_left 1l 24
    | Attribute.Inlinehint        -> Int32.shift_left 1l 25
    | Attribute.Stackalignment n  -> Int32.shift_left (Int32.of_int n) 26
    | Attribute.ReturnsTwice      -> Int32.shift_left 1l 29
    | Attribute.UWTable           -> Int32.shift_left 1l 30
    | Attribute.NonLazyBind       -> Int32.shift_left 1l 31

let unpack_attr (a : int32) : Attribute.t list =
  let l = ref [] in
  let check attr =
      Int32.logand (pack_attr attr) a in
  let checkattr attr =
      if (check attr) <> 0l then begin
          l := attr :: !l
      end
  in
  checkattr Attribute.Zext;
  checkattr Attribute.Sext;
  checkattr Attribute.Noreturn;
  checkattr Attribute.Inreg;
  checkattr Attribute.Structret;
  checkattr Attribute.Nounwind;
  checkattr Attribute.Noalias;
  checkattr Attribute.Byval;
  checkattr Attribute.Nest;
  checkattr Attribute.Readnone;
  checkattr Attribute.Readonly;
  checkattr Attribute.Noinline;
  checkattr Attribute.Alwaysinline;
  checkattr Attribute.Optsize;
  checkattr Attribute.Ssp;
  checkattr Attribute.Sspreq;
  let align = Int32.logand (Int32.shift_right_logical a 16) 31l in
  if align <> 0l then
      l := Attribute.Alignment (Int32.to_int align) :: !l;
  checkattr Attribute.Nocapture;
  checkattr Attribute.Noredzone;
  checkattr Attribute.Noimplicitfloat;
  checkattr Attribute.Naked;
  checkattr Attribute.Inlinehint;
  let stackalign = Int32.logand (Int32.shift_right_logical a 26) 7l in
  if stackalign <> 0l then
      l := Attribute.Stackalignment (Int32.to_int stackalign) :: !l;
  checkattr Attribute.ReturnsTwice;
  checkattr Attribute.UWTable;
  checkattr Attribute.NonLazyBind;
  !l;;

let add_function_attr llval attr =
  llvm_add_function_attr llval (pack_attr attr)

let remove_function_attr llval attr =
  llvm_remove_function_attr llval (pack_attr attr)

let function_attr f = unpack_attr (llvm_function_attr f)
*)

(*--... Operations on params ...............................................--*)
(*
external params : llvalue -> llvalue array = "llvm_params"
*)
val param : llvalue -> int -> llvalue = fn v => fn i => prim("@LLVMGetParam",(v,i))
(*
external llvm_param_attr : llvalue -> int32 = "llvm_param_attr"
let param_attr p = unpack_attr (llvm_param_attr p)
external param_parent : llvalue -> llvalue = "LLVMGetParamParent"
external param_begin : llvalue -> (llvalue, llvalue) llpos = "llvm_param_begin"
external param_succ : llvalue -> (llvalue, llvalue) llpos = "llvm_param_succ"
external param_end : llvalue -> (llvalue, llvalue) llrev_pos = "llvm_param_end"
external param_pred : llvalue -> (llvalue, llvalue) llrev_pos ="llvm_param_pred"

let rec iter_param_range f i e =
  if i = e then () else
  match i with
  | At_end _ -> raise (Invalid_argument "Invalid parameter range.")
  | Before p ->
      f p;
      iter_param_range f (param_succ p) e

let iter_params f fn =
  iter_param_range f (param_begin fn) (At_end fn)

let rec fold_left_param_range f init i e =
  if i = e then init else
  match i with
  | At_end _ -> raise (Invalid_argument "Invalid parameter range.")
  | Before p -> fold_left_param_range f (f init p) (param_succ p) e

let fold_left_params f init fn =
  fold_left_param_range f init (param_begin fn) (At_end fn)

let rec rev_iter_param_range f i e =
  if i = e then () else
  match i with
  | At_start _ -> raise (Invalid_argument "Invalid parameter range.")
  | After p ->
      f p;
      rev_iter_param_range f (param_pred p) e

let rev_iter_params f fn =
  rev_iter_param_range f (param_end fn) (At_start fn)

let rec fold_right_param_range f init i e =
  if i = e then init else
  match i with
  | At_start _ -> raise (Invalid_argument "Invalid parameter range.")
  | After p -> fold_right_param_range f (f p init) (param_pred p) e

let fold_right_params f fn init =
  fold_right_param_range f init (param_end fn) (At_start fn)

external llvm_add_param_attr : llvalue -> int32 -> unit
                                = "llvm_add_param_attr"
external llvm_remove_param_attr : llvalue -> int32 -> unit
                                = "llvm_remove_param_attr"

let add_param_attr llval attr =
  llvm_add_param_attr llval (pack_attr attr)

let remove_param_attr llval attr =
  llvm_remove_param_attr llval (pack_attr attr)

external set_param_alignment : llvalue -> int -> unit
                             = "llvm_set_param_alignment"
*)
(*--... Operations on basic blocks .........................................--*)
(*
external value_of_block : llbasicblock -> llvalue = "LLVMBasicBlockAsValue"
external value_is_block : llvalue -> bool = "llvm_value_is_block"
external block_of_value : llvalue -> llbasicblock = "LLVMValueAsBasicBlock"
external block_parent : llbasicblock -> llvalue = "LLVMGetBasicBlockParent"
external basic_blocks : llvalue -> llbasicblock array = "llvm_basic_blocks"
*)
val entry_block : llvalue -> llbasicblock = fn v => prim("@LLVMGetEntryBasicBlock",v)
val delete_block : llbasicblock -> unit = fn b => prim("@LLVMDeleteBasicBlock", b)
val append_block : llcontext -> string -> llvalue -> llbasicblock = fn C => fn n => fn v => prim("@LLVMAppendBasicBlockInContext", (C,v,n))
(*
external insert_block : llcontext -> string -> llbasicblock -> llbasicblock
                      = "llvm_insert_block"
external block_begin : llvalue -> (llvalue, llbasicblock) llpos
                     = "llvm_block_begin"
external block_succ : llbasicblock -> (llvalue, llbasicblock) llpos
                    = "llvm_block_succ"
external block_end : llvalue -> (llvalue, llbasicblock) llrev_pos
                   = "llvm_block_end"
external block_pred : llbasicblock -> (llvalue, llbasicblock) llrev_pos
                    = "llvm_block_pred"
external block_terminator : llbasicblock -> llvalue option =
    "llvm_block_terminator"

let rec iter_block_range f i e =
  if i = e then () else
  match i with
  | At_end _ -> raise (Invalid_argument "Invalid block range.")
  | Before bb ->
      f bb;
      iter_block_range f (block_succ bb) e

let iter_blocks f fn =
  iter_block_range f (block_begin fn) (At_end fn)

let rec fold_left_block_range f init i e =
  if i = e then init else
  match i with
  | At_end _ -> raise (Invalid_argument "Invalid block range.")
  | Before bb -> fold_left_block_range f (f init bb) (block_succ bb) e

let fold_left_blocks f init fn =
  fold_left_block_range f init (block_begin fn) (At_end fn)

let rec rev_iter_block_range f i e =
  if i = e then () else
  match i with
  | At_start _ -> raise (Invalid_argument "Invalid block range.")
  | After bb ->
      f bb;
      rev_iter_block_range f (block_pred bb) e

let rev_iter_blocks f fn =
  rev_iter_block_range f (block_end fn) (At_start fn)

let rec fold_right_block_range f init i e =
  if i = e then init else
  match i with
  | At_start _ -> raise (Invalid_argument "Invalid block range.")
  | After bb -> fold_right_block_range f (f bb init) (block_pred bb) e

let fold_right_blocks f fn init =
  fold_right_block_range f init (block_end fn) (At_start fn)
*)

(*--... Operations on instructions .........................................--*)
(*
external instr_parent : llvalue -> llbasicblock = "LLVMGetInstructionParent"
external instr_begin : llbasicblock -> (llbasicblock, llvalue) llpos
                     = "llvm_instr_begin"
external instr_succ : llvalue -> (llbasicblock, llvalue) llpos
                     = "llvm_instr_succ"
external instr_end : llbasicblock -> (llbasicblock, llvalue) llrev_pos
                     = "llvm_instr_end"
external instr_pred : llvalue -> (llbasicblock, llvalue) llrev_pos
                     = "llvm_instr_pred"

external instr_opcode : llvalue -> Opcode.t = "llvm_instr_get_opcode"
external icmp_predicate : llvalue -> Icmp.t option = "llvm_instr_icmp_predicate"

external icmp_predicate : llvalue -> Icmp.t option = "llvm_instr_icmp_predicate"

let rec iter_instrs_range f i e =
  if i = e then () else
  match i with
  | At_end _ -> raise (Invalid_argument "Invalid instruction range.")
  | Before i ->
      f i;
      iter_instrs_range f (instr_succ i) e

let iter_instrs f bb =
  iter_instrs_range f (instr_begin bb) (At_end bb)

let rec fold_left_instrs_range f init i e =
  if i = e then init else
  match i with
  | At_end _ -> raise (Invalid_argument "Invalid instruction range.")
  | Before i -> fold_left_instrs_range f (f init i) (instr_succ i) e

let fold_left_instrs f init bb =
  fold_left_instrs_range f init (instr_begin bb) (At_end bb)

let rec rev_iter_instrs_range f i e =
  if i = e then () else
  match i with
  | At_start _ -> raise (Invalid_argument "Invalid instruction range.")
  | After i ->
      f i;
      rev_iter_instrs_range f (instr_pred i) e

let rev_iter_instrs f bb =
  rev_iter_instrs_range f (instr_end bb) (At_start bb)

let rec fold_right_instr_range f i e init =
  if i = e then init else
  match i with
  | At_start _ -> raise (Invalid_argument "Invalid instruction range.")
  | After i -> fold_right_instr_range f (instr_pred i) e (f i init)

let fold_right_instrs f bb init =
  fold_right_instr_range f (instr_end bb) (At_start bb) init
*)

(*--... Operations on call sites ...........................................--*)
(*
external instruction_call_conv: llvalue -> int
                              = "llvm_instruction_call_conv"
external set_instruction_call_conv: int -> llvalue -> unit
                                  = "llvm_set_instruction_call_conv"

external llvm_add_instruction_param_attr : llvalue -> int -> int32 -> unit
                                         = "llvm_add_instruction_param_attr"
external llvm_remove_instruction_param_attr : llvalue -> int -> int32 -> unit
                                         = "llvm_remove_instruction_param_attr"

let add_instruction_param_attr llval i attr =
  llvm_add_instruction_param_attr llval i (pack_attr attr)

let remove_instruction_param_attr llval i attr =
  llvm_remove_instruction_param_attr llval i (pack_attr attr)
*)

(*--... Operations on call instructions (only) .............................--*)
(*
external is_tail_call : llvalue -> bool = "llvm_is_tail_call"
external set_tail_call : bool -> llvalue -> unit = "llvm_set_tail_call"
*)

(*--... Operations on phi nodes ............................................--*)
(*
external add_incoming : (llvalue * llbasicblock) -> llvalue -> unit
                      = "llvm_add_incoming"
external incoming : llvalue -> (llvalue * llbasicblock) list = "llvm_incoming"

external delete_instruction : llvalue -> unit = "llvm_delete_instruction"
*)

(*===-- Instruction builders ----------------------------------------------===*)
val create_builder : llcontext -> llbuilder = fn C => prim("@LLVMCreateBuilderInContext",C)
(*
external builder : llcontext -> llbuilder = "llvm_builder"
external position_builder : (llbasicblock, llvalue) llpos -> llbuilder -> unit
                          = "llvm_position_builder"
external insertion_block : llbuilder -> llbasicblock = "llvm_insertion_block"
external insert_into_builder : llvalue -> string -> llbuilder -> unit
                             = "llvm_insert_into_builder"
*)
val position_builder_end : llbuilder -> llbasicblock -> unit = fn b => fn bb => prim("@LLVMPositionBuilderAtEnd", (b,bb))

fun builder_at_end context bb =
    let val b = create_builder context
    in position_builder_end b bb;
       b
    end

val dispose_builder : llbuilder -> unit = fn b => prim("@LLVMDisposeBuilder", b)

(*
let builder_at context ip =
  let b = builder context in
  position_builder ip b;
  b

let builder_before context i = builder_at context (Before i)
let builder_at_end context bb = builder_at context (At_end bb)

let position_before i = position_builder (Before i)
let position_at_end bb = position_builder (At_end bb)
*)

(*--... Metadata ...........................................................--*)
(*
external set_current_debug_location : llbuilder -> llvalue -> unit
                                    = "llvm_set_current_debug_location"
external clear_current_debug_location : llbuilder -> unit
                                      = "llvm_clear_current_debug_location"
external current_debug_location : llbuilder -> llvalue option
                                    = "llvm_current_debug_location"
external set_inst_debug_location : llbuilder -> llvalue -> unit
                                 = "llvm_set_inst_debug_location"
*)

(*--... Terminators ........................................................--*)
val build_ret_void : llbuilder -> llvalue = fn b => prim("@LLVMBuildRetVoid",b)
val build_ret : llvalue -> llbuilder -> llvalue = fn v => fn b => prim("@LLVMBuildRet", (b,v))
(*
external build_aggregate_ret : llvalue array -> llbuilder -> llvalue
                             = "llvm_build_aggregate_ret"
*)
val build_br : llbasicblock -> llbuilder -> llvalue = fn bb => fn b => prim("@LLVMBuildBr", (b,bb))
val build_cond_br : llvalue -> llbasicblock -> llbasicblock -> llbuilder -> llvalue =
    fn v => fn bb1 => fn bb2 => fn b => prim("@LLVMBuildCondBr", (b,v,bb1,bb2))
(*
external build_switch : llvalue -> llbasicblock -> int -> llbuilder -> llvalue
                      = "llvm_build_switch"
external build_malloc : lltype -> string -> llbuilder -> llvalue =
    "llvm_build_malloc"
external build_array_malloc : lltype -> llvalue -> string -> llbuilder ->
    llvalue = "llvm_build_array_malloc"
external build_free : llvalue -> llbuilder -> llvalue = "llvm_build_free"
external add_case : llvalue -> llvalue -> llbasicblock -> unit
                  = "llvm_add_case"
external switch_default_dest : llvalue -> llbasicblock =
    "LLVMGetSwitchDefaultDest"
external build_indirect_br : llvalue -> int -> llbuilder -> llvalue
                           = "llvm_build_indirect_br"
external add_destination : llvalue -> llbasicblock -> unit
                         = "llvm_add_destination"
external build_invoke : llvalue -> llvalue array -> llbasicblock ->
                        llbasicblock -> string -> llbuilder -> llvalue
                      = "llvm_build_invoke_bc" "llvm_build_invoke_nat"
external build_landingpad : lltype -> llvalue -> int -> string -> llbuilder ->
                            llvalue = "llvm_build_landingpad"
external set_cleanup : llvalue -> bool -> unit = "llvm_set_cleanup"
external add_clause : llvalue -> llvalue -> unit = "llvm_add_clause"
external build_resume : llvalue -> llbuilder -> llvalue = "llvm_build_resume"
external build_unreachable : llbuilder -> llvalue = "llvm_build_unreachable"
*)

(*--... Arithmetic .........................................................--*)
val build_add : llvalue -> llvalue -> string -> llbuilder -> llvalue =
    fn v1 => fn v2 => fn n => fn b => prim("@LLVMBuildAdd", (b,v1,v2,n))
(*
external build_nsw_add : llvalue -> llvalue -> string -> llbuilder -> llvalue
                       = "llvm_build_nsw_add"
external build_nuw_add : llvalue -> llvalue -> string -> llbuilder -> llvalue
                       = "llvm_build_nuw_add"
*)
val build_fadd : llvalue -> llvalue -> string -> llbuilder -> llvalue =
 fn v1 => fn v2 => fn n => fn b => prim("@LLVMBuildFAdd", (b,v1,v2,n))
val build_sub : llvalue -> llvalue -> string -> llbuilder -> llvalue =
 fn v1 => fn v2 => fn n => fn b => prim("@LLVMBuildSub", (b,v1,v2,n))
(*
external build_nsw_sub : llvalue -> llvalue -> string -> llbuilder -> llvalue
                       = "llvm_build_nsw_sub"
external build_nuw_sub : llvalue -> llvalue -> string -> llbuilder -> llvalue
                       = "llvm_build_nuw_sub"
*)
val build_fsub : llvalue -> llvalue -> string -> llbuilder -> llvalue =
 fn v1 => fn v2 => fn n => fn b => prim("@LLVMBuildFSub", (b,v1,v2,n))
val build_mul : llvalue -> llvalue -> string -> llbuilder -> llvalue =
 fn v1 => fn v2 => fn n => fn b => prim("@LLVMBuildMul", (b,v1,v2,n))
(*
external build_nsw_mul : llvalue -> llvalue -> string -> llbuilder -> llvalue
                       = "llvm_build_nsw_mul"
external build_nuw_mul : llvalue -> llvalue -> string -> llbuilder -> llvalue
                       = "llvm_build_nuw_mul"
*)
val build_fmul : llvalue -> llvalue -> string -> llbuilder -> llvalue =
 fn v1 => fn v2 => fn n => fn b => prim("@LLVMBuildFMul", (b,v1,v2,n))
val build_udiv : llvalue -> llvalue -> string -> llbuilder -> llvalue =
 fn v1 => fn v2 => fn n => fn b => prim("@LLVMBuildUDiv", (b,v1,v2,n))
val build_sdiv : llvalue -> llvalue -> string -> llbuilder -> llvalue =
 fn v1 => fn v2 => fn n => fn b => prim("@LLVMBuildSDiv", (b,v1,v2,n))
(*
external build_exact_sdiv : llvalue -> llvalue -> string -> llbuilder -> llvalue
                          = "llvm_build_exact_sdiv"
*)
val build_fdiv : llvalue -> llvalue -> string -> llbuilder -> llvalue =
 fn v1 => fn v2 => fn n => fn b => prim("@LLVMBuildFDiv", (b,v1,v2,n))
val build_urem : llvalue -> llvalue -> string -> llbuilder -> llvalue =
 fn v1 => fn v2 => fn n => fn b => prim("@LLVMBuildURem", (b,v1,v2,n))
val build_srem : llvalue -> llvalue -> string -> llbuilder -> llvalue =
 fn v1 => fn v2 => fn n => fn b => prim("@LLVMBuildSRem", (b,v1,v2,n))
val build_frem : llvalue -> llvalue -> string -> llbuilder -> llvalue =
 fn v1 => fn v2 => fn n => fn b => prim("@LLVMBuildFRem", (b,v1,v2,n))
val build_shl : llvalue -> llvalue -> string -> llbuilder -> llvalue =
 fn v1 => fn v2 => fn n => fn b => prim("@LLVMBuildShl", (b,v1,v2,n))
val build_lshr : llvalue -> llvalue -> string -> llbuilder -> llvalue =
 fn v1 => fn v2 => fn n => fn b => prim("@LLVMBuildLShr", (b,v1,v2,n))
val build_ashr : llvalue -> llvalue -> string -> llbuilder -> llvalue =
 fn v1 => fn v2 => fn n => fn b => prim("@LLVMBuildAShr", (b,v1,v2,n))
val build_and : llvalue -> llvalue -> string -> llbuilder -> llvalue =
 fn v1 => fn v2 => fn n => fn b => prim("@LLVMBuildAnd", (b,v1,v2,n))
val build_or : llvalue -> llvalue -> string -> llbuilder -> llvalue =
 fn v1 => fn v2 => fn n => fn b => prim("@LLVMBuildOr", (b,v1,v2,n))
val build_xor : llvalue -> llvalue -> string -> llbuilder -> llvalue =
 fn v1 => fn v2 => fn n => fn b => prim("@LLVMBuildXor", (b,v1,v2,n))
val build_neg : llvalue -> string -> llbuilder -> llvalue =
 fn v => fn n => fn b => prim("@LLVMBuildNeg", (b,v,n))
(*
external build_nsw_neg : llvalue -> string -> llbuilder -> llvalue
                       = "llvm_build_nsw_neg"
external build_nuw_neg : llvalue -> string -> llbuilder -> llvalue
                       = "llvm_build_nuw_neg"
*)
val build_fneg : llvalue -> string -> llbuilder -> llvalue =
 fn v => fn n => fn b => prim("@LLVMBuildFNeg", (b,v,n))

val build_not : llvalue -> string -> llbuilder -> llvalue =
 fn v => fn n => fn b => prim("@LLVMBuildNot", (b,v,n))

(*--... Memory .............................................................--*)
(*
external build_alloca : lltype -> string -> llbuilder -> llvalue
                      = "llvm_build_alloca"
external build_array_alloca : lltype -> llvalue -> string -> llbuilder ->
                              llvalue = "llvm_build_array_alloca"
external build_load : llvalue -> string -> llbuilder -> llvalue
                    = "llvm_build_load"
external build_store : llvalue -> llvalue -> llbuilder -> llvalue
                     = "llvm_build_store"
*)
val build_gep : llvalue -> llvalue list -> string -> llbuilder -> llvalue =
    fn v => fn vs => fn n => fn b => prim("mlkit_llvm_build_gep", (v,vs,n,b))
(*
external build_in_bounds_gep : llvalue -> llvalue array -> string ->
                             llbuilder -> llvalue = "llvm_build_in_bounds_gep"
external build_struct_gep : llvalue -> int -> string -> llbuilder -> llvalue
                         = "llvm_build_struct_gep"

external build_global_string : string -> string -> llbuilder -> llvalue
                             = "llvm_build_global_string"
external build_global_stringptr  : string -> string -> llbuilder -> llvalue
                                 = "llvm_build_global_stringptr"
*)

(*--... Casts ..............................................................--*)
val build_trunc : llvalue -> lltype -> string -> llbuilder -> llvalue =
     fn v => fn t => fn n => fn b => prim("@LLVMBuildTrunc", (b,v,t,n))
val build_zext : llvalue -> lltype -> string -> llbuilder -> llvalue =
     fn v => fn t => fn n => fn b => prim("@LLVMBuildZExt", (b,v,t,n))
val build_sext : llvalue -> lltype -> string -> llbuilder -> llvalue =
     fn v => fn t => fn n => fn b => prim("@LLVMBuildSExt", (b,v,t,n))
val build_fptoui : llvalue -> lltype -> string -> llbuilder -> llvalue =
     fn v => fn t => fn n => fn b => prim("@LLVMBuildFPToUI", (b,v,t,n))
val build_fptosi : llvalue -> lltype -> string -> llbuilder -> llvalue =
     fn v => fn t => fn n => fn b => prim("@LLVMBuildFPToSI", (b,v,t,n))
val build_uitofp : llvalue -> lltype -> string -> llbuilder -> llvalue =
     fn v => fn t => fn n => fn b => prim("@LLVMBuildUIToFP", (b,v,t,n))
val build_sitofp : llvalue -> lltype -> string -> llbuilder -> llvalue =
     fn v => fn t => fn n => fn b => prim("@LLVMBuildSIToFP", (b,v,t,n))
val build_fptrunc : llvalue -> lltype -> string -> llbuilder -> llvalue =
     fn v => fn t => fn n => fn b => prim("@LLVMBuildFPTrunc", (b,v,t,n))
val build_fpext : llvalue -> lltype -> string -> llbuilder -> llvalue =
     fn v => fn t => fn n => fn b => prim("@LLVMBuildFPExt", (b,v,t,n))
(*
external build_ptrtoint : llvalue -> lltype -> string -> llbuilder -> llvalue
                        = "llvm_build_prttoint"
external build_inttoptr : llvalue -> lltype -> string -> llbuilder -> llvalue
                        = "llvm_build_inttoptr"
external build_bitcast : llvalue -> lltype -> string -> llbuilder -> llvalue
                       = "llvm_build_bitcast"
external build_zext_or_bitcast : llvalue -> lltype -> string -> llbuilder ->
                                 llvalue = "llvm_build_zext_or_bitcast"
external build_sext_or_bitcast : llvalue -> lltype -> string -> llbuilder ->
                                 llvalue = "llvm_build_sext_or_bitcast"
external build_trunc_or_bitcast : llvalue -> lltype -> string -> llbuilder ->
                                  llvalue = "llvm_build_trunc_or_bitcast"
external build_pointercast : llvalue -> lltype -> string -> llbuilder -> llvalue
                           = "llvm_build_pointercast"
external build_intcast : llvalue -> lltype -> string -> llbuilder -> llvalue
                       = "llvm_build_intcast"
external build_fpcast : llvalue -> lltype -> string -> llbuilder -> llvalue
                      = "llvm_build_fpcast"
*)

(*--... Comparisons ........................................................--*)
fun mem (f:unit -> 'a) : unit -> 'a  =
    let val r : 'a option ref = ref NONE
    in fn () => case !r of SOME v => v
                         | NONE => let val v = f() in r := SOME v; v end
    end

val LLVMIntEQ : unit -> int =
    mem (fn () => prim("@mlkit_llvm_IntEQ",()))

val build_icmp : Icmp.t -> llvalue -> llvalue -> string -> llbuilder -> llvalue =
    fn c => fn v1 => fn v2 => fn n => fn b => prim("@LLVMBuildICmp", (b,Icmp.index c + LLVMIntEQ(),v1,v2,n))

val build_fcmp : Fcmp.t -> llvalue -> llvalue -> string -> llbuilder -> llvalue =
    fn c => fn v1 => fn v2 => fn n => fn b => prim("@LLVMBuildFCmp", (b,Fcmp.index c,v1,v2,n))

(*--... Miscellaneous instructions .........................................--*)
val build_phi_node : lltype -> string -> llbuilder -> llvalue =
 fn t => fn n => fn b => prim("@LLVMBuildPhi",(b,t,n))

val mlkit_add_incoming : llvalue -> llvalue -> llbasicblock -> unit =
    fn node => fn v => fn bb => prim("@mlkit_add_incoming",(node,v,bb))

val build_phi : (llvalue * llbasicblock) list -> string -> llbuilder -> llvalue =
    fn nodes => fn n => fn b =>
                           case nodes of
                             nil => die "build_phi: empty list"
                           | (v,_) :: _ =>
                             let val node = build_phi_node (type_of v) n b
                             in List.app (fn (v,bb) => mlkit_add_incoming node v bb) nodes
                              ; node
                             end

val build_call : llvalue -> llvalue list -> string -> llbuilder -> llvalue =
    fn v => fn vs => fn n => fn b => prim("mlkit_llvm_build_call", (v,vs,n,b))

(*
external build_select : llvalue -> llvalue -> llvalue -> string -> llbuilder ->
                        llvalue = "llvm_build_select"
external build_va_arg : llvalue -> lltype -> string -> llbuilder -> llvalue
                      = "llvm_build_va_arg"
external build_extractelement : llvalue -> llvalue -> string -> llbuilder ->
                                llvalue = "llvm_build_extractelement"
external build_insertelement : llvalue -> llvalue -> llvalue -> string ->
                               llbuilder -> llvalue = "llvm_build_insertelement"
external build_shufflevector : llvalue -> llvalue -> llvalue -> string ->
                               llbuilder -> llvalue = "llvm_build_shufflevector"
external build_extractvalue : llvalue -> int -> string -> llbuilder -> llvalue
                            = "llvm_build_extractvalue"
external build_insertvalue : llvalue -> llvalue -> int -> string -> llbuilder ->
                             llvalue = "llvm_build_insertvalue"

external build_is_null : llvalue -> string -> llbuilder -> llvalue
                       = "llvm_build_is_null"
external build_is_not_null : llvalue -> string -> llbuilder -> llvalue
                           = "llvm_build_is_not_null"
external build_ptrdiff : llvalue -> llvalue -> string -> llbuilder -> llvalue
                       = "llvm_build_ptrdiff"
*)

(*===-- Memory buffers ----------------------------------------------------===*)
(*
module MemoryBuffer = struct
  external of_file : string -> llmemorybuffer = "llvm_memorybuffer_of_file"
  external of_stdin : unit -> llmemorybuffer = "llvm_memorybuffer_of_stdin"
  external dispose : llmemorybuffer -> unit = "llvm_memorybuffer_dispose"
end
*)

(*===-- Pass Manager ------------------------------------------------------===*)
(*
module PassManager = struct
  type 'a t
  type any = [ `Module | `Function ]
  external create : unit -> [ `Module ] t = "llvm_passmanager_create"
  external create_function : llmodule -> [ `Function ] t
                           = "LLVMCreateFunctionPassManager"
  external run_module : llmodule -> [ `Module ] t -> bool
                      = "llvm_passmanager_run_module"
  external initialize : [ `Function ] t -> bool = "llvm_passmanager_initialize"
  external run_function : llvalue -> [ `Function ] t -> bool
                        = "llvm_passmanager_run_function"
  external finalize : [ `Function ] t -> bool = "llvm_passmanager_finalize"
  external dispose : [< any ] t -> unit = "llvm_passmanager_dispose"
end
*)

(*===-- Non-Externs -------------------------------------------------------===*)
(* These functions are built using the externals, so must be declared late.   *)
(*
let concat2 sep arr =
  let s = ref "" in
  if 0 < Array.length arr then begin
    s := !s ^ arr.(0);
    for i = 1 to (Array.length arr) - 1 do
      s := !s ^ sep ^ arr.(i)
    done
  end;
  !s
*)
fun string_of_lltype ty =
  (* FIXME: stop infinite recursion! :) *)
  case classify_type ty of
    TypeKind.Integer => "i" ^ Int.toString (integer_bitwidth ty)
  | TypeKind.Pointer =>
      let val ety = element_type ty 
      in (*case classify_type ety of
           TypeKind.Struct =>
           (case struct_name ety of
              None => (string_of_lltype ety)
            | Some s => s) ^ "*"
         | _ =>*) (string_of_lltype (element_type ty)) ^ "*"
      end
(*
  | TypeKind.Struct =>
      let val s = "{ " ^ (concat2 ", " (
                Array.map string_of_lltype (struct_element_types ty)
              )) ^ " }"
      in if is_packed ty then "<" ^ s ^ ">" else s
      end
  | TypeKind.Array => "["   ^ (string_of_int (array_length ty)) ^
                      " x " ^ (string_of_lltype (element_type ty)) ^ "]"
  | TypeKind.Vector => "<"   ^ (string_of_int (vector_size ty)) ^
                       " x " ^ (string_of_lltype (element_type ty)) ^ ">"
  | TypeKind.Function => string_of_lltype (return_type ty) ^
                         " (" ^ (concat2 ", " (
                           Array.map string_of_lltype (param_types ty)
                         )) ^ ")"
*)
  | TypeKind.Label => "label"
  | TypeKind.Ppc_fp128 => "ppc_fp128"
  | TypeKind.Fp128 => "fp128"
  | TypeKind.X86fp80 => "x86_fp80"
  | TypeKind.Double => "double"
  | TypeKind.Float => "float"
  | TypeKind.Half => "half"
  | TypeKind.Void => "void"
  | TypeKind.Metadata => "metadata"
  | x => die ("string_of_lltype.unsupported type: " ^ TypeKind.toString x)

val write_bitcode_file : llmodule -> string -> bool = fn m => fn f => prim("@mlkit_llvm_write_bitcode_file", (m,f))

structure GenericValue = struct
  type t = foreignptr
  
  val of_float: lltype -> real -> t =
   fn t => fn r => prim("mlkit_llvm_genericvalue_of_float", (t,r))
(*
  external of_pointer: 'a -> t
    = "llvm_genericvalue_of_pointer"
  external of_int32: Llvm.lltype -> int32 -> t
    = "llvm_genericvalue_of_int32"
*)
  val of_int: lltype -> int -> t =
   fn t => fn i => prim("@mlkit_llvm_genericvalue_of_int", (t,i))
(*
  external of_nativeint: Llvm.lltype -> nativeint -> t
    = "llvm_genericvalue_of_nativeint"
  external of_int64: Llvm.lltype -> int64 -> t
    = "llvm_genericvalue_of_int64"
*)  
  val as_float: lltype -> t -> real =
   fn t => fn g => prim("mlkit_llvm_genericvalue_as_float",(t,g))
(*
  external as_pointer: t -> 'a
    = "llvm_genericvalue_as_pointer"
  external as_int32: t -> int32
    = "llvm_genericvalue_as_int32"
*)
  val as_int: t -> int =
   fn t => prim("@mlkit_llvm_genericvalue_as_int", t)
(*
  external as_nativeint: t -> nativeint
    = "llvm_genericvalue_as_nativeint"
  external as_int64: t -> int64
    = "llvm_genericvalue_as_int64"
*)
end


structure ExecutionEngine = struct
  type t = foreignptr
   
  val create: llmodule -> t =
   fn m => prim("@mlkit_llvm_ee_create", m)

  val create_interpreter: llmodule -> t =
   fn m => prim("@mlkit_llvm_ee_create_interpreter",m)

  val create_jit: llmodule -> int -> t =
    fn m => fn i => prim("@mlkit_llvm_ee_create_jit",(m,i))

  val dispose: t -> unit =
   fn ee => prim("@LLVMDisposeExecutionEngine", ee)

  val add_module: llmodule -> t -> unit =
   fn m => fn t => prim("@LLVMAddModule", (t,m))
(*
  val remove_module: llmodule -> t -> llmodule =
   fn m => fn t => prim("@mlkit_llvm_ee_remove_module")
*)

  val find_function: string -> t -> llvalue option =
      fn s => fn ee => 
                 SOME(prim("@mlkit_llvm_ee_find_function", (s,ee)))
                 handle _ => NONE

  val run_function: llvalue -> GenericValue.t list -> t -> GenericValue.t =
   fn f => fn args => fn ee => prim("mlkit_llvm_ee_run_function", (f, args, ee))
(*
  external run_static_ctors: t -> unit
    = "llvm_ee_run_static_ctors"
  external run_static_dtors: t -> unit
    = "llvm_ee_run_static_dtors"
  external run_function_as_main: Llvm.llvalue -> string array ->
                                 (string * string) array -> t -> int
    = "llvm_ee_run_function_as_main"
  external free_machine_code: Llvm.llvalue -> t -> unit
    = "llvm_ee_free_machine_code"

  external target_data: t -> Llvm_target.TargetData.t
    = "LLVMGetExecutionEngineTargetData"
*)  

  (* The following are not bound. Patches are welcome.
  
  get_target_data: t -> lltargetdata
  add_global_mapping: llvalue -> llgenericvalue -> t -> unit
  clear_all_global_mappings: t -> unit
  update_global_mapping: llvalue -> llgenericvalue -> t -> unit
  get_pointer_to_global_if_available: llvalue -> t -> llgenericvalue
  get_pointer_to_global: llvalue -> t -> llgenericvalue
  get_pointer_to_function: llvalue -> t -> llgenericvalue
  get_pointer_to_function_or_stub: llvalue -> t -> llgenericvalue
  get_global_value_at_address: llgenericvalue -> t -> llvalue option
  store_value_to_memory: llgenericvalue -> llgenericvalue -> lltype -> unit
  initialize_memory: llvalue -> llgenericvalue -> t -> unit
  recompile_and_relink_function: llvalue -> t -> llgenericvalue
  get_or_emit_global_variable: llvalue -> t -> llgenericvalue
  disable_lazy_compilation: t -> unit
  lazy_compilation_enabled: t -> bool
  install_lazy_function_creator: (string -> llgenericvalue) -> t -> unit  
   *)

  val initialize : unit -> unit = fn () => prim("mlkit_llvm_initialize",())
  val initialize_native_target : unit -> bool = fn () => prim("@mlkit_llvm_initialize_native_target",())

end

end
