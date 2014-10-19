#include <unistd.h>
#include "llvm-c/ExecutionEngine.h"
#include "llvm-c/BitWriter.h"
#include "llvm-c/Core.h"
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "List.h"
#include "String.h"
#include "Exception.h"
#include "Region.h"
#include "Tagging.h"

/* lltype -> int -> llvalue */
/* MLKit auto conversion: YES */
LLVMValueRef mlkit_llvm_const_int(LLVMTypeRef IntTy, int N) {
  return LLVMConstInt(IntTy, (long long)N, 1);
}

/* lltype -> real -> llvalue */
/* MLKit auto conversion: NO */
LLVMValueRef mlkit_llvm_const_float(LLVMTypeRef RealTy, ssize_t d) {
  LLVMValueRef result;
  double value = get_d(d);
  RealTy = (LLVMTypeRef)untag_scalar(RealTy);
  result = LLVMConstReal(RealTy, value);
  result = (LLVMValueRef)tag_scalar(result);
  return result;
}

/* llvalue -> llvalue list -> string -> llbuilder -> llvalue */
/* MLKit auto conversion: NO */
LLVMValueRef mlkit_llvm_build_call(LLVMValueRef Fn, uintptr_t Params,
                                   String Name, LLVMBuilderRef B) {
  int n = 0;
  uintptr_t list = Params;
  LLVMValueRef * array;
  LLVMValueRef elemML;
  LLVMValueRef result;
  char *name = &(Name -> data);
  Fn = (LLVMValueRef)untag_scalar(Fn);
  B = (LLVMBuilderRef)untag_scalar(B);
  for (n = 0; isCONS(list); list = tl(list), n++);
  array = (LLVMValueRef *) malloc(sizeof(LLVMValueRef) * (n+1));
  list = Params;
  for (n = 0; isCONS(list); list = tl(list), n++) {
    elemML = (LLVMValueRef) hd(list);
    elemML = (LLVMValueRef) untag_scalar(elemML);
    array[n] = elemML;
  }
  array[n] = NULL;
  result = LLVMBuildCall(B, Fn, array, n, name);
  result = (LLVMValueRef)tag_scalar(result);
  return result;
}

/* unit -> int */
/* MLKit auto conversion: YES */
int mlkit_llvm_IntEQ() {
  return LLVMIntEQ;
}

/* string -> lltype -> llmodule -> llvalue */
/* MLKit auto conversion: YES */
LLVMValueRef mlkit_llvm_define_function(char* Name, LLVMTypeRef Ty, LLVMModuleRef M) { 
  LLVMValueRef Fn = LLVMAddFunction(M, Name, Ty);
  LLVMAppendBasicBlockInContext(LLVMGetTypeContext(Ty), Fn, "entry");
  return Fn;
}

/* string -> lltype -> llmodule -> llvalue */
/* MLKit auto conversion: YES */
LLVMValueRef mlkit_llvm_declare_function(char* Name, LLVMTypeRef Ty, LLVMModuleRef M) {
  LLVMValueRef Fn;
  if ((Fn = LLVMGetNamedFunction(M, Name))) {
    if (LLVMGetElementType(LLVMTypeOf(Fn)) != Ty)
      return LLVMConstBitCast(Fn, LLVMPointerType(Ty, 0));
    return Fn;
  }
  return LLVMAddFunction(M, Name, Ty);
}

void mlkit_llvm_print_type(LLVMTypeRef Ty) {
  printf("%p\n",Ty);
}


/* lltype -> lltype list -> lltype */
/* MLKit auto conversion: NO */
LLVMTypeRef mlkit_llvm_function_type0(LLVMTypeRef RetTy,
                                      uintptr_t ParamTys, int kind) {
  LLVMTypeRef *tyArray;
  uintptr_t list = ParamTys;
  LLVMTypeRef result;
  LLVMTypeRef elemML;
  int n = 0;
  RetTy = (LLVMTypeRef)untag_scalar(RetTy);
  for (n = 0; isCONS(list); list = tl(list), n++);
  tyArray = (LLVMTypeRef *) malloc(sizeof(LLVMTypeRef) * (n+1));
  list = ParamTys;
  for (n = 0; isCONS(list); list = tl(list), n++) {
    elemML = (LLVMTypeRef) hd(list);
    elemML = (LLVMTypeRef) untag_scalar(elemML);
    tyArray[n] = elemML;
  }
  tyArray[n] = NULL;
  result = LLVMFunctionType(RetTy, tyArray, n, kind);
  result = (LLVMTypeRef)tag_scalar(result);
  return result;
}

/* lltype -> lltype list -> lltype */
/* MLKit auto conversion: NO */
LLVMTypeRef mlkit_llvm_var_arg_function_type(LLVMTypeRef RetTy, uintptr_t ParamTys) {
  return mlkit_llvm_function_type0(RetTy, ParamTys, 1);
}

/* lltype -> lltype list -> lltype */
/* MLKit auto conversion: NO */
LLVMTypeRef mlkit_llvm_function_type(LLVMTypeRef RetTy, uintptr_t ParamTys) {
  return mlkit_llvm_function_type0(RetTy, ParamTys, 0);
}

/* llvalue -> llvalue list -> string -> llbuilder -> llvalue */
/* MLKit auto conversion: NO */
LLVMValueRef mlkit_llvm_build_gep(LLVMValueRef Pointer, uintptr_t Indices,
                                  String Name, LLVMBuilderRef B) {
  int n = 0;
  LLVMValueRef result;
  LLVMValueRef elemML;
  LLVMValueRef * array;
  uintptr_t list = Indices;
  Pointer = (LLVMValueRef)untag_scalar(Pointer);
  B = (LLVMBuilderRef)untag_scalar(B);
  for (n = 0; isCONS(list); list = tl(list), n++);
  array = (LLVMValueRef *) malloc(sizeof(LLVMValueRef) * (n+1));
  list = Indices;
  for (n = 0; isCONS(list); list = tl(list), n++) {
    elemML = (LLVMValueRef) hd(list);
    elemML = (LLVMValueRef) untag_scalar(elemML);
    array[n] = elemML;
  }
  array[n] = NULL;
  result = LLVMBuildGEP(B, Pointer, array, n, &(Name -> data));
  result = (LLVMValueRef)tag_scalar(result);
  return result;
}

/* llvalue -> llvalue -> llbasicblock -> unit */
/* MLKit auto conversion: YES */
void mlkit_add_incoming(LLVMValueRef PhiNode, LLVMValueRef value, LLVMBasicBlockRef bb) {
  LLVMAddIncoming(PhiNode, (LLVMValueRef*) &value, (LLVMBasicBlockRef*) &bb, 1);
}

/* llmodule -> string -> bool */
/* MLKit auto conversion: YES */
int mlkit_llvm_write_bitcode_file(LLVMModuleRef M, char* Path) {
  int res = LLVMWriteBitcodeToFile(M, Path);
  return res == 0;
}

/* GenericValue support */

/* lltype -> real -> t */
/* MLKit auto conversion: NO */
LLVMGenericValueRef mlkit_llvm_genericvalue_of_float(LLVMTypeRef Ty, ssize_t f) {
  LLVMGenericValueRef result;
  double d = get_d(f);
  Ty = (LLVMTypeRef) untag_scalar(Ty);
  result = LLVMCreateGenericValueOfFloat(Ty, d);
  result = (LLVMGenericValueRef) tag_scalar(result);
  return result;
}

/* lltype -> int -> t */
/* MLKit auto conversion: YES */
LLVMGenericValueRef mlkit_llvm_genericvalue_of_int(LLVMTypeRef Ty, int i) {
  return LLVMCreateGenericValueOfInt(Ty, i, 1);
}

/* lltype -> t -> real */
/* MLKit auto conversion: NO */
ssize_t mlkit_llvm_genericvalue_as_float(ssize_t d, LLVMTypeRef Ty, LLVMGenericValueRef g) {
  Ty = (LLVMTypeRef) untag_scalar(Ty);
  g = (LLVMGenericValueRef) untag_scalar(g);
  get_d(d) = LLVMGenericValueToFloat(Ty,g);
  set_dtag(d);
  return d;
}

/* t -> int */
/* MLKit auto conversion: YES */
int mlkit_llvm_genericvalue_as_int(LLVMGenericValueRef g) {
  if (LLVMGenericValueIntWidth(g) > 8 * sizeof(ssize_t)) {
    raise_exn((uintptr_t)&exn_OVERFLOW);
  }
  return LLVMGenericValueToInt(g, 1);
}

/* Execution Engine support */

/* llmodule -> t */
/* MLKit auto conversion: YES */
LLVMExecutionEngineRef mlkit_llvm_ee_create(LLVMModuleRef M) {
  LLVMExecutionEngineRef Engine;
  char *Error;
  if (LLVMCreateExecutionEngineForModule(&Engine, M, &Error)) {
    printf("mlkit_llvm_ee_create: %s\n", Error);
    raise_exn((uintptr_t)&exn_OVERFLOW);
  }
  return Engine;
}

/* llmodule -> t */
/* MLKit auto conversion: YES */
LLVMExecutionEngineRef mlkit_llvm_ee_create_interpreter(LLVMModuleRef M) {
  LLVMExecutionEngineRef Engine;
  char *Error;
  if (LLVMCreateInterpreterForModule(&Engine, M, &Error)) {
    printf("mlkit_llvm_ee_create_interpreter: %s\n", Error);
    raise_exn((uintptr_t)&exn_OVERFLOW);
  }
  return Engine;
}

/* llmodule -> int -> t */
/* MLKit auto conversion: YES */
LLVMExecutionEngineRef mlkit_llvm_ee_create_jit(LLVMModuleRef M, int optlevel) {
  LLVMExecutionEngineRef Engine;
  char *Error;
  if (LLVMCreateJITCompilerForModule(&Engine, M, optlevel, &Error)) {
    printf("mlkit_llvm_ee_create_jit: %s\n", Error);
    raise_exn((uintptr_t)&exn_OVERFLOW);
  }
  return Engine;
}

/* string -> t -> llvalue */
/* MLKit auto conversion: YES */
LLVMValueRef mlkit_llvm_ee_find_function(char *Name, LLVMExecutionEngineRef EE) {
  LLVMValueRef Found;
  if (LLVMFindFunction(EE, Name, &Found)) {
    raise_exn((uintptr_t)&exn_OVERFLOW);
  }
  return Found;
}

/* llvalue -> GenericValue.t list -> t -> GenericValue.t */
/* MLKit auto conversion: NO */
LLVMGenericValueRef mlkit_llvm_ee_run_function(LLVMValueRef f, uintptr_t args, LLVMExecutionEngineRef ee) {
  LLVMGenericValueRef result, elemML, *array;
  int n = 0;
  uintptr_t list = args;
  f = (LLVMValueRef) untag_scalar(f);
  ee = (LLVMExecutionEngineRef) untag_scalar(ee);

  for (n = 0; isCONS(list); list = tl(list), n++);
  array = (LLVMGenericValueRef *) malloc(sizeof(LLVMGenericValueRef) * (n+1));
  list = args;
  for (n = 0; isCONS(list); list = tl(list), n++) {
    elemML = (LLVMGenericValueRef) hd(list);
    elemML = (LLVMGenericValueRef) untag_scalar(elemML);
    array[n] = elemML;
  }
  array[n] = NULL;
  
  result = LLVMRunFunction(ee, f, n, array);
  result = (LLVMGenericValueRef) tag_scalar(result);
  return result;
}

/* Force the LLVM interpreter and JIT to be linked in. */
void mlkit_llvm_initialize(void) {
  LLVMLinkInInterpreter();
  LLVMLinkInJIT();
}

/* unit -> bool */
/* MLKit auto conversion: YES */
int mlkit_llvm_initialize_native_target() {
  return LLVMInitializeNativeTarget();
}
