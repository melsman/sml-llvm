# First, download llvm (in source form), unpack it, and compile it:
#   cd $(HOME)/llvm-3.5.0.src/
#   TRIPLE=i686-apple-darwin10
#   ./configure --with-gxx-include-dir=/usr/include/c++/4.2.1 --build=$TRIPLE --host=$TRIPLE --target=$TRIPLE
#   make
# Download MLKit and install it.
# Update the variables LLVMDIR and MLKITINCLUDEDIR below.

PWD=$(shell pwd)
MLKITINCLUDEDIR=/usr/local/share/mlkit/include
MLKIT=mlkit
LLVMDIR=$(HOME)/llvm-3.5.0.src
LLVMINCLUDEDIR=$(LLVMDIR)/include
LLVMBINDIR=$(LLVMDIR)/Release+Asserts/bin
LLVMCONFIG=$(LLVMBINDIR)/llvm-config
#LLVM_LIBS_GEN=$(shell $(LLVMCONFIG) --libs core --libs bitwriter --libs engine --libs executionengine)
LLVM_LIBS_GEN=$(shell $(LLVMCONFIG) --libs) $(shell $(LLVMCONFIG) --system-libs)
TEST=$(shell $(LLVMCONFIG) --libs core jit native interpreter bitwriter)
#LLVM_LIBNAMES=$(shell $(LLVMCONFIG) --libnames core --libnames bitwriter)
LLVM_LDFLAGS=$(shell $(LLVMCONFIG) --ldflags)
LLVM_CPPFLAGS=$(shell $(LLVMCONFIG) --cppflags)
LLVM_LIBS=LLVMBitWriter,LLVMInterpreter,LLVMX86CodeGen,LLVMSelectionDAG,LLVMAsmPrinter,LLVMX86AsmParser,LLVMMCParser,LLVMX86Disassembler,LLVMX86Desc,LLVMX86Info,LLVMX86AsmPrinter,LLVMX86Utils,LLVMJIT,LLVMRuntimeDyld,LLVMExecutionEngine,LLVMCodeGen,LLVMScalarOpts,LLVMInstCombine,LLVMTransformUtils,LLVMipa,LLVMAnalysis,LLVMTarget,LLVMMC,LLVMObject,LLVMCore,LLVMSupport
LLVM_LIBS2=$(LLVM_LIBS_GEN:-l%=%,)
LIBS="c,LLVMMLKit,$(LLVM_LIBS2)dl"
LIBDIRS="$(LLVMDIR)/Release+Asserts/lib,."
CC="g++"
MLKIT_CC_FLAGS=-g -O2 -m32 -Wall -std=gnu99 -DTAG_VALUES -DTAG_FREE_PAIRS -DENABLE_GC

.PHONY: all
all: libLLVMMLKit.a
	$(MLKIT) -cc $(CC) --libs $(LIBS) --libdirs $(LIBDIRS) llvm.mlb

.PHONY: show
show:
	@echo "LLVM_LIBS2:"
	@echo " $(LLVM_LIBS2)"
	@echo "TEST:"
	@echo " $(TEST)"
	@echo "LLVMDIR:"
	@echo " $(LLVMDIR)"
	@echo "LLVMBINDIR:"
	@echo " $(LLVMBINDIR)"
	@echo "LLVMCONFIG"
	@echo " $(LLVMCONFIG)"
	@echo "LLVM_LIBS_GEN:"
	@echo " $(LLVM_LIBS_GEN)"
#	@echo "$(LLVM_LIBNAMES)"
	@echo "LLVM_LDFLAGS:"
	@echo " $(LLVM_LDFLAGS)"
	@echo "LLVM_CPPFLAGS:"
	@echo " $(LLVM_CPPFLAGS)"
	@echo " $(LIBS)"

libLLVMMLKit.a: LLVMMLKit.o
	cp -p $< $@

LLVMMLKit.o: llvm_mlkit.c
	gcc $(LLVM_CPPFLAGS) $(MLKIT_CC_FLAGS) -I $(MLKITINCLUDEDIR) -c -o $@ $<

miniml: miniml.ll
	$(LLVMDIR)/Release+Asserts/bin/llc -O3 miniml.bc -o miniml.s
	gcc -O3 miniml.s -o miniml
	./miniml

miniml.bc: minimlexe
	./minimlexe $@

minimlexe: libLLVMMLKit.a
	$(MLKIT) -cc $(CC) --libs $(LIBS) --libdirs $(LIBDIRS) -o $@ test/miniml/miniml.mlb

.PHONY: minimljit
minimljit: minimlexe
	./minimlexe -jit

.PHONY: minimlinterp
minimlinterp: minimlexe
	./minimlexe -interp

.PHONY: test
test: testexe
	./testexe

testexe: libLLVMMLKit.a
	$(MLKIT) -cc $(CC) --libs $(LIBS) --libdirs $(LIBDIRS) -o $@ test/unittest/unittest.mlb

test.bc: testexe
	./testexe

%.ll: %.bc
	$(LLVMDIR)/Release+Asserts/bin/llvm-dis $<

clean:
	rm -rf MLB *~ run test/*/MLB test/*/run test/*/*~ *.a *.o *.bc *.ll miniml *.s testexe minimlexe
