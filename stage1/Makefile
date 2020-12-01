# This Makefile produces the CakeML compiler executable by linking the
# bootstrapped CakeML compiler with the implementation of the FFI primitives.
# It also includes rules for compiling CakeML programs with the CakeML compiler
# then similarly linking the resulting machine code with the FFI primitives to
# produce executables. It is designed to work on GNU/Linux, macOS, and Windows.

# To set the stack and heap sizes for CakeML programs, set the
# CML_STACK_SIZE and CML_HEAP_SIZE environment variables (or see basis_ffi.c)
# The unit of measure for both arguments is mebibytes (1024 * 1024 bytes).

SRC_LIST = src/Error.sml src/Type.sml src/Coding.sml src/Encoder.sml src/Decoder.sml src/PrettyString.sml src/Contract.sml src/Runtime.sml

# Change this directory if necessary  -- or
# provide the directory for your machine on the make command-line, e.g.
# make -n   CAKE_DIR="/someOtherLocation/cake-x64-64"
CAKE_DIR = ~/cake-x64-64
CAKEC = $(CAKE_DIR)/cake
BASIS = $(CAKE_DIR)/basis_ffi.c

OS ?= $(shell uname)

ifeq ($(OS),Windows_NT)
	PREF =
	SUFF = .exe
else
	PREF = ./
	SUFF =
endif

ifeq ($(OS),Darwin)
	# These options avoid linker warnings on macOS
	LDFLAGS += -Wl,-no_pie
endif

DEBUG = true
ifeq ($(DEBUG), true)
	CFLAGS += -ggdb3
else
	CFLAGS = -DNDEBUG
endif

CC = gcc

.PHONY: all
all: contract clean

contract: contractFull.S basis_ffi.o
	$(CC) $(CFLAGS) $(LDFLAGS) -o $@ $^

contractFull.S: contractFull.sml
	$(CAKEC) < contractFull.sml > contractFull.S

contractFull.sml: $(SRC_LIST) Example.sml
	cat $^ > $@

basis_ffi.o: $(BASIS)
	$(CC) $(CFLAGS) -c $(BASIS)

.PHONY: clean
clean:
	rm -f contractFull.S contractFull.sml