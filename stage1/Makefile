srcdir = .
top_srcdir = ..
top_builddir = ..
DEPDIR = .deps

OBJEXT = o
OBJCOPY = objcopy
EXEEXT = 
RANLIB = ranlib
DEFS = -DHAVE_CONFIG_H
DEFAULT_INCLUDES = -I. -I$(srcdir) -I$(top_builddir)
CPPFLAGS = -Werror -Wall -Wmissing-prototypes -Wunused -Wshadow -Wpointer-arith -falign-jumps=1 -falign-loops=1 -falign-functions=1 -Wundef
CFLAGS = -m32 -g
#TODO STAGE1_CFLAGS = -O2
STAGE1_CFLAGS =
AM_CCASFLAGS = $(STAGE1_CFLAGS) -fno-builtin -nostdinc
CC = gcc34
CCAS = gcc34
CCLD = $(CC)
LINK = $(CCLD) $(AM_CFLAGS) $(CFLAGS) $(AM_LDFLAGS) $(LDFLAGS) -o $@
LDFLAGS = -nostdlib -Wl,-N,-Ttext,7C00
CCASFLAGS = $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) $(CPPFLAGS) $(CFLAGS)
CCASCOMPILE = $(CCAS) $(AM_CCASFLAGS) $(CCASFLAGS)

am_stage1_exec_OBJECTS = stage1.$(OBJEXT)
stage1_exec_OBJECTS = $(am_stage1_exec_OBJECTS)
stage1_exec_LDADD = $(LDADD)
nodist_pkglib_DATA = stage1
DATA = $(nodist_pkglib_DATA)
noinst_PROGRAMS = stage1.exec$(EXEEXT)
PROGRAMS = $(noinst_PROGRAMS)


all: all-am

.SUFFIXES:
.SUFFIXES: .exec .S .o

all-am: $(PROGRAMS) $(DATA)

.S.o:
	$(CCASCOMPILE) -c $<

stage1.exec$(EXEEXT): $(stage1_exec_OBJECTS) $(stage1_exec_DEPENDENCIES)
	@rm -f $@
	$(LINK) $(stage1_exec_LDFLAGS) $(stage1_exec_OBJECTS) $(stage1_exec_LDADD) $(LIBS)


.PHONY: clean disasm

clean:
	-rm -f $(PROGRAMS)
	-rm -f $(nodist_pkglib_DATA)
	-rm -f *.o

.exec:
	$(OBJCOPY) -O binary $< $@


disasm: stage1.exec$(EXEEXT)
	$(call disasmCode,$<,16)

# disasmCode()
define disasmCode
	objdump -S -d -j .text -j .rodata -j .data -j .bss -mi386 -Matt,data$(2),addr$(2) $(1) | less
endef

