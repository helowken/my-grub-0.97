srcdir = .
top_srcdir = ..
top_builddir = ..
DEPDIR = .deps

LIBS = 
OBJEXT = o
OBJCOPY = objcopy
EXEEXT = 
RANLIB = ranlib
DEFS = -DHAVE_CONFIG_H
DEFAULT_INCLUDES = -I. -I$(srcdir) -I$(top_builddir)
CPPFLAGS = -Wall -Wmissing-prototypes -Wunused -Wshadow -Wpointer-arith -falign-jumps=1 -falign-loops=1 -falign-functions=1 -Wundef
CPPFLAGS += -Werror
CFLAGS = -m32 -g
#TODO GRUB_CFLAGS = -O2
GRUB_CFLAGS = 
CC = gcc34
CCAS = gcc34
CCASFLAGS = $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) $(CPPFLAGS) $(CFLAGS)
CCLD = $(CC)
LINK = $(CCLD) $(AM_CFLAGS) $(CFLAGS) $(AM_LDFLAGS) $(LDFLAGS) -o $@
AR = ar
ARFLAGS = cru
libgrub_a_AR = $(AR) $(ARFLAGS)
NETBOOT_FLAGS = 
SERIAL_FLAGS = -DSUPPORT_SERIAL=1
HERCULES_FLAGS = -DSUPPORT_HERCULES=1
#TODO STAGE2_CFLAGS = -Os
STAGE2_CFLAGS = 
STAGE2_COMPILE = $(STAGE2_CFLAGS) -fno-builtin -nostdinc \
	$(NETBOOT_FLAGS) $(SERIAL_FLAGS) $(HERCULES_FLAGS)
STAGE1_5_LINK = -nostdlib -Wl,-N -Wl,-Ttext -Wl,2000
STAGE1_5_COMPILE = $(STAGE2_COMPILE) -DNO_DECOMPRESSION=1 -DSTAGE1_5=1
FSYS_CFLAGS = -DFSYS_EXT2FS=1
PRE_STAGE2_LINK = -nostdlib -Wl,-N -Wl,-Ttext -Wl,8200
START_LINK = -nostdlib -Wl,-N -Wl,-Ttext -Wl,8000


pkglib_DATA = stage2 e2fs_stage1_5
noinst_DATA = pre_stage2 start
DATA = $(noinst_DATA) $(pkglib_DATA)
noinst_PROGRAMS = e2fs_stage1_5.exec$(EXEEXT)
noinst_HEADERS = *.h
HEADERS = $(noinst_HEADERS)
PROGRAMS = $(noinst_PROGRAMS)
LIBRARIES = $(noinst_LIBRARIES)


# For <stage1.h>.
INCLUDES = -I$(top_srcdir)/stage1

# The library for /sbin/grub.
noinst_LIBRARIES = libgrub.a
libgrub_a_CFLAGS = $(GRUB_CFLAGS) -I$(top_srcdir)/lib \
        -DGRUB_UTIL=1 -DFSYS_EXT2FS=1 -DFSYS_FAT=1 -DFSYS_FFS=1 \
        -DFSYS_ISO9660=1 -DFSYS_JFS=1 -DFSYS_MINIX=1 -DFSYS_REISERFS=1 \
        -DFSYS_UFS2=1 -DFSYS_VSTAFS=1 -DFSYS_XFS=1 \
        -DUSE_MD5_PASSWORDS=1 -DSUPPORT_SERIAL=1 -DSUPPORT_HERCULES=1

libgrub_a_OBJECTS = libgrub_a-boot.$(OBJEXT) \
        libgrub_a-builtins.$(OBJEXT) libgrub_a-char_io.$(OBJEXT) \
        libgrub_a-cmdline.$(OBJEXT) libgrub_a-common.$(OBJEXT) \
        libgrub_a-disk_io.$(OBJEXT) libgrub_a-fsys_ext2fs.$(OBJEXT) \
        libgrub_a-gunzip.$(OBJEXT) \
		libgrub_a-serial.$(OBJEXT) \
        libgrub_a-stage2.$(OBJEXT)


# For e2fs_stage1_5 target.
am_e2fs_stage1_5_exec_OBJECTS = e2fs_stage1_5_exec-start.$(OBJEXT) \
	e2fs_stage1_5_exec-asm.$(OBJEXT) \
	e2fs_stage1_5_exec-common.$(OBJEXT) \
	e2fs_stage1_5_exec-char_io.$(OBJEXT) \
	e2fs_stage1_5_exec-disk_io.$(OBJEXT) \
	e2fs_stage1_5_exec-stage1_5.$(OBJEXT) \
	e2fs_stage1_5_exec-fsys_ext2fs.$(OBJEXT) \
	e2fs_stage1_5_exec-bios.$(OBJEXT)
e2fs_stage1_5_exec_OBJECTS = $(am_e2fs_stage1_5_exec_OBJECTS)
e2fs_stage1_5_exec_LDADD = $(LDADD)
e2fs_stage1_5_exec_SOURCES = start.S asm.S common.c char_io.c disk_io.c \
	stage1_5.c fsys_ext2fs.c bios.c
e2fs_stage1_5_exec_CFLAGS = $(STAGE1_5_COMPILE) -DFSYS_EXT2FS=1 -DNO_BLOCK_FILES=1
e2fs_stage1_5_exec_CCASFLAGS = $(STAGE1_5_COMPILE) -DFSYS_EXT2FS=1 -DNO_BLOCK_FILES=1
e2fs_stage1_5_exec_LDFLAGS = $(STAGE1_5_LINK)


# For stage2 target.
am_start_exec_OBJECTS = start_exec-start.$(OBJEXT)
start_exec_OBJECTS = $(am_start_exec_OBJECTS)
start_exec_LDADD = $(LDADD)
start_exec_CCASFLAGS = $(STAGE2_COMPILE)
start_exec_LDFLAGS = $(START_LINK)
am_pre_stage2_exec_OBJECTS = pre_stage2_exec-asm.$(OBJEXT) \
	pre_stage2_exec-bios.$(OBJEXT) \
	pre_stage2_exec-boot.$(OBJEXT) \
	pre_stage2_exec-builtins.$(OBJEXT) \
	pre_stage2_exec-char_io.$(OBJEXT) \
	pre_stage2_exec-cmdline.$(OBJEXT) \
	pre_stage2_exec-common.$(OBJEXT) \
	pre_stage2_exec-console.$(OBJEXT) \
	pre_stage2_exec-disk_io.$(OBJEXT) \
	pre_stage2_exec-fsys_ext2fs.$(OBJEXT) \
	pre_stage2_exec-gunzip.$(OBJEXT) \
	pre_stage2_exec-hercules.$(OBJEXT) \
	pre_stage2_exec-md5.$(OBJEXT) \
	pre_stage2_exec-serial.$(OBJEXT) \
	pre_stage2_exec-smp-imps.$(OBJEXT) \
	pre_stage2_exec-stage2.$(OBJEXT) \
	pre_stage2_exec-terminfo.$(OBJEXT) \
	pre_stage2_exec-tparm.$(OBJEXT)
pre_stage2_exec_OBJECTS = $(am_pre_stage2_exec_OBJECTS)
pre_stage2_exec_CFLAGS = $(STAGE2_COMPILE) $(FSYS_CFLAGS)
pre_stage2_exec_CCASFLAGS = $(STAGE2_COMPILE) $(FSYS_CFLAGS)
pre_stage2_exec_LDFLAGS = $(PRE_STAGE2_LINK)
BUILT_SOURCES = stage2_size.h
CLEANFILES = $(LIBRARIES) $(PROGRAMS) $(DATA) $(BUILT_SOURCES)


all:
	$(MAKE) all-am

all-am: Makefile $(LIBRARIES) $(PROGRAMS) $(DATA) $(HEADERS)

.SUFFIXES:
.SUFFIXES: .exec

# ============================== 
#     libgrub.a
# ============================== 
libgrub.a: $(libgrub_a_OBJECTS)
	-rm -f $@
	$(libgrub_a_AR) $@ $(libgrub_a_OBJECTS)
	$(RANLIB) $@

libgrub_a-boot.o: boot.c
	$(CC) $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) $(CPPFLAGS) $(libgrub_a_CFLAGS) $(CFLAGS) -MT $@ -MD -MP -MF "$(DEPDIR)/libgrub_a-boot.Tpo" -c -o $@ $<
        
libgrub_a-builtins.o: builtins.c
	$(CC) $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) $(CPPFLAGS) $(libgrub_a_CFLAGS) $(CFLAGS) -MT $@ -MD -MP -MF "$(DEPDIR)/libgrub_a-builtins.Tpo" -c -o $@ $<
        
libgrub_a-char_io.o: char_io.c
	$(CC) $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) $(CPPFLAGS) $(libgrub_a_CFLAGS) $(CFLAGS) -MT $@ -MD -MP -MF "$(DEPDIR)/libgrub_a-char_io.Tpo" -c -o $@ $<

libgrub_a-cmdline.o: cmdline.c
	$(CC) $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) $(CPPFLAGS) $(libgrub_a_CFLAGS) $(CFLAGS) -MT $@ -MD -MP -MF "$(DEPDIR)/libgrub_a-cmdline.Tpo" -c -o $@ $<

libgrub_a-common.o: common.c
	$(CC) $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) $(CPPFLAGS) $(libgrub_a_CFLAGS) $(CFLAGS) -MT $@ -MD -MP -MF "$(DEPDIR)/libgrub_a-common.Tpo" -c -o $@ $<

libgrub_a-disk_io.o: disk_io.c
	$(CC) $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) $(CPPFLAGS) $(libgrub_a_CFLAGS) $(CFLAGS) -MT $@ -MD -MP -MF "$(DEPDIR)/libgrub_a-disk_io.Tpo" -c -o $@ $<

libgrub_a-fsys_ext2fs.o: fsys_ext2fs.c
	$(CC) $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) $(CPPFLAGS) $(libgrub_a_CFLAGS) $(CFLAGS) -MT $@ -MD -MP -MF "$(DEPDIR)/libgrub_a-fsys_ext2fs.Tpo" -c -o $@ $<

libgrub_a-gunzip.o: gunzip.c
	$(CC) $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) $(CPPFLAGS) $(libgrub_a_CFLAGS) $(CFLAGS) -MT $@ -MD -MP -MF "$(DEPDIR)/libgrub_a-gunzip.Tpo" -c -o $@ $<

libgrub_a-stage2.o: stage2.c
	$(CC) $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) $(CPPFLAGS) $(libgrub_a_CFLAGS) $(CFLAGS) -MT $@ -MD -MP -MF "$(DEPDIR)/libgrub_a-stage2.Tpo" -c -o $@ $<

libgrub_a-serial.o: serial.c
	$(CC) $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) $(CPPFLAGS) $(libgrub_a_CFLAGS) $(CFLAGS) -MT $@ -MD -MP -MF "$(DEPDIR)/libgrub_a-serial.Tpo" -c -o $@ $<



# ============================== 
#     ext2fs stage1.5
# ============================== 
e2fs_stage1_5_exec-start.o: start.S
	$(CCAS) $(e2fs_stage1_5_exec_CCASFLAGS) $(CCASFLAGS) -c -o $@ $<

e2fs_stage1_5_exec-asm.o: asm.S
	$(CCAS) $(e2fs_stage1_5_exec_CCASFLAGS) $(CCASFLAGS) -c -o $@ $<

e2fs_stage1_5_exec-common.o: common.c
	$(CC) $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) $(AM_CPPFLAGS) $(CPPFLAGS) $(e2fs_stage1_5_exec_CFLAGS) $(CFLAGS) -MT $@ -MD -MP -MF "$(DEPDIR)/e2fs_stage1_5_exec-common.Tpo" -c -o $@ $<

e2fs_stage1_5_exec-char_io.o: char_io.c
	$(CC) $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) $(AM_CPPFLAGS) $(CPPFLAGS) $(e2fs_stage1_5_exec_CFLAGS) $(CFLAGS) -MT $@ -MD -MP -MF "$(DEPDIR)/e2fs_stage1_5_exec-char_io.Tpo" -c -o $@ $<

e2fs_stage1_5_exec-disk_io.o: disk_io.c
	$(CC) $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) $(AM_CPPFLAGS) $(CPPFLAGS) $(e2fs_stage1_5_exec_CFLAGS) $(CFLAGS) -MT $@ -MD -MP -MF "$(DEPDIR)/e2fs_stage1_5_exec-disk_io.Tpo" -c -o $@ $<

e2fs_stage1_5_exec-stage1_5.o: stage1_5.c
	$(CC) $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) $(AM_CPPFLAGS) $(CPPFLAGS) $(e2fs_stage1_5_exec_CFLAGS) $(CFLAGS) -MT $@ -MD -MP -MF "$(DEPDIR)/e2fs_stage1_5_exec-stage1_5.Tpo" -c -o $@ $<

e2fs_stage1_5_exec-fsys_ext2fs.o: fsys_ext2fs.c
	$(CC) $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) $(AM_CPPFLAGS) $(CPPFLAGS) $(e2fs_stage1_5_exec_CFLAGS) $(CFLAGS) -MT $@ -MD -MP -MF "$(DEPDIR)/e2fs_stage1_5_exec-fsys_ext2fs.Tpo" -c -o $@ $<

e2fs_stage1_5_exec-bios.o: bios.c
	$(CC) $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) $(AM_CPPFLAGS) $(CPPFLAGS) $(e2fs_stage1_5_exec_CFLAGS) $(CFLAGS) -MT $@ -MD -MP -MF "$(DEPDIR)/e2fs_stage1_5_exec-bios.Tpo" -c -o $@ $<

e2fs_stage1_5.exec$(EXEEXT): $(e2fs_stage1_5_exec_OBJECTS) $(e2fs_stage1_5_exec_DEPENDENCIES) 
	@rm -f $@
	$(LINK) $(e2fs_stage1_5_exec_LDFLAGS) $(e2fs_stage1_5_exec_OBJECTS) $(e2fs_stage1_5_exec_LDADD) $(LIBS)


# ============================== 
#     stage2
# ============================== 
stage2_size.h: pre_stage2
	-rm -f $@
	set dummy `ls -l pre_stage2`; \
	echo "#define STAGE2_SIZE $$6" > $@

stage2: pre_stage2 start
	-rm -f $@
	cat start pre_stage2 > $@


# For start.
start_exec-start.$(OBJEXT): stage2_size.h

start_exec-start.o: start.S
	$(CCAS) $(start_exec_CCASFLAGS) $(CCASFLAGS) -c -o $@ $<

start.exec$(EXEEXT): $(start_exec_OBJECTS) $(start_exec_DEPENDENCIES) 
	@rm -f $@
	$(LINK) $(start_exec_LDFLAGS) $(start_exec_OBJECTS) $(start_exec_LDADD) $(LIBS)


# For pre_stage2.
pre_stage2.exec$(EXEEXT): $(pre_stage2_exec_OBJECTS) $(pre_stage2_exec_DEPENDENCIES) 
	@rm -f $@
	$(LINK) $(pre_stage2_exec_LDFLAGS) $(pre_stage2_exec_OBJECTS) $(pre_stage2_exec_LDADD) $(LIBS)

pre_stage2_exec-asm.o: asm.S
	$(CCAS) $(pre_stage2_exec_CCASFLAGS) $(CCASFLAGS) -c -o $@ $<

pre_stage2_exec-bios.o: bios.c
	$(CC) $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) $(AM_CPPFLAGS) $(CPPFLAGS) $(pre_stage2_exec_CFLAGS) $(CFLAGS) -MT $@ -MD -MP -MF "$(DEPDIR)/pre_stage2_exec-bios.Tpo" -c -o $@ $<

pre_stage2_exec-boot.o: boot.c
	$(CC) $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) $(AM_CPPFLAGS) $(CPPFLAGS) $(pre_stage2_exec_CFLAGS) $(CFLAGS) -MT $@ -MD -MP -MF "$(DEPDIR)/pre_stage2_exec-boot.Tpo" -c -o $@ $<

pre_stage2_exec-builtins.o: builtins.c
	$(CC) $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) $(AM_CPPFLAGS) $(CPPFLAGS) $(pre_stage2_exec_CFLAGS) $(CFLAGS) -MT $@ -MD -MP -MF "$(DEPDIR)/pre_stage2_exec-builtins.Tpo" -c -o $@ $<

pre_stage2_exec-char_io.o: char_io.c
	$(CC) $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) $(AM_CPPFLAGS) $(CPPFLAGS) $(pre_stage2_exec_CFLAGS) $(CFLAGS) -MT $@ -MD -MP -MF "$(DEPDIR)/pre_stage2_exec-char_io.Tpo" -c -o $@ $<

pre_stage2_exec-cmdline.o: cmdline.c
	$(CC) $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) $(AM_CPPFLAGS) $(CPPFLAGS) $(pre_stage2_exec_CFLAGS) $(CFLAGS) -MT $@ -MD -MP -MF "$(DEPDIR)/pre_stage2_exec-cmdline.Tpo" -c -o $@ $<

pre_stage2_exec-common.o: common.c
	$(CC) $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) $(AM_CPPFLAGS) $(CPPFLAGS) $(pre_stage2_exec_CFLAGS) $(CFLAGS) -MT $@ -MD -MP -MF "$(DEPDIR)/pre_stage2_exec-common.Tpo" -c -o $@ $<

pre_stage2_exec-console.o: console.c
	$(CC) $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) $(AM_CPPFLAGS) $(CPPFLAGS) $(pre_stage2_exec_CFLAGS) $(CFLAGS) -MT $@ -MD -MP -MF "$(DEPDIR)/pre_stage2_exec-console.Tpo" -c -o $@ $<

pre_stage2_exec-disk_io.o: disk_io.c
	$(CC) $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) $(AM_CPPFLAGS) $(CPPFLAGS) $(pre_stage2_exec_CFLAGS) $(CFLAGS) -MT $@ -MD -MP -MF "$(DEPDIR)/pre_stage2_exec-disk_io.Tpo" -c -o $@ $<

pre_stage2_exec-fsys_ext2fs.o: fsys_ext2fs.c
	$(CC) $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) $(AM_CPPFLAGS) $(CPPFLAGS) $(pre_stage2_exec_CFLAGS) $(CFLAGS) -MT $@ -MD -MP -MF "$(DEPDIR)/pre_stage2_exec-fsys_ext2fs.Tpo" -c -o $@ $<

pre_stage2_exec-gunzip.o: gunzip.c
	$(CC) $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) $(AM_CPPFLAGS) $(CPPFLAGS) $(pre_stage2_exec_CFLAGS) $(CFLAGS) -MT $@ -MD -MP -MF "$(DEPDIR)/pre_stage2_exec-gunzip.Tpo" -c -o $@ $<

pre_stage2_exec-hercules.o: hercules.c
	$(CC) $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) $(AM_CPPFLAGS) $(CPPFLAGS) $(pre_stage2_exec_CFLAGS) $(CFLAGS) -MT $@ -MD -MP -MF "$(DEPDIR)/pre_stage2_exec-hercules.Tpo" -c -o $@ $<

pre_stage2_exec-md5.o: md5.c
	$(CC) $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) $(AM_CPPFLAGS) $(CPPFLAGS) $(pre_stage2_exec_CFLAGS) $(CFLAGS) -MT $@ -MD -MP -MF "$(DEPDIR)/pre_stage2_exec-md5.Tpo" -c -o $@ $<

pre_stage2_exec-serial.o: serial.c
	$(CC) $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) $(AM_CPPFLAGS) $(CPPFLAGS) $(pre_stage2_exec_CFLAGS) $(CFLAGS) -MT $@ -MD -MP -MF "$(DEPDIR)/pre_stage2_exec-serial.Tpo" -c -o $@ $<

pre_stage2_exec-smp-imps.o: smp-imps.c
	$(CC) $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) $(AM_CPPFLAGS) $(CPPFLAGS) $(pre_stage2_exec_CFLAGS) $(CFLAGS) -MT $@ -MD -MP -MF "$(DEPDIR)/pre_stage2_exec-smp-imps.Tpo" -c -o $@ $<

pre_stage2_exec-stage2.o: stage2.c
	$(CC) $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) $(AM_CPPFLAGS) $(CPPFLAGS) $(pre_stage2_exec_CFLAGS) $(CFLAGS) -MT $@ -MD -MP -MF "$(DEPDIR)/pre_stage2_exec-stage2.Tpo" -c -o $@ $<

pre_stage2_exec-terminfo.o: terminfo.c
	$(CC) $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) $(AM_CPPFLAGS) $(CPPFLAGS) $(pre_stage2_exec_CFLAGS) $(CFLAGS) -MT $@ -MD -MP -MF "$(DEPDIR)/pre_stage2_exec-terminfo.Tpo" -c -o $@ $<

pre_stage2_exec-tparm.o: tparm.c
	$(CC) $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) $(AM_CPPFLAGS) $(CPPFLAGS) $(pre_stage2_exec_CFLAGS) $(CFLAGS) -MT $@ -MD -MP -MF "$(DEPDIR)/pre_stage2_exec-tparm.Tpo" -c -o $@ $<




.PHONY: clean

clean:
	-rm -f $(CLEANFILES)
	-rm -f *.o

.exec:
	$(OBJCOPY) -O binary $< $@

        

disasm1_5: e2fs_stage1_5.exec$(EXEEXT)
	$(call disasmCode,$<,16)

disasmStart: start.exec$(EXEEXT)
	$(call disasmCode,$<,16)

disasmPre16: pre_stage2.exec$(EXEEXT)
	$(call disasmCode,$<,16)

disasmPre32: pre_stage2.exec$(EXEEXT)
	$(call disasmCode,$<,32)


# disasmCode()
define disasmCode
	objdump -S -d -j .text -j .rodata -j .data -j .bss -mi386 -Matt,data$(2),addr$(2) $(1) | less
endef

