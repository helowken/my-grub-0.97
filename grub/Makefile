srcdir = .
top_srcdir = ..
top_builddir = ..
DEPDIR = .deps

OBJEXT = o
EXEEXT = 
RANLIB = ranlib
DEFS = -DHAVE_CONFIG_H
DEFAULT_INCLUDES = -I. -I$(srcdir) -I$(top_builddir)
CPPFLAGS = -Werror -Wall -Wmissing-prototypes -Wunused -Wshadow -Wpointer-arith -falign-jumps=1 -falign-loops=1 -falign-functions=1 -Wundef
CFLAGS = -m32 -g
#TODO GRUB_CFLAGS = -O2
GRUB_CFLAGS =
AM_CFLAGS = $(GRUB_CFLAGS)
CC = gcc34

# For <stage1.h>.
INCLUDES = -I$(top_srcdir)/stage1

sbin_PROGRAMS = grub$(EXEEXT)
am_grub_OBJECTS = main.$(OBJEXT) asmstub.$(OBJEXT)
grub_OBJECTS = $(am_grub_OBJECTS)
grub_DEPENDENCIES = ../stage2/libgrub.a ../lib/libcommon.a 
HEADERS = *.h
PROGRAMS = $(sbin_PROGRAMS)
SERIAL_FLAGS = -DSUPPORT_SERIAL=1
AM_CPPFLAGS = -DGRUB_UTIL=1 -DFSYS_EXT2FS=1 -DFSYS_FAT=1 -DFSYS_FFS=1 \
			  -DFSYS_ISO9660=1 -DFSYS_JFS=1 -DFSYS_MINIX=1 -DFSYS_REISERFS=1 \
			  -DFSYS_UFS2=1 -DFSYS_VSTAFS=1 -DFSYS_XFS=1 \
			  -DUSE_MD5_PASSWORDS=1 -DSUPPORT_HERCULES=1 \
			  $(SERIAL_FLAGS) -I$(top_srcdir)/stage2 \
			  -I$(top_srcdir)/stage1 -I$(top_srcdir)/lib

LIBS = 
GRUB_LIBS = -lncurses
#GRUB_LIBS = /lib32/libncurses.so.5.9 -L/lib32 -ltinfo
grub_LDADD = ../stage2/libgrub.a  ../lib/libcommon.a $(GRUB_LIBS)

COMPILE = $(CC) $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) $(AM_CPPFLAGS) \
		          $(CPPFLAGS) $(AM_CFLAGS) $(CFLAGS)
CCLD = $(CC)
LINK = $(CCLD) $(AM_CFLAGS) $(CFLAGS) $(AM_LDFLAGS) $(LDFLAGS) -o $@

all:
	$(MAKE) all-am

all-am: Makefile $(PROGRAMS)


grub$(EXEEXT): $(grub_OBJECTS) $(grub_DEPENDENCIES)
	@rm -f grub$(EXEEXT)
	$(LINK) $(grub_LDFLAGS) $(grub_OBJECTS) $(grub_LDADD) $(LIBS)

.c.o:
	$(COMPILE) -MT $@ -MD -MP -MF "$(DEPDIR)/$*.Tpo" -c -o $@ $<


.PHONY: clean

clean:
	-rm -f $(PROGRAMS)
	-rm -f *.o



         
