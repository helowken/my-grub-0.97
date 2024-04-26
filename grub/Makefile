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
AM_CPPFLAGS = -DGRUB_UTIL=1 -DFSYS_EXT2FS=1 -DSUPPORT_HERCULES=1 \
			  -I$(top_srcdir)/stage2 -I$(top_srcdir)/stage1 -I$(top_srcdir)/lib

LIBS = 
GRUB_LIBS = -lncurses
grub_LDADD = ../stage2/libgrub.a  ../lib/libcommon.a $(GRUB_LIBS)

COMPILE = $(CC) $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) $(AM_CPPFLAGS) \
		          $(CPPFLAGS) $(AM_CFLAGS) $(CFLAGS)
CCLD = $(CC)
LINK = $(CCLD) $(AM_CFLAGS) $(CFLAGS) $(AM_LDFLAGS) $(LDFLAGS) -o $@

all:
	$(MAKE) all-am

all-am: Makefile $(PROGRAMS)


grub$(EXEEXT): $(grub_OBJECTS) $(grub_DEPENDENCIES)
	@rm -f $@
	$(LINK) $(grub_LDFLAGS) $(grub_OBJECTS) $(grub_LDADD) $(LIBS)

.c.o:
	$(COMPILE) -MT $@ -MD -MP -MF "$(DEPDIR)/$*.Tpo" -c -o $@ $<


.PHONY: clean

clean:
	-rm -f $(PROGRAMS)
	-rm -f *.o



         
