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
CC = gcc34
AR = ar
ARFLAGS = cru
libcommon_a_AR = $(AR) $(ARFLAGS)
noinst_LIBRARIES = libcommon.a
LIBRARIES = $(noinst_LIBRARIES)
AM_CFLAGS = $(GRUB_CFLAGS) -I$(top_srcdir)/stage2 \
			-I$(top_srcdir)/stage1
am_libcommon_a_OBJECTS = device.$(OBJEXT)
libcommon_a_LIBADD = 
libcommon_a_OBJECTS = $(am_libcommon_a_OBJECTS)

COMPILE = $(CC) $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) $(AM_CPPFLAGS) \
		          $(CPPFLAGS) $(AM_CFLAGS) $(CFLAGS)

all: all-am

all-am: Makefile $(LIBRARIES)

.c.o:
	$(COMPILE) -MT $@ -MD -MP -MF "$(DEPDIR)/$*.Tpo" -c -o $@ $<

libcommon.a: $(libcommon_a_OBJECTS) $(libcommon_a_DEPENDENCIES)
	-rm -f $@
	$(libcommon_a_AR) $@ $(libcommon_a_OBJECTS) $(libcommon_a_LIBADD)
	$(RANLIB) $@


.PHONY: clean


clean:
	-rm -f $(LIBRARIES)
	-rm -f *.o
