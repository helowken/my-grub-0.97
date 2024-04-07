/* builtins.c - the GRUB builtin commands */
/*
 *  GRUB  --  GRand Unified Bootloader
 *  Copyright (C) 1999,2000,2001,2002,2003,2004  Free Software Foundation, Inc.
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#ifdef GRUB_UTIL
# include <stdio.h>
#endif /* GRUB_UTIL */

#include <shared.h>
#include <filesys.h>
#include <term.h>






#ifdef SUPPORT_SERIAL
# include <serial.h>
# include <terminfo.h>
#endif

#ifdef GRUB_UTIL
# include <device.h>
#else /* ! GRUB_UTIL */
# include <apic.h>
# include <smp-imps.h>
#endif /* ! GRUB_UTIL */

#ifdef USE_MD5_PASSWORDS
# include <md5.h>
#endif

/* The type of kernel loaded. */
kernel_t kernel_type;
/* The boot device. */
static int bootdev;
/* True when the debug mode is turned on, and false
 * when it is turned off. */
int debug = 0;
/* The default entry. */
int default_entry = 0;
/* The fallback entry. */
int fallback_entryno;
int fallback_entries[MAX_FALLBACK_ENTRIES];
/* The number of current entry. */
int current_entryno;
/* The address for Multiboot command-line buffer. */
static char *mb_cmdline;
/* The password. */
char *password;
/* The password type. */
//password_t password_type;
/* The flag for indicating that the user is authoritative. */
int auth = 0;
/* The timeout. */
int grub_timeout = -1;
/* Whether to show the menu or not. */
int show_menu = 1;
/* The BIOS drive map. */
//static unsigned short bios_drive_map[DRIVE_MAP_SIZE + 1];





/* Initialize the data for builtins. */
void init_builtins() {
	kernel_type = KERNEL_TYPE_NONE;
	/* BSD and chainloading evil hacks! */
	bootdev = set_bootdev(0);
	mb_cmdline = (char *) MB_CMDLINE_BUF;
}

/* Initialize the data for the configuration file. */
void init_config() {
	default_entry = 0;
	password = 0;
	fallback_entryno = -1;
	fallback_entries[0] = -1;
	grub_timeout = -1;
}


/* Print which sector is read when loading a file. */
static void disk_read_print_func(int sector, int offset, int length) {
	grub_printf("[%d,%d,%d]", sector, offset, length);
}


/* blocklist */
static int blocklist_func(char *arg, int flags) {
	char *dummy = (char *) RAW_ADDR(0x100000);
	int start_sector;
	int num_sectors = 0;
	int num_entries = 0;
	int last_length = 0;

	auto void disk_read_blocklist_func(int sector, int offset, int length);

	/* Collect contiguous blocks into one entry as many as possible,
	 * and print the blocklist notation on the screen. */
	auto void disk_read_blocklist_func(int sector, int offset, int length) {
		if (num_sectors > 0) {
			if (start_sector + num_sectors == sector &&
						offset == 0 && last_length == SECTOR_SIZE) {
				num_sectors++;
				last_length = length;
				return;
			} else {
				if (last_length == SECTOR_SIZE)
				  grub_printf("%s%d+%d", num_entries ? "," : "", 
							start_sector - part_start, num_sectors);
				else if (num_sectors > 1)
				  grub_printf("%s%d+%d,%d[0-%d]", num_entries ? "," : "",
							start_sector - part_start, num_sectors - 1,
							start_sector + num_sectors - 1 - part_start,
							last_length);
				else
				  grub_printf("%s%d[0-%d]", num_entries ? "," : "",
							start_sector - part_start, last_length);

				num_entries++;
				num_sectors = 0;
			}
		}

		if (offset > 0) {
			grub_printf("%s%d[%d-%d]", num_entries ? "," : "",
					sector - part_start, offset, offset + length);
			num_entries++;
		} else {
			start_sector = sector;
			num_sectors = 1;
			last_length = length;
		}
	}

	/* Open the file. */
	if (! grub_open(arg))
	  return 1;

	/* Print the device name. */
	grub_printf("(%cd%d",
			(current_drive & 0x80) ? 'h' : 'f',
			current_drive & ~0x80);

	if ((current_partition & 0xFF0000) != 0xFF0000)
	  grub_printf(",%d", (current_partition >> 16) & 0xFF);

	if ((current_partition & 0x00FF00) != 0x00FF00)
	  grub_printf(",%c", 'a' + ((current_partition >> 8) & 0xFF));

	grub_printf(")");

	/* Read in the whole file to DUMMY. */
	disk_read_hook = disk_read_blocklist_func;
	if (! grub_read(dummy, -1))
	  goto fail;

	/* The last entry may not be printed yet.  Don't check if it is a
	 * full sector, since it doesn't matter if we read too much. */
	if (num_sectors > 0)
	  grub_printf("%s%d+%d", num_entries ? "," : "",
				start_sector - part_start, num_sectors);

	grub_printf("\n");

fail:
	disk_read_hook = 0;
	grub_close();
	return errnum;
}

static struct builtin builtin_blocklist = {
	"blocklist",
	blocklist_func,
	BUILTIN_CMDLINE | BUILTIN_HELP_LIST,
	"blocklist FILE",
	"Print the blocklist notation of the file FILE."
};


/* boot */
static int boot_func(char *arg, int flags) {
	/* Clear the int15 handler if we can boot the kernel successfully.
	 * This assumes that the boot code never fails only if KERNEL_TYPE is
	 * not KERNEL_TYPE_NONE. Is this assumption is bad? */
	if (kernel_type != KERNEL_TYPE_NONE)
	  unset_int15_handler();

	switch (kernel_type) {
		case KERNEL_TYPE_LINUX:
			/* Linux */
			linux_boot();
			break;

		case KERNEL_TYPE_BIG_LINUX:
			/* Big Linux */
			big_linux_boot();
			break;

		default:
			errnum = ERR_BOOT_COMMAND;
			return 1;
	}

	return 0;
}

static struct builtin builtin_boot = {
	"boot",
	boot_func,
	BUILTIN_CMDLINE | BUILTIN_HELP_LIST,
	"boot",
	"Boot the OS/chain-loader which has been loaded."
};


/* cat */
static int cat_func(char *arg, int flags) {
	char c;

	if (! grub_open(arg))
	  return 1;
	
	while (grub_read(&c, 1)) {
		/* Because running "cat" with a binary file can confuse the terminal,
		 * print only some characters as they are. */
		if (grub_isspace(c) || (c >= ' '&& c <= '~'))
		  grub_putchar(c);
		else
		  grub_putchar('?');
	}
	grub_close();
	return 0;
}

static struct builtin builtin_cat = {
	"cat",
	cat_func,
	BUILTIN_CMDLINE | BUILTIN_HELP_LIST,
	"cat FILE",
	"Print the contents of the file FILE. "
};


/* configfile */
static int configfile_func(char *arg, int flags) {
	char *new_config = config_file;

	/* Check if the file ARG is present. */
	if (! grub_open(arg))
	  return 1;

	grub_close();

	/* Copy ARG to CONFIG_FILE. */
	while ((*new_config++ = *arg++) != 0) {
	}

#ifdef GRUB_UTIL
	/* Force to load the configuration file. */
	use_config_file = 1;
#endif

	/* Make sure that the user will not be authoritative. */
	auth = 0;

	/* Restart cmain. */
	grub_longjmp(restart_env, 0);

	/* Never reach here. */
	return 0;
}

static struct builtin builtin_configfile = {
	"configfile",
	configfile_func,
	BUILTIN_CMDLINE | BUILTIN_HELP_LIST,
	"configfile FILE",
	"Load FILE as the configuration file."
};


/* debug */
static int debug_func(char *arg, int flags) {
	if (debug) {
		debug = 0;
		grub_printf(" Debug mode is turned off\n");
	} else {
		debug = 1;
		grub_printf(" Debug mode is turned on\n");
	}

	return 0;
}

static struct builtin builtin_debug = {
	"debug",
	debug_func,
	BUILTIN_CMDLINE,
	"debug",
	"Turn on/off the debug mode."
};


/* default */
static int default_func(char *arg, int flags) {
#ifndef SUPPORT_DISKLESS
	if (grub_strcmp(arg, "saved") == 0) {
		default_entry = saved_entryno;
		return 0;
	}
#endif /* ! SUPPORT_DISKLESS */

	if (! safe_parse_maxint(&arg, &default_entry))
	  return 1;

	return 0;
}

static struct builtin builtin_default = {
	"default",
	default_func,
	BUILTIN_MENU,
#if 0
	"default [NUM | `saved']",
	"Set the default entry to entry number NUM (if not specified, it is"
	" 0, the first entry) or the entry number saved by savedefault."
#endif
};


#ifdef GRUB_UTIL
/* device */
static int device_func(char *arg, int flags) {
	char *drive = arg;
	char *device;

	/* Get the drive number from DRIVE. */
	if (! set_device(drive))
	  return 1;

	/* Get the device argument. */
	device = skip_to(0, drive);

	/* Terminate DEVICE. */
	nul_terminate(device);

	if (! *device || ! check_device(device)) {
		errnum = ERR_FILE_NOT_FOUND;
		return 1;
	}

	assign_device_name(current_drive, device);

	return 0;
}

static struct builtin builtin_device = {
	"device",
	device_func,
	BUILTIN_MENU | BUILTIN_CMDLINE | BUILTIN_HELP_LIST,
	"device DRIVE DEVICE",
	"Specify DEVICE as the actual drive for a BIOS drive DRIVE. This command"
	" can be used only in the grub shell."
};
#endif


/* displayapm */
static int displayapm_func(char *arg, int flags) {
	if (mbi.flags & MB_INFO_APM_TABLE) {
		grub_printf("APM BIOS information:\n"
			" Version:          0x%x\n"
			" 32-bit CS:        0x%x\n"
			" Offset:           0x%x\n"
			" 16-bit CS:        0x%x\n"
			" 16-bit DS:        0x%x\n"
			" 32-bit CS length: 0x%x\n"
			" 16-bit CS length: 0x%x\n"
			" 16-bit DS length: 0x%x\n",
			(unsigned) apm_bios_info.version,
			(unsigned) apm_bios_info.cseg,
			apm_bios_info.offset,
			(unsigned) apm_bios_info.cseg_16,
			(unsigned) apm_bios_info.dseg_16,
			(unsigned) apm_bios_info.cseg_len,
			(unsigned) apm_bios_info.cseg_16_len,
			(unsigned) apm_bios_info.dseg_16_len);
	} else {
		grub_printf("No APM BIOS found or probe failed\n");
	}

	return 0;
}

static struct builtin builtin_displayapm = {
	"displayapm",
	displayapm_func,
	BUILTIN_CMDLINE | BUILTIN_HELP_LIST,
	"displayapm",
	"Display APM BIOS information."
};

/* displaymem */
static int displaymem_func(char *arg, int flags) {
	if (get_eisamemsize() != -1)
	  grub_printf(" EISA Memory BIOS Interface is present\n");
	if (get_mmap_entry((void *) SCRATCHADDR, 0) != 0 ||
				*((int *) SCRATCHADDR) != 0)
	  grub_printf(" Address Map BIOS Interface is present\n");

	grub_printf(" Lower memory: %uK, "
				" Upper memory (to first chipset hole): %uK\n",
				mbi.mem_lower, mbi.mem_upper);

	if (mbi.flags & MB_INFO_MEM_MAP) {
		struct AddrRangeDesc *map = (struct AddrRangeDesc *) mbi.mmap_addr;
		int end_addr = mbi.mmap_addr + mbi.mmap_length;

		grub_printf(" [Address Range Descriptor entries "
			"immediately follow (values are 64-bit)]\n");
		while (end_addr > (int) map) {
			char *str;

			if (map->Type == MB_ARD_MEMORY)
			  str = "Usable RAM";
			else
			  str = "Reserved";

			grub_printf("   %s:  Base Address:  0x%x X 4GB + 0x%x,\n"
					"      Length:  0x%x X 4GB + 0x%x bytes\n",
					str,
					(unsigned long) (map->BaseAddr >> 32),
					(unsigned long) (map->BaseAddr & 0xFFFFFFFF),
					(unsigned long) (map->Length >> 32),
					(unsigned long) (map->Length & 0xFFFFFFFF));

			map = ((struct AddrRangeDesc *) (((int) map) + 4 + map->size));
		}
	}

	return 0;
}

static struct builtin builtin_displaymem = {
	"displaymem",
	displaymem_func,
	BUILTIN_CMDLINE | BUILTIN_HELP_LIST,
	"displaymem",
	"Display what GRUB thinks the system address space map of the"
	" machine is, including all regions of physical RAM installed."
};


static char embed_info[32];
/* embed */
/* Embed a Stage 1.5 in the first cylinder after MBR or in the
 * bootloader block in a FFS. */
static int embed_func(char *arg, int flags) {
	char *stage1_5;
	char *device;
	char *stage1_5_buffer = (char *) RAW_ADDR(0x100000);
	int len, size;
	int sector;

	stage1_5 = arg;
	device = skip_to(0, stage1_5);
	
	/* Open a Stage 1.5. */
	if (! grub_open(stage1_5)) 
	  return 1;

	/* Read the whole of the Stage 1.5. */
	len = grub_read(stage1_5_buffer, -1);
	grub_close();

	if (errnum)
	  return 1;

	size = (len + SECTOR_SIZE - 1) / SECTOR_SIZE;

	/* Get the device where the Stage 1.5 will be embeded. */
	set_device(device);
	if (errnum)
	  return 1;

	if (current_partition == 0xFFFFFF) {
		/* Embed it after the MBR. */

		char mbr[SECTOR_SIZE];
		int i;

		/* Open the partition. */
		if (! open_partition())
		  return 1;

		/* No floppy has MBR. */
		if (! (current_drive & 0x80)) {
			errnum = ERR_DEV_VALUES;
			return 1;
		}

		/* Read the MBR of CURRENT_DRIVE. */
		if (! rawread(current_drive, PC_MBR_SECTOR, 0, SECTOR_SIZE, mbr))
		  return 1;

		/* Sanity check. */
		if (! PC_MBR_CHECK_SIG(mbr)) {
			errnum = ERR_BAD_PART_TABLE;
			return 1;
		}

		/* Check if the disk can store the Stage 1.5. */
		for (i = 0; i < 4; i++) {
			if (PC_SLICE_TYPE(mbr, i) && PC_SLICE_START(mbr, i) - 1 < size) {
				errnum = ERR_NO_DISK_SPACE;
				return 1;
			}
		}

		// NO_SUPPORT: for EZ-BIOS
		
		sector = 1;
	} else {
		// NO_SUPPORT: for FFS partition */
		sector = -1;
	}

	/* Clear the cache. */
	buf_track = -1;

	/* Now perform the embedding. */
	if (! devwrite(sector - part_start, size, stage1_5_buffer))
	  return 1;

	grub_printf(" %d sectors are embeded.\n", size);
	grub_sprintf(embed_info, "%d+%d", sector - part_start, size);
	return 0;
}

static struct builtin builtin_embed = {
	"embed",
	embed_func,
	BUILTIN_CMDLINE,
	"embed STAGE1_5 DEVICE",
	"Embed the Stage 1.5 STAGE1_5 in the sectors after MBR if DEVICE"
	" is a drive, or in the \"bootloader\" area if DEVICE is a FFS partition."
	" Print the number of sectors which STAGE1_5 occupies if successful."
};


/* fallback */
static int fallback_func(char *arg, int flags) {
	int i = 0;

	while (*arg) {
		int entry;
		int j;

		if (! safe_parse_maxint(&arg, &entry))
		  return 1;

		/* Remove duplication to prevent infinite looping. */
		for (j = 0; j < i; j++) {
			if (entry == fallback_entries[j])
			  break;
		}
		if (j != i)
		  continue;

		fallback_entries[i++] = entry;
		if (i == MAX_FALLBACK_ENTRIES)
		  break;

		arg = skip_to(0, arg);
	}

	if (i < MAX_FALLBACK_ENTRIES)
	  fallback_entries[i] = -1;

	fallback_entryno = (i == 0) ? -1 : 0;

	return 0;
}

static struct builtin builtin_fallback = {
	"fallback",
	fallback_func,
	BUILTIN_MENU,
#if 0
	"fallback NUM...",
	"Go into unattended boot mode: if the default boot entry has any"
	" errors, instead of waiting for the user to do anything, it"
	" immediately starts over using the NUM entry (same numbering as the"
	" `default' command). This obviously won't help if the machine"
	" was rebooted by a kernel that GRUB loaded."
#endif
};


/* find */
/* Search for the filename ARG in all of partitions. */
static int find_func(char *arg, int flags) {
	char *filename = arg;
	unsigned long drive;
	unsigned long tmp_drive = saved_drive;
	unsigned long tmp_partition = saved_partition;
	int got_file = 0;

	/* Floppies. */
	for (drive = 0; drive < 8; drive++) {
		current_drive = drive;
		current_partition = 0xFFFFFF;

		if (open_device()) {
			saved_drive = current_drive;
			saved_partition = current_partition;
			if (grub_open(filename)) {
				grub_close();
				grub_printf(" (fd%d)\n", drive);
				got_file = 1;
			}
		}

		errnum = ERR_NONE;
	}

	/* Hard disks. */
	for (drive = 0x80; drive < 0x88; drive++) {
		unsigned long part = 0xFFFFFF;
		unsigned long start, len, offset, ext_offset;
		int type, entry;
		char buf[SECTOR_SIZE];

		current_drive = drive;
		while (next_partition(drive, 0xFFFFFF, &part, &type,
						&start, &len, &offset, &entry,
						&ext_offset, buf)) {
			if (type != PC_SLICE_TYPE_NONE &&
					! IS_PC_SLICE_TYPE_BSD(type) &&
					! IS_PC_SLICE_TYPE_EXTENDED(type)) {
				current_partition = part;
				if (open_device()) {
					saved_drive = current_drive;
					saved_partition = current_partition;
					if (grub_open(filename)) {
						int bsd_part = (part >> 8) & 0xFF;
						int pc_slice = part >> 16;

						grub_close();

						if (bsd_part == 0xFF)
						  grub_printf(" (hd%d,%d)\n",
								  drive - 0x80, pc_slice);
						else
						  grub_printf(" (hd%d,%d,%c)\n", 
								drive - 0x80, pc_slice, bsd_part + 'a');

						got_file = 1;
					}
				}
			}

			/* We want to ignore any error here. */
			errnum = ERR_NONE;
		}

		/* next_partition always sets ERRNUM in the last call, so clear it */
		errnum = ERR_NONE;
	}

	saved_drive = tmp_drive;
	saved_partition = tmp_partition;

	if (got_file) {
		errnum = ERR_NONE;
		return 0;
	}

	errnum = ERR_FILE_NOT_FOUND;
	return 1;
}

static struct builtin builtin_find = {
	"find",
	find_func,
	BUILTIN_CMDLINE | BUILTIN_HELP_LIST,
	"find FILENAME",
	"Search for the filename FILENAME in all of partitions and print the list of"
	" the devices which contain the file."
};


/* fstest */
static int fstest_func(char *arg, int flags) {
	if (disk_read_hook) {
		disk_read_hook = NULL;
		printf(" Filesystem tracing is now off\n");
	} else {
		disk_read_hook = disk_read_print_func;
		printf(" Filesystem tracing is now on\n");
	}

	return 0;
}

static struct builtin builtin_fstest = {
	"fstest",
	fstest_func,
	BUILTIN_CMDLINE,
	"fstest",
	"Toggle filesystem test mode."
};


/* geometry */
static int geometry_func(char *arg, int flags) {
	struct geometry geom;
	char *msg;
	char *device = arg;
#ifdef GRUB_UTIL
	char *ptr;
#endif /* GRUB_UTIL */

	/* Get the device number. */
	set_device(device);
	if (errnum)
	  return 1;

	/* Check for the geometry. */
	if (get_diskinfo(current_drive, &geom)) {
		errnum = ERR_NO_DISK;
		return 1;
	}

	/* Attempt to read the first sector, because some BIOSes turns out not
	 * to support LBA even though they set the bit 0 in the support
	 * bitmap, only after reading something actually. */
	if (biosdisk(BIOSDISK_READ, current_drive, &geom, 0, 1, SCRATCHSEG)) {
		errnum = ERR_READ;
		return 1;
	}

#ifdef GRUB_UTIL
	ptr = skip_to(0, device);
	if (*ptr) {
		char *cylinder, *head, *sector, *total_sector;
		int num_cylinder, num_head, num_sector, num_total_sector;
	
		cylinder = ptr;
		head = skip_to(0, cylinder);
		sector = skip_to(0, head);
		total_sector = skip_to(0, sector);
		if (! safe_parse_maxint(&cylinder, &num_cylinder) ||
					! safe_parse_maxint(&head, &num_head) ||
					! safe_parse_maxint(&sector, &num_sector))
		  return 1;
	
		disks[current_drive].cylinders = num_cylinder;
		disks[current_drive].heads = num_head;
		disks[current_drive].sectors = num_sector;

		if (safe_parse_maxint(&total_sector, &num_total_sector))
		  disks[current_drive].total_sectors = num_total_sector;
		else
		  disks[current_drive].total_sectors = num_cylinder * num_head * num_sector;
		errnum = 0;

		geom = disks[current_drive];
		buf_drive = -1;
	}
#endif /* GRUB_UTIL */

#ifdef GRUB_UTIL
	msg = device_map[current_drive];
#else
	if (geom.flags && BIOSDISK_FLAG_LBA_EXTENSION)
	  msg = "LBA";
	else
	  msg = "CHS";
#endif /* GRUB_UTIL */
	
	grub_printf("drive 0x%x: C/H/S = %d/%d/%d, "
				"The number of sectors = %d, %s\n",
				current_drive,
				geom.cylinders, geom.heads, geom.sectors,
				geom.total_sectors, msg);
	real_open_partition(1);

	return 0;
}

static struct builtin builtin_geometry = {
	"geometry",
	geometry_func,
	BUILTIN_CMDLINE | BUILTIN_HELP_LIST,
	"geometry DRIVE [CYLINDER HEAD SECTOR [TOTAL_SECTOR]]",
	"Print the information for a drive DRIVE. In the grub shell, you can"
	" set the geometry of the drive arbitrarily. The number of the cylinders,"
	" the one of the heads, the one of the sectors and the one of the total"
	" sectors are set to CYLINDER, HEAD, SECTOR and TOTAL_SECTOR,"
	" respectively. If you omit TOTAL_SECTOR, then it will be calculated based"
	" on the C/H/S values automatically."
};


/* help */
#define MAX_SHORT_DOC_LEN	39
#define MAX_LONG_DOC_LEN	66

static int help_func(char *arg, int flags) {
	int all = 0;

	if (grub_memcmp(arg, "--all", sizeof("--all") - 1) == 0) {
		all = 1;
		arg = skip_to(0, arg);
	}

	if (! *arg) {
		/* Invoked with no argument. Print the list of the short docs. */
		struct builtin **builtin;
		int left = 1;

		for (builtin = builtin_table; *builtin != 0; builtin++) {
			int len;
			int i;

			/* If this cannot be used in the command-line interface,
			 * skip this. */
			if (! ((*builtin)->flags & BUILTIN_CMDLINE))
			  continue;

			/* If this doesn't need to be listed automatically and "--all"
			 * is not specified, skip this. */
			if (! all && ! ((*builtin)->flags & BUILTIN_HELP_LIST))
			  continue;

			len = grub_strlen((*builtin)->short_doc);
			/* If the length of SHORT_DOCS is too long, truncate it. */
			if (len > MAX_SHORT_DOC_LEN - 1)
			  len = MAX_SHORT_DOC_LEN - 1;

			for (i = 0; i < len; i++) {
				grub_putchar((*builtin)->short_doc[i]);
			}

			for (; i < MAX_SHORT_DOC_LEN; i++) {
				grub_putchar(' ');
			}

			if (! left)
			  grub_putchar('\n');

			left = ! left;
		}

		/* If the last entry was at the left column, no newline was printed
		 * at the end. */
		if (! left)
		  grub_putchar('\n');
	} else {
		/* Invoked with one or more patterns. */
		do {
			struct builtin **builtin;
			char *next_arg;

			/* Get the next argument. */
			next_arg = skip_to(0, arg);

			/* Terminate ARG. */
			nul_terminate(arg);

			for (builtin = builtin_table; *builtin; builtin++) {
				/* Skip this if this is only for the configuration file. */
				if (! ((*builtin)->flags & BUILTIN_CMDLINE))
				  continue;

				if (substring(arg, (*builtin)->name) < 1) {
					char *doc = (*builtin)->long_doc;

					/* At first, print the name and the short doc. */
					grub_printf("%s: %s\n",
							(*builtin)->name, (*builtin)->short_doc);

					/* Print the long doc. */
					while (*doc) {
						int len = grub_strlen(doc);
						int i;

						/* If LEN is too long, fold DOC. */
						if (len > MAX_LONG_DOC_LEN) {
							/* Fold this line at the position of a space. */
							for (len = MAX_LONG_DOC_LEN; len > 0; len--) {
								if (doc[len - 1] == ' ')
								  break;
							}
						}

						grub_printf("    ");
						for (i = 0; i < len; i++) {
							grub_putchar(*doc++);
						}
						grub_putchar('\n');
					}
				}
			}

			arg = next_arg;

		} while (*arg);
	}

	return 0;
}

static struct builtin builtin_help = {
	"help",
	help_func,
	BUILTIN_CMDLINE | BUILTIN_HELP_LIST,
	"help [--all] [PATTERN ...]",
	"Display222 helpful information about builtin commands. Not all commands"
	" aren't shown without the option `--all'."
};


/* hiddenmenu */
static int hiddenmenu_func(char *arg, int flags) {
	show_menu = 0;
	return 0;
}

static struct builtin builtin_hiddenmenu = {
	"hiddenmenu",
	hiddenmenu_func,
	BUILTIN_MENU,
#if 0
	"hiddenmenu",
	"Hide the menu."
#endif
};


/* impsprobe */
static int impsprobe_func(char *arg, int flags) {
#ifdef GRUB_UTIL
	/* In the grub shell, we cannot probe IMPS. */
	errnum = ERR_UNRECOGNIZED;
	return 1;
#else /* ! GRUB_UTIL */
	//TODO if (! imps_probe()) 
	  printf(" No MPS information found or probe failed\n");

	return 0;
#endif /* ! GRUB_UTIL */
}

static struct builtin builtin_impsprobe = {
	"impsprobe",
	impsprobe_func,
	BUILTIN_CMDLINE,
	"impsprobe",
	"Probe the Intel Multiprocessor Specification 1.1 or 1.4"
	" configuration table and boot the various CPUs which are found into"
	" a tight loop."
};


/* initrd */
static int initrd_func(char *arg, int flags) {
	switch (kernel_type) {
		case KERNEL_TYPE_LINUX:
		case KERNEL_TYPE_BIG_LINUX:
			if (! load_initrd(arg))
			  return 1;
			break;
		default:
			errnum = ERR_NEED_LX_KERNEL;
			return 1;
	}
	return 0;
}

static struct builtin builtin_initrd = {
	"initrd",
	initrd_func,
	BUILTIN_CMDLINE | BUILTIN_HELP_LIST,
	"initrd FILE [ARG ...]",
	"Load an initial ramdisk FILE for a Linux format boot image and set the"
	" appropriate parameters in the Linux setup area in memory."
};


/* install */
static int install_func(char *arg, int flags) {
	char *stage1_file, *dest_dev, *file, *addr;
	char *stage1_buffer = (char *) RAW_ADDR(0x100000);
	char *stage2_buffer = stage1_buffer + SECTOR_SIZE;
	char *old_sect = stage2_buffer + SECTOR_SIZE;
	char *stage2_first_buffer = old_sect + SECTOR_SIZE;
	char *stage2_second_buffer = stage2_first_buffer + SECTOR_SIZE;
	/* XXX: Probably SECTOR_SIZE is reasonable. */
	char *config_filename = stage2_second_buffer + SECTOR_SIZE;
	char *dummy = config_filename + SECTOR_SIZE;
	int new_drive = GRUB_INVALID_DRIVE;
	int dest_drive, dest_partition, dest_sector;
	int src_drive, src_partition, src_part_start;
	int i;
	struct geometry dest_geom, src_geom;
	int saved_sector;
	int stage2_first_sector, stage2_second_sector;
	char *ptr;
	int installaddr, installlist;
	/* Point to the location of the name of a configuration file in Stage 2. */
	char *config_file_location;
	/* If FILE is a Stage 1.5? */
	int is_stage1_5 = 0;
	/* Must call grub_close? */
	int is_open = 0;
	/* If LBA is forced? */
	int is_force_lba = 0;
	/* Was the last sector full? */
	int last_length = SECTOR_SIZE;

#ifdef GRUB_UTIL
	/* If the Stage 2 is in a partition mounted by an OS, this will store
	 * the filename under the OS. */
	char *stage2_os_file = 0;
#endif /* GRUB_UTIL */

	auto void disk_read_savesect_func(int sector, int offset, int length);
	auto void disk_read_blocklist_func(int sector, int offset, int length);

	/* Save the first sector of Stage2 in STAGE2_SECT. */
	auto void disk_read_savesect_func(int sector, int offset, int length) {
		if (debug)
		  printf("[%d]", sector);

		if (offset != 0 || length != SECTOR_SIZE)
		  errnum = ERR_UNALIGNED;

		saved_sector = sector;
	}

	/* Write SECTOR to INSTALLLIST, and update INSTALLADDR and
	 * INSTALLLIST. */
	auto void disk_read_blocklist_func(int sector, int offset, int length) {
		if (debug)
		  printf("[%d]", sector);

		if (offset != 0 || last_length != SECTOR_SIZE) {
			/* We found a non-sector-aligned data block. */
			errnum = ERR_UNALIGNED;
			return;
		}

		last_length = length;

		/* See data definition in the bottom of start.S.
		 *
		 * installlist - 4 (long): start sector
		 * installlist (short): sectors count
		 * installlist + 2 (short): segment
		 */
		if (*((unsigned long *) (installlist - 4)) + 
						*((unsigned short *) installlist) != sector ||
					installlist == (int) stage2_first_buffer + SECTOR_SIZE + 4) {
			installlist -= 8;

			if (*((unsigned long *) (installlist - 8)))
			  errnum = ERR_WONT_FIT;
			else {
				*((unsigned short *) (installlist + 2)) = (installaddr >> 4);
				*((unsigned long *) (installlist - 4)) = sector;
			}
		}

		*((unsigned short *) installlist) += 1;
		installaddr += 512;
	}

	/* First, check the GNU-style long option. */
	while (1) {
		if (grub_memcmp("--force-lba", arg, sizeof("--force-lba") - 1) == 0) {
			is_force_lba = 1;
			arg = skip_to(0, arg);
		}
#ifdef GRUB_UTIL
		else if (grub_memcmp("--stage2=", arg, sizeof("--stage2=") - 1) == 0) {
			stage2_os_file = arg + sizeof("--stage2=") - 1;
			arg = skip_to(0, arg);
			nul_terminate(stage2_os_file);
		}
#endif /* GRUB_UTIL */
		else
		  break;
	}

	stage1_file = arg;
	dest_dev = skip_to(0, stage1_file);
	if (*dest_dev == 'd') {
		new_drive = 0;
		dest_dev = skip_to(0, dest_dev);
	}
	file = skip_to(0, dest_dev);
	addr = skip_to(0, file);

	/* Get the installation address. */
	if (! safe_parse_maxint(&addr, &installaddr)) {
		/* ADDR is not specified. */
		installaddr = 0;
		ptr = addr;
		errnum = 0;
	} else
	  ptr = skip_to(0, addr);

#ifndef NO_DECOMPRESSION
	/* Do not decompress Stage 1 or Stage 2. */
	no_decompression = 1;
#endif /* NO_DECOMPRESSION */

	/* Read Stage 1. */
	is_open = grub_open(stage1_file);
	if (! is_open || 
			grub_read(stage1_buffer, SECTOR_SIZE) != SECTOR_SIZE)
	  goto fail;

	/* Read the old sector from DEST_DEV. */
	if (! set_device(dest_dev) ||
			! open_partition() ||
			! devread(0, 0, SECTOR_SIZE, old_sect))
	  goto fail;

	/* Store the information for the destination device. */
	dest_drive = current_drive;
	dest_partition = current_partition;
	dest_geom = buf_geom;
	dest_sector = part_start;

	/* Copy the possible DOS BPB, 59 bytes at byte offset 3. */
	grub_memmove(stage1_buffer + BOOTSEC_BPB_OFFSET,
				old_sect + BOOTSEC_BPB_OFFSET,
				BOOTSEC_BPB_LENGTH);

	/* If for a hard disk, copy the possible MBR/extended part table. */
	if (dest_drive & 0x80) 
	  grub_memmove(stage1_buffer + STAGE1_WINDOWS_NT_MAGIC,
				old_sect + STAGE1_WINDOWS_NT_MAGIC,
				STAGE1_PARTEND - STAGE1_WINDOWS_NT_MAGIC);

	/* Check for the version and the signature of Stage 1. */
	if (*((short *) (stage1_buffer + STAGE1_VER_MAJ_OFFS)) != COMPAT_VERSION ||
			(*((unsigned short *) (stage1_buffer + BOOTSEC_SIG_OFFSET)) != BOOTSEC_SIGNATURE)) {
		errnum = ERR_BAD_VERSION;
		goto fail;
	}

	/* NOT_SUPPORT: a floppy */

	grub_close();

	/* Open Stage 2. */
	is_open = grub_open(file);
	if (! is_open)
	  goto fail;

	src_drive = current_drive;
	src_partition = current_partition;
	src_part_start = part_start;
	src_geom = buf_geom;

	if (! new_drive)
	  new_drive = src_drive;
	else if (src_drive != dest_drive)
	  grub_printf("Warning: the option `d' was not used, but the Stage 1 will"
			" be installed on a\n different drive than the drive where"
			" the Stage 2 resides.\n");

	/* Set the boot drive. */
	*((unsigned char *) (stage1_buffer + STAGE1_BOOT_DRIVE)) = new_drive;

	/* Set the "force LBA" flag. */
	*((unsigned char *) (stage1_buffer + STAGE1_FORCE_LBA)) = is_force_lba;

	/* If DEST_DRIVE is a hard disk, enable the workaround, which is
	 * for buggy BIOSes which don't pass boot drive correctly. Instead,
	 * they pass 0x00 or 0x01 even when booted from 0x80. */
	if (dest_drive & BIOS_FLAG_FIXED_DISK) {
		/* Replace the jmp (2 bytes) with double nop's. */
		*((unsigned short *) (stage1_buffer + STAGE1_BOOT_DRIVE_CHECK)) = 0x9090;
	}

	/* Read the first sector of Stage 2. */
	disk_read_hook = disk_read_savesect_func;
	if (grub_read(stage2_first_buffer, SECTOR_SIZE) != SECTOR_SIZE)
	  goto fail;

	stage2_first_sector = saved_sector;
	
	/* Read the second sector of Stage 2. */
	if (grub_read(stage2_second_buffer, SECTOR_SIZE) != SECTOR_SIZE)
	  goto fail;

	stage2_second_sector = saved_sector;

	/* Check for the version of Stage 2. */
	if (*((short *) (stage2_second_buffer + STAGE2_VER_MAJ_OFFS)) != COMPAT_VERSION) {
		errnum = ERR_BAD_VERSION;
		goto fail;
	}

	/* Check for Stage 2 id. */
	if (stage2_second_buffer[STAGE2_STAGE2_ID] != STAGE2_ID_STAGE2)
	  is_stage1_5 = 1;

	/* If INSTALLADDR is not specified explicitly in the command-line,
	 * determine it by the Stage 2 id. */
	if (! installaddr) {
		if (! is_stage1_5)
		  /* Stage 2. */
		  installaddr = 0x8000;
		else
		  /* Stage 1.5. */
		  installaddr = 0x2000;
	}

	*((unsigned long *) (stage1_buffer + STAGE1_STAGE2_SECTOR)) = stage2_first_sector;
	*((unsigned short *) (stage1_buffer + STAGE1_STAGE2_ADDRESS)) = installaddr;
	*((unsigned short *) (stage1_buffer + STAGE1_STAGE2_SEGMENT)) = installaddr >> 4;

	i = (int) stage2_first_buffer + SECTOR_SIZE - 4;
	while (*((unsigned long *) i)) {
		if (i < (int) stage2_first_buffer || 
				(*((int *) (i - 4)) & 0x80000000) ||
				*((unsigned short *) i) >= 0xA00 ||
				*((short *) (i + 2)) == 0) {
			errnum = ERR_BAD_VERSION;
			goto fail;
		}
		*((int *) i) = 0;
		*((int *) (i - 4)) = 0;
		i -= 8;
	}

	/* for the first time to setup the start sector, sector count and segment. */
	installlist = (int) stage2_first_buffer + SECTOR_SIZE + 4;
	installaddr += SECTOR_SIZE;

	/* Read the whole of Stage2 except for the first sector. */
	grub_seek(SECTOR_SIZE);
	
	disk_read_hook = disk_read_blocklist_func;
	if (! grub_read(dummy, -1))
	  goto fail;

	disk_read_hook = 0;

	/* Find a string for the configuration filename. */
	config_file_location = stage2_second_buffer + STAGE2_VER_STR_OFFS;
	while (*(config_file_location++)) {
	}

	/* Set the "force LBA" flag for Stage2. */
	*((unsigned char *) (stage2_second_buffer + STAGE2_FORCE_LBA)) = is_force_lba;

	if (*ptr == 'p') {
		*((long *) (stage2_second_buffer + STAGE2_INSTALLPART)) = src_partition;
		if (is_stage1_5) {
			/* Reset the device information in FILE if it is a Stage 1.5. */
			unsigned long device = 0xFFFFFFFF;

			grub_memmove(config_file_location, (char *) &device, sizeof(device));
		}

		ptr = skip_to(0, ptr);
	}

	if (*ptr) {
		grub_strcpy(config_filename, ptr);
		nul_terminate(config_filename);

		if (! is_stage1_5)
		  /* If it is a Stage 2, just copy PTR to CONFIG_FILE_LOCATION. */
		  grub_strcpy(config_file_location, ptr);
		else {
			char *real_config;
			unsigned long device;

			/* Translate the external device syntax to the internal device
			 * syntax. */
			if (! (real_config = set_device(ptr))) {
				/* The Stage 2 PTR does not contain the device name, so
				 * use the root device instead. */
				errnum = ERR_NONE;
				current_drive = saved_drive;
				current_partition = saved_partition;
				real_config = ptr;
			}

			if (current_drive == src_drive) {
				/* If the drive where the Stage2 resides is the same as
				 * the one where the Stage 1.5 resides, do not embed the
				 * drive number. */
				current_drive = GRUB_INVALID_DRIVE;
			}

			device = (current_drive << 24) | current_partition;
			grub_memmove(config_file_location, (char *) &device, sizeof(device));
			grub_strcpy(config_file_location + sizeof(device), real_config);
		}

		/* If a Stage 1.5 is used, then we need to modify the Stage2. */
		if (is_stage1_5) {
			char *real_config_filename = skip_to(0, ptr);

			is_open = grub_open(config_filename);
			if (! is_open)
			  goto fail;

			/* Skip the first sector. */
			grub_seek(SECTOR_SIZE);

			disk_read_hook = disk_read_savesect_func;
			if (grub_read(stage2_buffer, SECTOR_SIZE) != SECTOR_SIZE)
			  goto fail;

			disk_read_hook = 0;
			grub_close();
			is_open = 0;

			/* Sanity check. */
			if (*(stage2_buffer + STAGE2_STAGE2_ID) != STAGE2_ID_STAGE2) {
				errnum = ERR_BAD_VERSION;
				goto fail;
			}

			/* Set the "force LBA" flag for Stage2. */
			*(stage2_buffer + STAGE2_FORCE_LBA) = is_force_lba;

			/* If REAL_CONFIG_FILENAME is specified, copy it to the Stage2. */
			if (*real_config_filename) {
				/* Specified. */
				char *location;

				/* Find a string for the configuration filename. */
				location = stage2_buffer + STAGE2_VER_STR_OFFS;
				while (*(location++)) {
				}

				/* Copy the name. */
				grub_strcpy(location, real_config_filename);
			}

			/* Write it to the disk. */
			buf_track = -1;

#ifdef GRUB_UTIL
			/* In the grub shell, access the Stage 2 via the OS filesystem
			 * service, if possible. */
			if (stage2_os_file) {
				FILE *fp;

				fp = fopen(stage2_os_file, "r+");
				if (! fp) {
					errnum = ERR_FILE_NOT_FOUND;
					goto fail;
				}

				if (fseek(fp, SECTOR_SIZE, SEEK_SET) != 0) {
					fclose(fp);
					errnum = ERR_BAD_VERSION;
					goto fail;
				}

				if (fwrite(stage2_buffer, 1, SECTOR_SIZE, fp) != SECTOR_SIZE) {
					fclose(fp);
					errnum = ERR_WRITE;
					goto fail;
				}

				fclose(fp);
			}
			else 
#endif /* GRUB_UTIL */
			{
				if (! devwrite(saved_sector - part_start, 1, stage2_buffer))
				  goto fail;
			}
		}
	}

	/* Clear the cache. */
	buf_track = -1;

	/* Write the modified sectors of Stage2 to the disk. */
#ifdef GRUB_UTIL
	if (! is_stage1_5 && stage2_os_file) {
		FILE *fp;

		fp = fopen(stage2_os_file, "r+");
		if (! fp) {
			errnum = ERR_FILE_NOT_FOUND;
			goto fail;
		}

		if (fwrite(stage2_first_buffer, 1, SECTOR_SIZE, fp) != SECTOR_SIZE) {
			fclose(fp);
			errnum = ERR_WRITE;
			goto fail;
		}

		if (fwrite(stage2_second_buffer, 1, SECTOR_SIZE, fp) != SECTOR_SIZE) {
			fclose(fp);
			errnum = ERR_WRITE;
			goto fail;
		}

		fclose(fp);
	}
	else
#endif /* GRUB_UTIL */
	{
		/* Modify the 1st and 2nd sectors of stage1.5. */
		current_drive = src_drive;
		current_partition = src_partition;

		if (! open_partition()) 
		  goto fail;

		if (! devwrite(stage2_first_sector - src_part_start, 1, stage2_first_buffer))
		  goto fail;

		if (! devwrite(stage2_second_sector - src_part_start, 1, stage2_second_buffer))
		  goto fail;
	}

	/* Write the modified sector of Stage 1 to the disk. */
	current_drive = dest_drive;
	current_partition = dest_partition;
	if (! open_partition())
	  goto fail;

	devwrite(0, 1, stage1_buffer);

fail:
	if (is_open)
	  grub_close();

	disk_read_hook = 0;

#ifndef NO_DECOMPRESSION
	no_decompression = 0;
#endif /* ! NO_DECOMPRESSION */

	return errnum;
}


/* ioprobe */
static int ioprobe_func(char *arg, int flags) {
#ifdef GRUB_UTIL
	errnum = ERR_UNRECOGNIZED;
	return 1;
#else /* ! GRUB_UTIL */

	unsigned short *port;

	/* Get the drive number. */
	set_device(arg);
	if (errnum)
	  return 1;

	/* Clean out IO_MAP. */
	grub_memset((char *) io_map, 0, IO_MAP_SIZE * sizeof(unsigned short));

	/* Track the int13 handler. */
	track_int13(current_drive);

	/* Print out the result. */
	for (port = io_map; *port != 0; port++) {
		grub_printf(" 0x%x", (unsigned int) *port);
	}

	return 0;
#endif /* ! GRUB_UTIL */
}

static struct builtin builtin_ioprobe = {
	"ioprobe",
	ioprobe_func,
	BUILTIN_CMDLINE,
	"ioprobe DRIVE",
	"Probe I/O ports used for the drive DRIVE."
};


/* kernel. */
static int kernel_func(char *arg, int flags) {
	int len;
	kernel_t suggested_type = KERNEL_TYPE_NONE;
	unsigned long load_flags = 0;

#ifndef AUTO_LINUX_MEM_OPT
	load_flags |= KERNEL_LOAD_NO_MEM_OPTION;
#endif /* ! AUTO_LINUX_MEM_OPT */

	/* Deal with GNU-style long options. */
	while (1) {
		/* If the option `--type=TYPE' is specified, convert the string to
		 * a kernel type. */
		if (grub_memcmp(arg, "--type=", 7) == 0) {
			arg += 7;

			if (grub_memcmp(arg, "linux", 5) == 0)
			  suggested_type = KERNEL_TYPE_LINUX;
			else if (grub_memcmp(arg, "biglinux", 8) == 0)
			  suggested_type = KERNEL_TYPE_BIG_LINUX;
			else if (grub_memcmp(arg, "multiboot", 9) == 0)
			  suggested_type = KERNEL_TYPE_MULTIBOOT;
			else {
				errnum = ERR_BAD_ARGUMENT;
				return 1;
			}
		} else if (grub_memcmp(arg, "--no-mem-option", 15) == 0) {
		/* If the `--no-mem-option' is specified, don't pass a Linux's mem
		 * option automatically. If the kernel is another type, this flag
		 * has no effect. */
			load_flags |= KERNEL_LOAD_NO_MEM_OPTION;
		} else
		  break;

		/* Try the next. */
		arg = skip_to(0, arg);
	}

	len = grub_strlen(arg);

	/* Reset MB_CMDLINE. */
	mb_cmdline = (char *) MB_CMDLINE_BUF;
	if (len + 1 > MB_CMDLINE_BUFLEN) {
		errnum = ERR_WONT_FIT;
		return 1;
	}

	/* Copy the command-line to MB_CMDLINE. */
	grub_memmove(mb_cmdline, arg, len + 1);
	kernel_type = load_image(arg, mb_cmdline, suggested_type, load_flags);
	if (kernel_type == KERNEL_TYPE_NONE)
	  return 1;
	
	mb_cmdline += len + 1;
	return 0;
}

static struct builtin builtin_kernel = {
	"kernel",
	kernel_func,
	BUILTIN_CMDLINE | BUILTIN_HELP_LIST,
	"kernel [--no-mem-option] [--type=TYPE] FILE [ARG ...]",
	"Attempt to load the primary boot image from FILE. The rest of the"
	" line is passed verbatim as the \"kernel command line\".  Any modules"
	" must be reloaded after using this command. The option --type is used"
	" to suggest what type of kernel to be loaded. TYPE must be either of"
	" \"linux\", \"biglinux\" and \"multiboot\". The option --no-mem-option"
	" tells GRUB not to pass a Linux's mem option automatically."
};


/* reboot */
static int reboot_func(char *arg, int flags) {
	grub_reboot();

	/* Never reach here. */
	return 1;
}

static struct builtin builtin_reboot = {
	"reboot",
	reboot_func,
	BUILTIN_CMDLINE | BUILTIN_HELP_LIST,
	"reboot",
	"Reboot your system."
};


#ifdef GRUB_UTIL
static int quit_func(char *arg, int flags) {
	stop();

	/* Never reach here. */
	return 0;
}

static struct builtin builtin_quit = {
	"quit",
	quit_func,
	BUILTIN_CMDLINE | BUILTIN_HELP_LIST,
	"quit",
	"Exit from the GRUB shell."
};
#endif /* GRUB_UTIL */


/* Print the root device information. */
static void print_root_device() {
	if (saved_drive == NETWORK_DRIVE) {
		/* Network drive. */
		grub_printf("* (nd):");
	} else if (saved_drive & 0x80) {
		/* Hard disk drive. */
		grub_printf(" (hd%d", saved_drive - 0x80);

		if ((saved_partition & 0xFF0000) != 0xFF0000)
		  grub_printf(",%d", saved_partition >> 16);

		if ((saved_partition & 0x00FF00) != 0x00FF00)
		  grub_printf(",%c", ((saved_partition >>8) & 0xFF) + 'a');

		grub_printf("):");
	} else {
		/* Floppy disk drive. */
		grub_printf(" (fd%d):", saved_drive);
	}

	/* Print the filesystem information. */
	current_partition = saved_partition;
	current_drive = saved_drive;
	print_fsys_type();
}


/* Setup SAVED_DRIVE and SAVED_PARTITION.
 */
static int real_root_func(char *arg, int attempt_mount) {
	int hdbias = 0;
	char *biasptr;
	char *next;

	/* If ARG is empty, just print the current root device. */
	if (! *arg) {
		print_root_device();
		return 0;
	}

	/* Call set_device to get the drive and the partition in ARG. */
	next = set_device(arg);
	if (! next)
	  return 1;

	/* Ignore ERR_FSYS_MOUNT. */
	if (attempt_mount) {
		if (! open_device() && errnum != ERR_FSYS_MOUNT)
		  return 1;
	} else {
		/* This is necessary, because the location of a partition table
		 * must be set appropriately. */
		if (open_partition()) {
			set_bootdev(0);
			if (errnum)
			  return 1;
		}
	}

	/* Clear ERRNUM. */
	errnum = 0;
	saved_partition = current_partition;
	saved_drive = current_drive;

	if (attempt_mount) {
		/* BSD and chainloading evil hacks !! */
		biasptr = skip_to(0, next);
		safe_parse_maxint(&biasptr, &hdbias);
		errnum = 0;
		bootdev = set_bootdev(hdbias);
		if (errnum)
		  return 1;

		/* Print the type of the filesystem. */
		print_fsys_type();
	}

	return 0;
}


static int root_func(char *arg, int flags) {
	return real_root_func(arg, 1);
}

static struct builtin builtin_root = {
	"root",
	root_func,
	BUILTIN_CMDLINE | BUILTIN_HELP_LIST,
	"root [DEVICE [HDBIAS]]",
	"Set the current \"root device\" to the device DEVICE, then"
	" attempt to mount it to get the partition szie (for passing the"
	" partition descriptor in `ES:ESI', used by some chain-loaded"
	" bootloadersz), the BSD drive-type (for booting BSD kernels using"
	" their native boot format), and correctly determine "
	" the PC partition where a BSD sub-partition is located. The"
	" optional HDBIAS parameter is a number to tell a BSD kernel"
	" how many BIOS drive numbers are on controllers before the current"
	" one. For example, if there is an IDE disk and a SCSI disk, and your"
	" FreeBSD root partition is on the SCSI disk, then use a `1' for HDBIAS."
};


/* setup */
static int setup_func(char *arg, int flags) {
	/* Point to the string of the installed drive/partition. */
	char *install_ptr;
	/* Point to the string of the drive/partition where the GRUB images
	 * reside. */
	char *image_ptr;
	unsigned long installed_drive, installed_partition;
	unsigned long image_drive, image_partition;
	unsigned long tmp_drive, tmp_partition;
	char stage1[64];
	char stage2[64];
	char config_filename[64];
	char real_config_filename[64];
	char cmd_arg[256];
	char device[16];
	char *buffer = (char *) RAW_ADDR(0x100000);
	int is_force_lba = 0;
	char *stage2_arg = 0;
	char *prefix = 0;

	auto int check_file(char *file);
	auto void sprint_device(int drive, int partition);
	auto int embed_stage1_5(char *stage1_5, int drive, int partition);

	/* Check if the file FILE exists like Autoconf. */
	int check_file(char *file) {
		int ret;

		grub_printf(" Checking if \"%s\" exists... ", file);
		ret = grub_open(file);
		if (ret) {
			grub_close();
			grub_printf("yes\n");
		} else 
		  grub_printf("no\n");

		return ret;
	}

	/* Construct a device name in DEVICE. */
	void sprint_device(int drive, int partition) {
		grub_sprintf(device, "(%cd%d",
				(drive & 0x80) ? 'h' : 'f',
				drive & ~0x80);
		if ((partition & 0xFF0000) != 0xFF0000) {
			char tmp[16];
			grub_sprintf(tmp, ",%d", (partition >> 16) & 0xFF);
			grub_strncat(device, tmp, 256);
		}
		if ((partition & 0x00FF00) != 0x00FF00) {
			char tmp[16];
			grub_sprintf(tmp, ",%c", 'a' + ((partition >> 8) & 0xFF));
			grub_strncat(device, tmp, 256);
		}
		grub_strncat(device, ")", 256);
	}

	int embed_stage1_5(char *stage1_5, int drive, int partition) {
		/* We install GRUB into the MBR, so try to embed the
		 * Stage 1.5 in the sectors right after the MBR. */
		sprint_device(drive, partition);
		grub_sprintf(cmd_arg, "%s %s", stage1_5, device);

		/* Notify what will be run. */
		grub_printf(" Running \"embed %s\"... ", cmd_arg);

		embed_func(cmd_arg, flags);
		if (! errnum) {
			/* Construct the blocklist representation. */
			grub_sprintf(buffer, "%s%s", device, embed_info);
			grub_printf("succeeded\n");
			return 1;
		} else {
			grub_printf("failed (this is not fatal)\n");
			return 0;
		}
	}

	struct stage1_5_map {
		char *fsys;
		char *name;
	};
	struct stage1_5_map stage1_5_map[] = {
		{"ext2fs", "/e2fs_stage1_5"}	
	};

	tmp_drive = saved_drive;
	tmp_partition = saved_partition;

	/* Check if the user specifies --force-lba. */
	while (1) {
		if (grub_memcmp("--force-lba", arg, sizeof("--force-lba") - 1) == 0) {
			is_force_lba = 1;
			arg = skip_to(0, arg);
		} else if (grub_memcmp("--prefix=", arg, sizeof("--prefix=") - 1) == 0) {
			prefix = arg + sizeof("--prefix=") - 1;
			arg = skip_to(0, arg);
			nul_terminate(prefix);
		}
#ifdef GRUB_UTIL
		else if (grub_memcmp("--stage2=", arg, sizeof("--stage2=") - 1) == 0) {
			stage2_arg = arg;
			arg = skip_to(0, arg);
			nul_terminate(stage2_arg);
		}
#endif /* GRUB_UTIL */
		else
		  break;
	}

	install_ptr = arg;
	image_ptr = skip_to(0, install_ptr);

	
	/* Make sure that INSTALL_PTR is valid. */
	set_device(install_ptr);
	if (errnum)
	  return 1;

	installed_drive = current_drive;
	installed_partition = current_partition;

	/* Mount the drive pointed by IMAGE_PTR */
	if (*image_ptr) {
		/* If the drive/partition where the images reside is specified,
		 * get the drive and the partition. */
		set_device(image_ptr);
		if (errnum)
		  return 1;
	} else {
		/* If omitted, use SAVED_PARTITION and SAVED_DRIVE. */
		current_drive = saved_drive;
		current_partition = saved_partition;
	}
	
	image_drive = saved_drive = current_drive;
	image_partition = saved_partition = current_partition;

	/* Open it. */
	if (! open_device())
	  goto fail;

	/* Check if stage1 exists. If the user doesn't specify the option
	 * `--prefix', attempt /boot/grub and /grub. */
	/* NOTE: It is dangerous to run this command without `--prefix' in the
	 * grub shell, since that affects `--stage2'. */
	if (! prefix) {
		prefix = "/boot/grub";
		grub_sprintf(stage1, "%s%s", prefix, "/stage1");
		if (! check_file(stage1)) {
			errnum = ERR_NONE;
			prefix = "/grub";
			grub_sprintf(stage1, "%s%s", prefix, "/stage1");
			if (! check_file(stage1))
			  goto fail;
		}
	} else {
		grub_sprintf(stage1, "%s%s", prefix, "/stage1");
		if (! check_file(stage1))
		  goto fail;
	}

	/* The prefix was determined. */
	grub_sprintf(stage2, "%s%s", prefix, "/stage2");
	grub_sprintf(config_filename, "%s%s", prefix, "/menu.lst");
	*real_config_filename = 0;

	/* Check if stage2 exists. */
	if (! check_file(stage2))
	  goto fail;

	{
		char *fsys = fsys_table[fsys_type].name;
		int i;
		int size = sizeof(stage1_5_map) / sizeof(stage1_5_map[0]);

		/* Iterate finding the same filesystem name as FSYS. */
		for (i = 0; i < size; i++) {
			if (grub_strcmp(fsys, stage1_5_map[i].fsys) == 0) {
				/* OK, check if the Stage 1.5 exists. */
				char stage1_5[64];

				grub_sprintf(stage1_5, "%s%s", prefix, stage1_5_map[i].name);
				if (check_file(stage1_5)) {
					if (embed_stage1_5(stage1_5, installed_drive, installed_partition) ||
							embed_stage1_5(stage1_5, image_drive, image_partition)) {
						grub_strcpy(real_config_filename, config_filename);
						sprint_device(image_drive, image_partition);
						grub_sprintf(config_filename, "%s%s", device, stage2);
						grub_strcpy(stage2, buffer);
					}
				}
				errnum = 0;
				break;
			}
		}
	}

	/* Construct a string that is used by the command "install" as its
	 * arguments. */
	sprint_device(installed_drive, installed_partition);
	
	/* Don't embed a drive number unnecessarily. */
	grub_sprintf(cmd_arg, "%s%s%s%s %s%s %s p %s %s",
				is_force_lba ? "--force-lba " : "",
				stage2_arg ? stage2_arg : "",
				stage2_arg ? " " : "",
				stage1,
				(installed_drive != image_drive) ? "d " : "",
				device,
				stage2,
				config_filename,
				real_config_filename);

	/* Notify what will be run. */
	grub_printf(" Running \"install %s\"... ", cmd_arg);

	/* Make sure that SAVED_DRIVE and SAVED_PARTITION are identical
	 * with IMAGE_DRIVE and IMAGE_PARTITION, respectively. */
	saved_drive = image_drive;
	saved_partition = image_partition;

	/* Run the command. */
	if (! install_func(cmd_arg, flags))
	  grub_printf("succeeded\nDone.\n");
	else
	  grub_printf("failed\n");

fail:
	saved_drive = tmp_drive;
	saved_partition = tmp_partition;
	return errnum;
}

static struct builtin builtin_setup = {
	"setup",
	setup_func,
	BUILTIN_CMDLINE | BUILTIN_HELP_LIST,
	"setup [--prefix=DIR] [--stage2=STAGE2_FILE] [--force-lba] INSTALL_DEVICE [IMAGE_DEVICE]",
	"Set up the installation of GRUB automatically. This command uses"
	" the more flexible command \"install\" in the backend and installs"
	" GRUB into the device INSTALL_DEVICE. If IMAGE_DEVICE is specified,"
	" then find the GRUB images in the device IMAGE_DEVICE, otherwise"
	" use the current \"root device\", which can be set by the command"
	" \"root\". If you know that your BIOS should support LBA but GRUB"
	" doesn't work in LBA mode, specify the option `--force-lba'."
	" If you install GRUB under the grub shell and you cannot unmount the"
	" partition where GRUB images reside, specify the option `--stage2'"
	" to tell GRUB the file name under your OS."
};


/* testvbe MODE
 * refer to: https://pdos.csail.mit.edu/6.828/2018/readings/hardware/vbe3.pdf */
static int testvbe_func(char *arg, int flags) {
	int mode_number;
	struct vbe_controller controller;
	struct vbe_mode mode;

	if (! *arg) {
		errnum = ERR_BAD_ARGUMENT;
		return 1;
	}

	if (! safe_parse_maxint(&arg, &mode_number))
	  return 1;

	/* Preset `VBE2'. */
	grub_memmove(controller.signature, "VBE2", 4);

	/* Detect VBE BIOS. */
	if (get_vbe_controller_info(&controller) != 0x004F) {
		grub_printf(" VBE BIOS is not present.\n");
		return 0;
	}

	if (controller.version < 0x0200) {
		grub_printf(" VBE version %d.%d is not supported.\n",
				(int) (controller.version >> 8),
				(int) (controller.version &0xFF));
		return 0;
	}

	if (get_vbe_mode_info(mode_number, &mode) != 0x004F ||
				(mode.mode_attributes & 0x0091) != 0x0091) {
		grub_printf(" Mode 0x%x is not supported.\n", mode_number);
		return 0;
	}

	/* Now trip to the graphics mode. */
	if (set_vbe_mode(mode_number | (1 << 14)) != 0x004F) {
		grub_printf(" Switching to Mode 0x%x failed.\n", mode_number);
		return 0;
	}

	/* Draw something on the screen... */
	{
		unsigned char *base_buf = (unsigned char *) mode.phys_base;
		int scanline = controller.version >= 0x0300 ? 
						mode.linear_bytes_per_scanline :
						mode.bytes_per_scanline;
		/* FIXME: this assumes that any depth is a modulo of 8. */
		int bpp = mode.bits_per_pixel / 8;
		int width = mode.x_resolution;
		int height = mode.y_resolution;
		int x, y;
		unsigned color = 0;

		/* Iterate drawing on the screen, until the user hits any key. */
		while (checkkey() == -1) {
			for (y = 0; y < height; y++) {
				unsigned char *line_buf = base_buf + scanline * y;

				for (x = 0; x < width; x++) {
					unsigned char *buf = line_buf + bpp * x;
					int i;

					for (i = 0; i < bpp; i++, buf++) {
						*buf = (color >> (i * 8)) & 0xff;
					}
				}

				color++;
			}
		}

		/* Discard the input. */
		getkey();
	}
	
	/* Back to the default text mode. */
	if (set_vbe_mode(0x03) != 0x004F) {
		/* Why?! */
		grub_reboot();
	}

	return 0;
}

static struct builtin builtin_testvbe = {
	"testvbe",
	testvbe_func,
	BUILTIN_CMDLINE | BUILTIN_HELP_LIST,
	"testvbe MODE",
	"Test the VBE mode MODE. Hit any key to return."
};


/* timeout */
static int timeout_func(char *arg, int flags) {
	if (! safe_parse_maxint(&arg, &grub_timeout))
	  return 1;

	return 0;
}

static struct builtin builtin_timeout = {
	"timeout",
	timeout_func,
	BUILTIN_MENU,
#if 0
	"timeout SEC",
	"Set a timeout, in SEC seconds, before automatically booting the"
	" default entry (normally the first entry defined)."
#endif
};


/* title */
static int title_func(char *arg, int flags) {
	/* This function is not actually used at least currently. */
	return 0;
}

static struct builtin builtin_title = {
	"title",
	title_func,
	BUILTIN_TITLE,
#if 0
	"title [NAME ...]",
	"Start a new boot entry, and set its name to the contents of the"
	" rest of the line, starting with the first non-space character."
#endif
};


/* uppermem */
static int uppermem_func(char *arg, int flags) {
	if (! safe_parse_maxint(&arg, (int *) &mbi.mem_upper))
	  return 1;

	mbi.flags &= ~MB_INFO_MEM_MAP;
	return 0;
}

static struct builtin builtin_uppermem = {
	"uppermem",
	uppermem_func,
	BUILTIN_CMDLINE | BUILTIN_HELP_LIST,
	"uppermem KBYTES",
	"Force GRUB to assume that only KBYTES kilobytes of upper memory are"
	" installed.  Any system address range maps are discarded."
};


/* vbeprobe */
static int vbeprobe_func(char *arg, int flags) {
	struct vbe_controller controller;
	unsigned short *mode_list;
	int mode_number = -1;

	auto unsigned long vbe_far_ptr_to_linear(unsigned long);
	
	unsigned long vbe_far_ptr_to_linear(unsigned long ptr) {
		unsigned short seg = (ptr >> 16);
		unsigned short off = (ptr & 0xFFFF);

		return (seg << 4) + off;
	}

	if (*arg) {
		if (! safe_parse_maxint(&arg, &mode_number)) 
		  return 1;
	}

	/* Set the signature to `VBE2', to obtain VBE 3.0 information. */
	grub_memmove(controller.signature, "VBE2", 4);

	if (get_vbe_controller_info(&controller) != 0x004F) {
		grub_printf(" VBE BIOS is not present.\n");
		return 0;
	}

	/* Check the version. */
	if (controller.version < 0x0200) {
		grub_printf(" VBE version %d.%d is not suppported.\n",
				(int) (controller.version >> 8),
				(int) (controller.version & 0xFF));
		return 0;
	}

	/* Print some information. */
	grub_printf(" VBE version %d.%d\n",
				(int) (controller.version >> 8),
				(int) (controller.version & 0xFF));

	/* Iterate probing modes. */
	for (mode_list = (unsigned short *) vbe_far_ptr_to_linear(controller.video_mode);
					*mode_list != 0xFFFF;
					mode_list++) {
		struct vbe_mode mode;

		if (get_vbe_mode_info(*mode_list, &mode) != 0x004F)
		  continue;

		/* Skip this, if this is not supported or linear frame buffer
		 * mode is not support. */
		if ((mode.mode_attributes & 0x0081) != 0x0081) 
		  continue;

		if (mode_number == -1 || mode_number == *mode_list) {
			char *model;

			switch (mode.memory_model) {
				case 0x00: model = "Text"; break;
				case 0x01: model = "CGA graphics"; break;
				case 0x02: model = "Hercules graphics"; break;
				case 0x03: model = "Planar"; break;
				case 0x04: model = "Packed pixel"; break;
				case 0x05: model = "Non-chain 4, 256 color"; break;
				case 0x06: model = "Direct Color"; break;
				case 0x07: model = "YUV"; break;
				default: model = "unknown"; break;
			}

			grub_printf("  0x%x: %s, %ux%ux%u\n",
					(unsigned) *mode_list,
					model,
					(unsigned) mode.x_resolution,
					(unsigned) mode.y_resolution,
					(unsigned) mode.bits_per_pixel);

			if (mode_number != -1)
			  break;
		}
	}

	if (mode_number != -1 && mode_number != *mode_list)
	  grub_printf("  Mode 0x%x is not found or supported.\n", mode_number);

	return 0;
}

static struct builtin builtin_vbeprobe = {
	"vbeprobe",
	vbeprobe_func,
	BUILTIN_CMDLINE | BUILTIN_HELP_LIST,
	"vbeprobe [MODE]",
	"Probe VBE information. If the mode number MODE is specified, show only"
	" the information about only the mode. "
};


/* The table of builtin commands. Sorted in dictionary order. */
struct builtin *builtin_table[] = {
	&builtin_blocklist,
	&builtin_boot,
	&builtin_cat,
	//&builtin_chainloader,
	//&builtin_cmp,
	//&builtin_color,
	&builtin_configfile,
	&builtin_debug,
	&builtin_default,
#ifdef GRUB_UTIL
	&builtin_device,
#endif /* GRUB_UTIL */
	&builtin_displayapm,
	&builtin_displaymem,
#ifdef GRUB_UTIL
//	&builtin_dump,
#endif /* GRUB_UTIL */
	&builtin_embed,
	&builtin_fallback,
	&builtin_find,
	&builtin_fstest,
	&builtin_geometry,
//	&builtin_halt,
	&builtin_help,
	&builtin_hiddenmenu,
//	&builtin_hide,
	&builtin_impsprobe,
	&builtin_initrd,
//	&builtin_install,
	&builtin_ioprobe,
	&builtin_kernel,
//	&builtin_lock,
//	&builtin_makeactive,
//	&builtin_map,
#ifdef USE_MD5_PASSWORDS
//	&builtin_md5crypt,
#endif /* USE_MD5_PASSWORDS */
//	&builtin_module,
//	&builtin_modulenounzip,
//	&builtin_pager,
//	&builtin_partnew,
//	&builtin_parttype,
//	&builtin_password,
//	&builtin_pause,
#ifdef GRUB_UTIL
	&builtin_quit,
#endif /* GRUB_UTIL */
//	&builtin_read,
	&builtin_reboot,
	&builtin_root,
//	&builtin_rootnoverify,
//	&builtin_savedefault,
#ifdef SUPPORT_SERIAL
//	&builtin_serial,
#endif /* SUPPORT_SERIAL */
//	&builtin_setkey,
	&builtin_setup,
#if defined(SUPPORT_SERIAL) || defined(SUPPORT_HERCULES)
//	&builtin_terminal,
#endif /* SUPPORT_SERIAL || SUPPORT_HERCULES */
#ifdef SUPPORT_SERIAL
//	&builtin_terminfo,
#endif /* SUPPORT_SERIAL */
//	&builtin_testload,
	&builtin_testvbe,
	&builtin_timeout,
	&builtin_title,
//	&builtin_unhide,
	&builtin_uppermem,
	&builtin_vbeprobe,
	0
};

