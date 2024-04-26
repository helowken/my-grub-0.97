/* disk_io.c - implement abstract BIOS disk input and output */
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


#include <shared.h>
#include <filesys.h>

#ifdef GRUB_UTIL
# include <device.h>
#endif /* GRUB_UTIL */

/* instrumentation variables */
void (*disk_read_hook)(int, int, int) = NULL;
void (*disk_read_func)(int, int, int) = NULL;

#ifndef STAGE1_5
int print_possibilities;

static int do_completion;
static int unique;
static char *unique_string;

#endif /* ! STAGE1_5 */

int fsmax;
struct fsys_entry fsys_table[NUM_FSYS + 1] = {
# ifdef FSYS_EXT2FS
	{"ext2fs", ext2fs_mount, ext2fs_read, ext2fs_dir, 0, 0},
# endif
	{0, 0, 0, 0, 0, 0}
};


/* These have the same format as "boot_drive" and "install_partition", but
 * are meant to be working values. */
unsigned long current_drive = GRUB_INVALID_DRIVE;
unsigned long current_partition;

#ifndef STAGE1_5
/* The register ESI should contain the address of the partition to be
 * used for loading a chain-loader when chain-loading the loader. */
unsigned long boot_part_addr = 0;
#endif /* ! STAGE1_5 */

/*
 * Global variables describing details of the filesystem
 */


/* filesystem type */
int fsys_type = NUM_FSYS;
#ifndef NO_BLOCK_FILES
static int block_file = 0;
#endif /* ! NO_BLOCK_FILES */

/* these are the translated numbers for the open partition */
unsigned long part_start;
unsigned long part_length;

int current_slice;

/* disk buffer parameters */
int buf_drive = -1;
int buf_track;
struct geometry buf_geom;

/* filesystem common variables */
int filepos;
int filemax;

static inline unsigned long log2i(unsigned long word) {
	asm volatile ("bsfl %1,%0"
			: "=r" (word)
			: "r" (word));
	return word;
}

int rawread(int drive, int sector, int byte_offset, int byte_len, char *buf) {
	int slen, sectors_per_vtrack;
	int sector_size_bits = log2i(buf_geom.sector_size);
	
	if (byte_len <= 0)
	  return 1;

	while (byte_len > 0 && ! errnum) {
		int soff, num_sect, track, size = byte_len;
		char *bufaddr;

		/* 
		 * Check track buffer.  If it isn't valid or it is from the
		 * wrong disk, then reset the disk geometry.
		 */
		if (buf_drive != drive) {
			if (get_diskinfo(drive, &buf_geom)) {
				errnum = ERR_NO_DISK;
				return 0;
			}
			buf_drive = drive;
			buf_track = -1;
			sector_size_bits = log2i(buf_geom.sector_size);
		}

		/* Make sure that SECTOR is valid. */
		if (sector < 0 || sector >= buf_geom.total_sectors) {
			errnum = ERR_GEOM;
			return 0;
		}

		slen = ((byte_offset + byte_len + buf_geom.sector_size - 1) >> sector_size_bits);

		/* Eliminate a buffer overflow. */
		if ((buf_geom.sectors << sector_size_bits) > BUFFERLEN)
		  sectors_per_vtrack = (BUFFERLEN >> sector_size_bits);
		else
		  sectors_per_vtrack = buf_geom.sectors;

		/* Get the first sector of track. */
		soff = sector % sectors_per_vtrack;
		track = sector - soff;
		num_sect = sectors_per_vtrack - soff;
		bufaddr = ((char *) BUFFERADDR + (soff << sector_size_bits) + byte_offset);

		if (track != buf_track) {
			int bios_err, read_start = track, read_len = sectors_per_vtrack;

			/* 
			 * If there's more than one read in this entire loop, then
			 * only make the earlier reads for the portion needed.  This
			 * saves filling the buffer with data that won't be used!
			 */
			if (slen > num_sect) {
				read_start = sector;
				read_len = num_sect;
				bufaddr = (char *) BUFFERADDR + byte_offset;
			}

			bios_err = biosdisk(BIOSDISK_READ, drive, &buf_geom,
						read_start, read_len, BUFFERSEG);
			if (bios_err) {
				buf_track = -1;

				if (bios_err == BIOSDISK_ERROR_GEOMETRY)
				  errnum = ERR_GEOM;
				else {
					/*
					 * If there was an error, try to load only the
					 * required sector(s) rather than failing completely.
					 */
					if (slen > num_sect ||
								biosdisk(BIOSDISK_READ, drive, &buf_geom,
									sector, slen, BUFFERSEG))
					  errnum = ERR_READ;

					bufaddr = (char *) BUFFERADDR + byte_offset;
				}
			} else {
				buf_track = track;
			}

			/* TODO EZD is not supported now */
		}

		if (size > ((num_sect << sector_size_bits) - byte_offset))
		  size = (num_sect << sector_size_bits) - byte_offset;

		/*
		 * Instrumentation to tell which sectors were read and used.
		 */
		if (disk_read_func) {
			int sector_num = sector;
			int length = buf_geom.sector_size - byte_offset;
			if (length > size)
			  length = size;
			(*disk_read_func)(sector_num++, byte_offset, length);
			length = size - length;
			if (length > 0) {
				while (length > buf_geom.sector_size) {
					(*disk_read_func)(sector_num++, 0, buf_geom.sector_size);
					length -= buf_geom.sector_size;
				}
				(*disk_read_func)(sector_num, 0, length);
			}

		}

		grub_memmove(buf, bufaddr, size);

		buf += size;
		byte_len -= size;
		sector += num_sect;
		byte_offset = 0;
	}

	return (! errnum);
}


int devread(int sector, int byte_offset, int byte_len, char *buf) {
	/* 
	 * Check partition boundaries
	 */
	if (sector < 0 || 
			((sector + ((byte_offset + byte_len - 1) >> SECTOR_BITS)) >= part_length)) {
		errnum = ERR_OUTSIDE_PART;
		return 0;
	}

	/* 
	 * Get the read to the beginning of a partition.
	 */
	sector += byte_offset >> SECTOR_BITS;
	byte_offset &= SECTOR_SIZE - 1;

#if !defined(STAGE1_5)
	if (disk_read_hook && debug)
	  printf("<%d, %d, %d>", sector, byte_offset, byte_len);
#endif /* !STAGE1_5 */

	/* 
	 * Call RAWREAD, which is very similar, but:
	 *
	 *   --  It takes an extra parameter, the drive number.
	 *   --  It requires that "sector" is relative to the beginning
	 *           of the disk.
	 *   --  It doesn't handle offsets of more than 511 bytes into the
	 *           sector.
	 */
	return rawread(current_drive, part_start + sector, byte_offset,
				byte_len, buf);
}


#ifndef STAGE1_5
int rawwrite(int drive, int sector, char *buf) {
	if (sector == 0) {
		if (biosdisk(BIOSDISK_READ, drive, &buf_geom, 0, 1, SCRATCHSEG)) {
			errnum = ERR_WRITE;
			return 0;
		}

		if (PC_SLICE_TYPE(SCRATCHADDR, 0) == PC_SLICE_TYPE_EZD ||
				PC_SLICE_TYPE(SCRATCHADDR, 1) == PC_SLICE_TYPE_EZD ||
				PC_SLICE_TYPE(SCRATCHADDR, 2) == PC_SLICE_TYPE_EZD ||
				PC_SLICE_TYPE(SCRATCHADDR, 3) == PC_SLICE_TYPE_EZD)
		  sector = 1;
	}

	memmove((char *) SCRATCHADDR, buf, SECTOR_SIZE);
	if (biosdisk(BIOSDISK_WRITE, drive, &buf_geom, sector, 1, SCRATCHSEG)) {
		errnum = ERR_WRITE;
		return 0;
	}

	if (sector - sector % buf_geom.sectors == buf_track) {
		/* Clear the cache. */
		buf_track = -1;
	}

	return 1;
}

int devwrite(int sector, int sector_count, char *buf) {
#if defined(GRUB_UTIL) && defined(__linux__)
	if (current_partition != 0xFFFFFF &&
				is_disk_device(device_map, current_drive)) {
		/* If the grub shell is running under Linux and the user wants to
		 * embed a Stage 1.5 into a partition instead of a MBR, use system
		 * calls directly instead of biosdisk, because of the bug in
		 * Linux, "sigh" */
		return write_to_partition(device_map, current_drive, current_partition,
					sector, sector_count, buf);
	} 
	else
#endif /* GRUB_UTIL* && __linux__ */
	{
		int i;

		for (i = 0; i < sector_count; i++) {
			if (! rawwrite(current_drive, part_start + sector + i,
						buf + (i << SECTOR_BITS)))
			  return 0;
		}
		return 1;
	}
}

static int sane_partition() {
	/* network drive */
	if (current_drive == NETWORK_DRIVE)
	  return 1;

	if (!(current_partition & 0xFF000000uL) &&
			((current_drive & 0xFFFFFF7F) < 8 || current_drive == cdrom_drive) &&
			(current_partition & 0xFF) == 0xFF &&
			((current_partition & 0xFF00) == 0xFF00 || (current_partition & 0xFF00) < 0x800) &&
			((current_partition >> 16) == 0xFF || (current_drive & 0x80)))
	  return 1;

	errnum = ERR_DEV_VALUES;
	return 0;
}
#endif /* ! STAGE1_5 */


/* Read the SUPERBLOCK to buffer.
 */
static void attempt_mount() {
#ifndef STAGE1_5
	for (fsys_type = 0; fsys_type < NUM_FSYS; fsys_type++) {
		if ((fsys_table[fsys_type].mount_func)())
		  break;
	}

	if (fsys_type == NUM_FSYS && errnum == ERR_NONE)
	  errnum = ERR_FSYS_MOUNT;
#else
	fsys_type = 0;
	if ((*(fsys_table[fsys_type].mount_func))() != 1) {
		fsys_type = NUM_FSYS;
		errnum = ERR_FSYS_MOUNT;
	}
#endif /* ! STAGE1_5 */
}


#ifndef STAGE1_5
static void check_and_print_mount() {
	attempt_mount();
	if (errnum == ERR_FSYS_MOUNT)
	  errnum = ERR_NONE;
	if (! errnum)
	  print_fsys_type();
	print_error();
}
#endif /* ! STAGE1_5 */


/* Get the information on next partition on the drive DRIVE.
 * The caller must no modify the contents of the arguments when
 * iterating this function. The partition representation in GRUB will
 * be stored in *PARTITION. Likewise, the partition type in *TYPE, the
 * start sector in *START, the length in *LEN, the offset of the
 * partition table in *OFFSET, the entry enumber in the table in *ENTRY,
 * the offset of the extended partition in *EXT_OFFSET.
 * BUF is used to store a MBR, the boot sector of a partition, or
 * a BSD label sector, and it must be at least 512 bytes length.
 * When calling this function first, *PARTITION must be initialized to
 * 0xFFFFFF. The return value is zero if fails, otherwise non-zero. */
int next_partition(unsigned long drive, unsigned long dest,
			unsigned long *partition, int *type,
			unsigned long *start, unsigned long *len,
			unsigned long *offset, int *entry,
			unsigned long *ext_offset, char *buf) {
	/* Forward declarations. */
	auto int next_pc_slice(void);

	/* Get next PC slice. Be careful of that this function may return
	 * an empty PC slice (i.e. a partition whose type is zero) as well. */
	int next_pc_slice(void) {
		int pc_slice_no = (*partition & 0xFF0000) >> 16;

		/* If this is the first time... */
		if (pc_slice_no == 0xFF) {
			*offset = 0;
			*ext_offset = 0;
			*entry = -1;
			pc_slice_no = -1;
		}

		/* Read the MBR or the boot sector of the extended partition. */
		if (! rawread(drive, *offset, 0, SECTOR_SIZE, buf))
		  return 0;

		/* Check if it is valid. */
		if (! PC_MBR_CHECK_SIG(buf)) {
			errnum = ERR_BAD_PART_TABLE;
			return 0;
		}

		/* Increase the entry number. */
		(*entry)++;

		/* If this is out of current partition table... */
		if (*entry == PC_SLICE_MAX) {
			int i;

			/* Search the first extended partition in current table. */
			for (i = 0; i < PC_SLICE_MAX; i++) {
				if (IS_PC_SLICE_TYPE_EXTENDED(PC_SLICE_TYPE(buf, i))) {
					/* Found. Set the new offset and the entry number,
					 * and restart this function. */
					*offset = *ext_offset + PC_SLICE_START(buf, i);
					if (! *ext_offset)
					  *ext_offset = *offset;
					*entry = -1;
					return next_pc_slice();
				}
			}

			errnum = ERR_NO_PART;
			return 0;
		}

		*type = PC_SLICE_TYPE(buf, *entry);
		*start = *offset + PC_SLICE_START(buf, *entry);
		*len = PC_SLICE_LENGTH(buf, *entry);

		/* The calculation of a PC slice number is complicated, because of
		 * the rather odd definition of extended partitions. Even worse,
		 * there is no guarantee that this is consistent with every
		 * operating systems. Uggh. */
		if (pc_slice_no < PC_SLICE_MAX ||
					(! IS_PC_SLICE_TYPE_EXTENDED(*type) && *type != PC_SLICE_TYPE_NONE))
		  pc_slice_no++;

		*partition = (pc_slice_no << 16) | 0xFFFF;
		return 1;
	}

	/* Start the body of this function. */

#ifndef STAGE1_5
	if (current_drive == NETWORK_DRIVE)
	  return 0;
#endif /* ! STAGE1_5 */

	// NO_SUPPORT: BSD partitions
	
	return next_pc_slice();
}


/* Open a partition.
 * Setup CURRENT_SLICE, PART_START, PART_LENGTH.
 */
int real_open_partition(int flags) {
	unsigned long dest_partition = current_partition;
	unsigned long part_offset;
	unsigned long ext_offset;
	int entry;
	char buf[SECTOR_SIZE];
	int bsd_part, pc_slice;

	/* For simplicity. */
	auto int next(void);
	auto int next(void) {
		int ret = next_partition(current_drive, dest_partition,
					&current_partition, &current_slice,
					&part_start, &part_length,
					&part_offset, &entry, &ext_offset, buf);
		bsd_part = (current_partition >> 8) & 0xFF;
		pc_slice = current_partition >> 16;
		return ret;
	}

#ifndef STAGE1_5
	/* network drive */
	if (current_drive == NETWORK_DRIVE)
	  return 1;

	if (! sane_partition())
	  return 0;
#endif /* ! STAGE1_5 */

	current_slice = 0;
	part_start = 0;

	/* Make sure that buf_geom is valid. */
	if (buf_drive != current_drive) {
		if (get_diskinfo(current_drive, &buf_geom)) {
			errnum = ERR_NO_DISK;
			return 0;
		}
		buf_drive = current_drive;
		buf_track = -1;
	}
	part_length = buf_geom.total_sectors;

	/* If this is the whole disk, return here. */
	if (! flags && current_partition == 0xFFFFFF)
	  return 1;

	if (flags)
	  dest_partition = 0xFFFFFF;

	/* Initialize CURRENT_PARTITION for next_partition. */
	current_partition = 0xFFFFFF;

	while (next()) {
		/* If this is a valid partition... */
		if (current_slice) {
#ifndef STAGE1_5
			/* Display partition information. */
			if (flags && ! IS_PC_SLICE_TYPE_EXTENDED(current_slice)) {
				if (! do_completion) {
					if (current_drive & 0x80)
					  grub_printf("   Partition num: %d, ",
								  current_partition >> 16);

					if (! IS_PC_SLICE_TYPE_BSD(current_slice))
					  check_and_print_mount();
					else {
						/* TODO BSD is not supported now */
					}
				} else {
					if (bsd_part != 0xFF) {
						char str[16];

						if (! (current_drive & 0x80) ||
									(dest_partition >> 16) == pc_slice)
						  grub_sprintf(str, "%c)", bsd_part + 'a');
						else
						  grub_printf(str, "%d,%c)", pc_slice, bsd_part + 'a');
						print_a_completion(str);
					} else if (! IS_PC_SLICE_TYPE_BSD(current_slice)) {
						char str[8];

						grub_sprintf(str, "%d)", pc_slice);
						print_a_completion(str);
					}
				}
			}
			
			errnum = ERR_NONE;
#endif /* ! STAGE1_5 */
			
			/* Check if this is the destination partition. */
			if (! flags &&
						(dest_partition == current_partition || 
						 ((dest_partition >> 16) == 0xFF && ((dest_partition >> 8) & 0xFF) == bsd_part)))
			  return 1;
		}
	}

#ifndef STAGE1_5
	if (flags) {
		if (! (current_drive & 0x80)) {
			current_partition = 0xFFFFFF;
			check_and_print_mount();
		}

		errnum = ERR_NONE;
		return 1;
	}
#endif /* ! STAGE1_5 */
	
	return 0;
}

int open_partition() {
	return real_open_partition(0);
}

#ifndef STAGE1_5
/* XX used for device completion in 'set_device' and 'print_completions' */
static int incomplete, disk_choice;
static enum {
	PART_UNSPECIFIED = 0,
	PART_DISK,
	PART_CHOSEN
} part_choice;
#endif /* ! STAGE1_5 */


/* Given (hd0), (hd0,0) or (hd0,0)/grub/menu.lst, setup the CURRENT_DIRVE 
 * and CURRENT_PARTITION.
 * If device is (hd0), then CURRENT_PARTITION is 0xFFFFFF.b
 */
char *set_device(char *device) {
#ifdef STAGE1_5
	/* In Stage 1.5, the first 4 bytes of FILENAME has a device number. */
	unsigned long dev = *((unsigned long *) device);
	int drive = (dev >> 24) & 0xFF;
	int partition = dev & 0xFFFFFF;

	/* If DRIVE is disabled, use SAVED_DRIVE instead. */
	if (drive == GRUB_INVALID_DRIVE)
	  current_drive = saved_drive;
	else
	  current_drive = drive;

	/* The `partition' part must always have a valid number. */
	current_partition = partition;

	return device + sizeof(unsigned long);

#else /* ! STAGE1_5 */

	int result = 0;

	incomplete = 0;
	disk_choice = 1;
	part_choice = PART_UNSPECIFIED;
	current_drive = saved_drive;
	current_partition = 0xFFFFFF;

	if (*device == '(' && !*(device + 1)) {
		/* user has given '(' only, let disk_choice handle what disks we have */
		return device + 1;
	}
	
	if (*device == '(' && *(++device)) {
		if (*device != ',' && *device != ')') {
			char ch = *device;

			if (*device == 'f' || *device == 'h' ||
					(*device == 'c' && cdrom_drive != GRUB_INVALID_DRIVE)) {
				/* user has given '([fhn]', check for resp, add 'd' and
				 * let disk_choice handle what disks we have */
				if (!*(device + 1)) {
					device++;
					*device++ = 'd';
					*device = '\0';
					return device;
				} else if (*(device + 1) == 'd' && !*(device + 2))
				  return device + 2;
			}

			if ((*device == 'f' || *device == 'h' ||
						(*device == 'c' && cdrom_drive != GRUB_INVALID_DRIVE)) &&
					(device += 2, (*(device - 1) != 'd')))
			  errnum = ERR_NUMBER_PARSING;

			if (ch == 'c' && cdrom_drive != GRUB_INVALID_DRIVE)
			  current_drive = cdrom_drive;
			else {
				safe_parse_maxint(&device, (int *) (void *) &current_drive);

				disk_choice = 0;
				if (ch == 'h')
				  current_drive += 0x80;
			}
		}

		if (errnum)
		  return 0;

		if (*device == ')') {
			part_choice = PART_CHOSEN;
			result = 1;
		} else if (*device == ',') {
			/* Either an absolute PC or BSD partition. */
			disk_choice = 0;
			part_choice++;
			device++;

			if (*device >= '0' && *device <= '9') {
				part_choice++;
				current_partition = 0;

				if (!(current_drive & 0x80) ||
						!safe_parse_maxint(&device, (int *) (void *) &current_partition) ||
						current_partition > 254) {
					errnum = ERR_DEV_FORMAT;
					return 0;
				}

				current_partition = (current_partition << 16) + 0xFFFF;

				if (*device == ',')
				  device++;

				if (*device >= 'a' && *device <= 'h') {
					current_partition = (((*(device++) - 'a') << 8) | 
								(current_partition & 0xFF00FF));
				}
			} else if (*device >= 'a' && *device <= 'h') {
				part_choice++;
				current_partition = ((*(device++) - 'a') << 8) | 0xFF00FF;
			}

			if (*device == ')') {
				if (part_choice == PART_DISK) {
					current_partition = saved_partition;
					part_choice++;
				}
				
				result = 1;
			}
		}
	}

	if (! sane_partition())
	  return 0;

	if (result)
	  return device + 1;
	else {
		if (! *device)
		  incomplete = 1;
		errnum = ERR_DEV_FORMAT;
	}

	return 0;
#endif /* STAGE1_5 */
}

/*
 * This performs a "mount" on the current device, both drive and partition
 * number.
 *
 * CURRENT_DRIVE and CURRENT_PARTITION should be set before.
 * attempt_mount() is used to read the SUPERBLOCK into buffer.
 */
int open_device() {
	if (open_partition())
	  attempt_mount();

	if (errnum != ERR_NONE)
	  return 0;

	return 1;
}


static char *setup_part(char *filename) {
#ifdef STAGE1_5

	if (! (filename = set_device(filename))) {
		current_drive = GRUB_INVALID_DRIVE;
		return 0;
	}

# ifndef NO_BLOCK_FILES
	if (*filename != '/')
	  open_partition();
	else
# endif /* ! NO_BLOCK_FILES */
	  open_device();

#else /* ! STAGE1_5 */

	if (*filename == '(') {
		if ((filename = set_device(filename)) == 0) {
			current_drive = GRUB_INVALID_DRIVE;
			return 0;
		}
# ifndef NO_BLOCK_FILES
		if (*filename != '/')
		  open_partition();
		else
# endif /* ! NO_BLOCK_FILES */
		  open_device();

	} else if (saved_drive != current_drive ||
				saved_partition != current_partition ||
				(*filename == '/' && fsys_type == NUM_FSYS) ||
				buf_drive == -1) {
		current_drive = saved_drive;
		current_partition = saved_partition;
		/* allow for the error case of "no filesystem" after the partition
		 * is found.  This makes block files work find on no filesystem */
# ifndef NO_BLOCK_FILES
		if (*filename != '/')
		  open_partition();
		else
# endif /* ! NO_BLOCK_FILES */
		  open_device();
	}

#endif /* ! STAGE1_5 */

	if (errnum && (*filename == '/' || errnum != ERR_FSYS_MOUNT))
	  return 0;
	else
	  errnum = ERR_NONE;

#ifndef STAGE1_5
	if (!sane_partition())
	  return 0;
#endif /* ! STAGE1_5 */
	
	return filename;
}


#ifndef STAGE1_5
/* 
 * This prints the filesystem type or gives relevant information.
 */

void print_fsys_type() {
	if (! do_completion) {
		printf(" Filesystem type ");

		if (fsys_type != NUM_FSYS)
		  printf("is %s, ", fsys_table[fsys_type].name);
		else
		  printf("unknown, ");

		if (current_partition == 0xFFFFFF)
		  printf("using whole disk\n");
		else
		  printf("partition type 0x%x\n", current_slice & 0xFF);
	}
}
#endif /* ! STAGE1_5 */


#ifndef STAGE1_5
/* If DO_COMPLETION is true, just print NAME. Otherwise save the unique
 * part into UNIQUE_STRING. */
void print_a_completion(char *name) {
	/* If NAME is "." or "..", do not count it. */
	if (grub_strcmp(name, ".") == 0 || grub_strcmp(name, "..") == 0)
	  return;

	if (do_completion) {
		char *buf = unique_string;

		if (! unique) {
			while ((*buf++ = *name++)) {
			}
		} else {
			while (*buf && (*buf == *name)) {
				buf++;
				name++;
			}
			/* mismatch, strip it. */
			*buf = '\0';
		}
	} else
	  grub_printf(" %s", name);

	unique++;
}


/*
 * This lists the possible completions of a device string, filename, or
 * any same combination of the two.
 */

int print_completions(int is_filename, int is_completion) {
	char *buf = (char *) COMPLETION_BUF;
	char *ptr = buf;

	unique_string = (char *) UNIQUE_BUF;
	*unique_string = 0;
	unique = 0;
	do_completion = is_completion;

	if (! is_filename) {
		/* Print the completions of builtin commands. */
		struct builtin **builtin;

		if (! is_completion)
		  grub_printf(" Possible commands are:");

		for (builtin = builtin_table; (*builtin); builtin++) {
			/* If *BUILTIN cannot be run in the command-line, skip it. */
			if (! ((*builtin)->flags & BUILTIN_CMDLINE))
			  continue;

			if (substring(buf, (*builtin)->name) <= 0)
			  print_a_completion((*builtin)->name);
		}

		if (is_completion && *unique_string) {
			if (unique == 1) {
				char *u = unique_string + grub_strlen(unique_string);

				*u++ = ' ';
				*u = 0;
			}
			grub_strcpy(buf, unique_string);
		}

		if (! is_completion)
		  grub_putchar('\n');

		print_error();
		do_completion = 0;
		if (errnum)
		  return -1;
		else
		  return unique - 1;
	}

	if (*buf == '/' || (ptr = set_device(buf)) || incomplete) {
		errnum = 0;

		if (*buf == '(' && (incomplete || ! *ptr)) {
			if (! part_choice) {
				/* disk completions */
				int disk_no, i, j;
				struct geometry geom;

				if (! is_completion)
				  grub_printf(" Possible disks are: ");

				if (! ptr || *(ptr - 1) != 'd' || *(ptr - 2) != 'c') {
					for (i = (ptr && (*(ptr - 1) == 'd' && *(ptr - 2) == 'h') ? 1 : 0);
							 i < (ptr && (*(ptr - 1) == 'd' && *(ptr - 2) == 'f') ? 1 : 2);
							 i++) {
						for (j = 0; j < 8; j++) {
							disk_no = (i * 0x80) + j;
							if ((disk_choice || disk_no == current_drive) &&
										! get_diskinfo(disk_no, &geom)) {
								char dev_name[8];

								grub_sprintf(dev_name, "%cd%d", i ? 'h' : 'f', j);
								print_a_completion(dev_name);
							}
						}
					}
				}

				if (cdrom_drive != GRUB_INVALID_DRIVE && 
							(disk_choice || cdrom_drive == current_drive) &&
							(! ptr || *(ptr - 1) == '(' || 
							 (*(ptr - 1) == 'd' && *(ptr - 2) == 'c')))
				  print_a_completion("cd");

				if (is_completion && *unique_string) {
					ptr = buf;
					while (*ptr != '(') {
						ptr--;
					}
					ptr++;
					grub_strcpy(ptr, unique_string);
					if (unique == 1) {
						ptr += grub_strlen(ptr);
						if (*unique_string == 'h') {
							*ptr++ = ',';
							*ptr = 0;
						} else {
							*ptr++ = ')';
							*ptr = 0;
						}
					}
				}

				if (! is_completion)
				  grub_putchar('\n');
			} else {
				/* partition completions */
				if (part_choice == PART_CHOSEN && 
							open_partition() &&
							! IS_PC_SLICE_TYPE_BSD(current_slice)) {
					unique = 1;
					ptr = buf + grub_strlen(buf);
					if (*(ptr - 1) != ')') {
						*ptr++ = ')';
						*ptr = 0;
					}
				} else {
					if (! is_completion)
					  grub_printf(" Possible partitions are:\n");
					real_open_partition(1);
					if (is_completion && *unique_string) {
						ptr = buf;
						while (*ptr++ != ',') {
						}
						grub_strcpy(ptr, unique_string);
					}
				}
			}
		} else if (ptr && *ptr == '/') {
			/* filename completions */
			if (! is_completion)
			  grub_printf(" Possible files are:");

			dir(buf);

			if (is_completion && *unique_string) {
				ptr += grub_strlen(ptr);
				while (*ptr != '/') {
					ptr--;
				}
				ptr++;

				grub_strcpy(ptr, unique_string);

				if (unique == 1) {
					ptr += grub_strlen(unique_string);

					/* Check if the file UNIQUE_STRING is a directory. */
					*ptr = '/';
					*(ptr + 1) = 0;

					dir(buf);

					/* Restore the original unique value. */
					unique = 1;

					if (errnum) {
						/* Regular file */
						errnum = 0;
						*ptr = ' ';
						*(ptr + 1) = 0;
					}
				}
			}

			if (! is_completion)
			  grub_putchar('\n');
		} else {
			errnum = ERR_BAD_FILENAME;
		}
	}

	print_error();
	do_completion = 0;
	if (errnum)
	  return -1;
	else
	  return unique - 1;
}
#endif /* ! STAGE1_5 */


/*
 * This is the generic file open function. 
 * If the filename is "(hd0)1+16,20+5", set BLOCK_FILE to 1 and parse
 * the ranges(1+16,20+5) to BLK records.
 */
int grub_open(char *filename) {
	/* if any "dir" function uses/sets filepos, it must
	 * set it to zero before retruning if opening a file! */
	filepos = 0;

	if (!(filename = setup_part(filename)))
	  return 0;

#ifndef NO_BLOCK_FILES
	block_file = 0;
#endif /* ! NO_BLOCK_FILES */

	/* This accounts for partial filesystem implementations. */
	fsmax = MAXINT;

	if (*filename != '/') {
#ifndef NO_BLOCK_FILES
		char *ptr = filename;
		int tmp, list_addr = BLK_BLKLIST_START;
		filemax = 0;

		/* Parse ranges to BLK records.
		 * For example, 1+16,20+5, will be:
		 *  [BLK_BLKSTART(l)=1, BLK_BLKLENGTH(l)=16],
		 *  [BLK_BLKSTART(l)=20, BLK_BLKLENGTH(l)=5]
		 *
		 * Since each record has (start, length), its size is
		 * BLK_BLKLIST_INC_VAL(8).
		 */
		while (list_addr < BLK_MAX_ADDR) {
			tmp = 0;
			safe_parse_maxint(&ptr, &tmp);
			errnum = 0;

			if (*ptr != '+') {
				if ((*ptr && *ptr != '/' && !isspace(*ptr)) ||
							tmp == 0 || tmp > filemax) 
				  errnum = ERR_BAD_FILENAME;
				else
				  filemax = tmp;

				break;
			}

			/* Since we use the same filesystem buffer, mark it to
			 * be remounted */
			fsys_type = NUM_FSYS;

			BLK_BLKSTART(list_addr) = tmp;
			ptr++;
			
			if (!safe_parse_maxint(&ptr, &tmp) ||
						tmp == 0 ||
						(*ptr && *ptr != ',' && *ptr != '/' && !isspace(*ptr))) {
				errnum = ERR_BAD_FILENAME;
				break;
			}

			BLK_BLKLENGTH(list_addr) = tmp;
			
			filemax += (tmp * SECTOR_SIZE);
			list_addr += BLK_BLKLIST_INC_VAL;

			if (*ptr != ',')
			  break;

			ptr++;
		}

		if (list_addr < BLK_MAX_ADDR && ptr != filename && !errnum) {
			block_file = 1;
			BLK_CUR_FILEPOS = 0;
			BLK_CUR_BLKLIST = BLK_BLKLIST_START;
			BLK_CUR_BLKNUM = 0;
			return 1;
		}
#else /* NO_BLOCK_FILES */
		errnum = ERR_BAD_FILENAME;
#endif /* ! NO_BLOCK_FILES */
	}

	if (!errnum && fsys_type == NUM_FSYS)
	  errnum = ERR_FSYS_MOUNT;

#ifndef STAGE1_5
	/* set "dir" function to open a file */
	print_possibilities = 0;
#endif

	if (!errnum && (*(fsys_table[fsys_type].dir_func))(filename)) {
		return 1;
	}

	return 0;
}

/* If BLOCK_FILE is 1, use BLK records to calculate the position
 * for reading.
 */
int grub_read(char *buf, int len) {
	/* Make sure "filepos" is a sane value */
	if ((filepos < 0) || (filepos > filemax))
	  filepos = filemax;

	/* Make sure "len" is a sane value */
	if ((len < 0) || (len > (filemax - filepos)))
	  len = filemax - filepos;

	/* if target file position is past the end of
	 * the supported/configured filesize, then
	 * there is an error */
	if (filepos + len > fsmax) {
		errnum = ERR_FILELENGTH;
		return 0;
	}

#ifndef NO_BLOCK_FILES
	if (block_file) {
		int size, off, ret = 0;

		while (len && ! errnum) {
			/* we may need to look for the right block in the list(s) */
			if (filepos < BLK_CUR_FILEPOS) {
				BLK_CUR_FILEPOS = 0;
				BLK_CUR_BLKLIST = BLK_BLKLIST_START;
				BLK_CUR_BLKNUM = 0;
			}

			/* run BLK_CUR_FILEPOS up to filepos.
			 * For example, "(hd0)1+16,20+5", 
			 * if BLK_CUR_FILEPOS = 1, BLK_CUR_BLKNUM >= 16,
			 * then set BLK_CUR_FILEPOS = 20, BLK_CUR_BLKNUM = 0
			 */
			while (filepos > BLK_CUR_FILEPOS) {
				if ((filepos - (BLK_CUR_FILEPOS & ~(SECTOR_SIZE -1))) >= SECTOR_SIZE) {
					BLK_CUR_FILEPOS += SECTOR_SIZE;
					BLK_CUR_BLKNUM++;

					if (BLK_CUR_BLKNUM >= BLK_BLKLENGTH (BLK_CUR_BLKLIST)) {
						BLK_CUR_BLKLIST += BLK_BLKLIST_INC_VAL;	
						BLK_CUR_BLKNUM = 0;
					}
				} else {
					BLK_CUR_FILEPOS = filepos;
				}
			}

			off = filepos & (SECTOR_SIZE - 1);
			size = ((BLK_BLKLENGTH (BLK_CUR_BLKLIST) - BLK_CUR_BLKNUM) * SECTOR_SIZE) - off;
			if (size > len)
			  size = len;

			disk_read_func = disk_read_hook;

			/* read current block and put it in the right place in memory */
			devread(BLK_BLKSTART (BLK_CUR_BLKLIST) + BLK_CUR_BLKNUM, off, size, buf);

			disk_read_func = NULL;

			len -= size;
			filepos += size;
			ret += size;
			buf += size;
		}

		if (errnum)
		  ret = 0;

		return ret;
	}
#endif /* ! NO_BLOCK_FILES */

	if (fsys_type == NUM_FSYS) {
		errnum = ERR_FSYS_MOUNT;
		return 0;
	}

	return (*(fsys_table[fsys_type].read_func)) (buf, len);
}


#ifndef STAGE1_5
/* Reposition a file offset. */
int grub_seek(int offset) {
	if (offset > filemax || offset < 0)
	  return -1;

	filepos = offset;
	return offset;
}

int dir(char *dirname) {
	if (!(dirname = setup_part(dirname)))
	  return 0;

	if (*dirname != '/')
	  errnum = ERR_BAD_FILENAME;

	if (fsys_type == NUM_FSYS)
	  errnum = ERR_FSYS_MOUNT;

	if (errnum)
	  return 0;

	/* set "dir" function to list completions */
	print_possibilities = 1;

	return (*(fsys_table[fsys_type].dir_func))(dirname);
}
#endif /* ! STAGE1_5 */

void grub_close() {
#ifndef NO_BLOCK_FILES
	if (block_file)
	  return;
#endif /* ! NO_BLOCK_FILES */

	if (fsys_table[fsys_type].close_func != 0)
	  (*(fsys_table[fsys_type].close_func))();
}


