/* device.c - Some helper functions for OS devices and BIOS drives */
/*
 *  GRUB  --  GRand Unified Bootloader
 *  Copyright (C) 1999,2000,2001,2002,2003,2004,2005  Free Software Foundation, Inc.
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

/* Try to use glibc's transparent LFS support. */
#define _LARGEFILE_SOURCE		1
/* lseek becomes synonymous with lseek64. */
#define _FILE_OFFSET_BITS		64

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <assert.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <errno.h>
#include <limits.h>
#include <stdarg.h>
#include <linux/loop.h>

#ifdef __linux__
# if !defined(__GLIBC__) || \
		((__GLIBC__ < 2) || ((__GLIBC__ == 2) && (__GLIBC_MINOR__ < 1)))
/* Maybe libc doesn't have large file support. */
#  include <linux/unistd.h>		/* _llseek */
# endif /* (__GLIBC__ < 2) || ((__GLIBC__ == 2) && (__GLIBC_MINOR__ < 1)) */
# include <sys/ioctl.h>		/* ioctl. */
# ifndef HDIO_GETGEO
#  define HDIO_GETGEO	0x0301	/* get device geometry */
/* If HDIO_GETGEO is not defined, it is unlikely that hd_geometry is
 * defined. */
struct hd_geometry {
	unsigned char heads;
	unsigned char sectors;
	unsigned short cylinders;
	unsigned long start;
};
# endif	/* ! HDIO_GETGEO */
# ifndef FLOPPY_MAJOR
#  define FLOPPY_MAJOR	2	/* the major number for floppy */
# endif	/* ! FLOPPY_MAJOR */
# ifndef MAJOR
#  define MAJOR(dev)	\
   ({ \
	  unsigned long long __dev = (dev); \
	  (unsigned) ((__dev >> 8) & 0xfff) \
				  | ((unsigned int) (__dev >> 32) & ~0xfff); \
   })
# endif /* ! MAJOR */
# ifndef CDROM_GET_CAPABILITY
#  define CDROM_GET_CAPABILITY	0x5331	/* get capabilities */
# endif /* ! CDROM_GET_CAPABILITY */
# ifndef BLKGETSIZE
#  define BLKGETSIZE	_IO(0x12,96)	/* return device size */
# endif	/* ! BLKGETSIZE */
#endif /* __linux__ */























































#define WITHOUT_LIBC_STUBS	1
#include <shared.h>
#include <device.h>

/* Get the geometry of a drive DRIVE. */
void get_drive_geometry(struct geometry *geom, char **map, int drive) {
	int fd;

	if (geom->flags == -1) {
		fd = open(map[drive], O_RDONLY);
		assert(fd >= 0);
	} else
	  fd = geom->flags;

	/* This is the default size. */
	geom->sector_size = SECTOR_SIZE;

#if defined(__linux__)
	/* Linux */
	{
		struct hd_geometry hdg;
		unsigned long nr;

		if (ioctl(fd, HDIO_GETGEO, &hdg)) {
			if (drive & 0x80) {
				struct stat st;
				int v;

				if (fstat(fd, &st)) 
				  goto fail;

				hdg.heads = 255;
				hdg.sectors = DEFAULT_HD_SECTORS;
				nr = st.st_size / SECTOR_SIZE;
				if (st.st_size % SECTOR_SIZE)
				  nr++;

				v = hdg.heads * hdg.sectors;
				hdg.cylinders = nr / v;
				if (nr % v)
				  hdg.cylinders++;

				goto file_disk;
			} 
			goto fail;
		}

		if (ioctl(fd, BLKGETSIZE, &nr))
		  goto fail;

file_disk:	
		/* Got the geometry, so save it. */
		geom->cylinders = hdg.cylinders;
		geom->heads = hdg.heads;
		geom->sectors = hdg.sectors;
		geom->total_sectors = nr;

		goto success;
	}
#else
	/* Notably, defined(__GNU__) */
# warning "Automatic detection of geometries will be performed only \
	partially. This is not fatal."
#endif
	
	fail:
	{
		struct stat st;

		/* FIXME: It would be nice to somehow compute fake C/H/S settings,
		 * given a proper st_blocks size. */
		if (drive & 0x80) {
			
			geom->cylinders = DEFAULT_HD_CYLINDERS;
			geom->heads = DEFAULT_HD_HEADS;
			geom->sectors = DEFAULT_HD_SECTORS;
		} else {
			geom->cylinders = DEFAULT_FD_CYLINDERS;
			geom->heads = DEFAULT_FD_HEADS;
			geom->sectors = DEFAULT_FD_SECTORS;
		}

		/* Set the total sectors properly, if we can. */
		if (! fstat(fd, &st) && st.st_size)
		  geom->total_sectors = st.st_size >> SECTOR_BITS;
		else
		  geom->total_sectors = geom->cylinders * geom->heads * geom->sectors;
	}

	success:
	if (geom->flags == -1)
	  close(fd);
}








































































































































/* These three functions are quite different among OSes. */
static void get_floppy_disk_name(char *name, int unit) {
#if defined(__linux__)
	sprintf(name, "dev/fd%d", unit);
#else
# warning "BIOS floppy drives cannot be guessed in your operating system."
	/* Set NAME to a bogus string. */
	*name = 0;
#endif
}



































static void get_ide_disk_name(char *name, int unit) {
#if defined(__linux__)
	sprintf(name, "/dev/hd%c", unit + 'a');
#else
# warning "BIOS IDE drives cannot be guessed in your operating system."
	/* Set NAME to a bogus string. */
	*name = 0;
#endif
}
































static void get_scsi_disk_name(char *name, int unit) {
#if defined(__linux__)
	sprintf(name, "/dev/sd%c", unit + 'a');
#else
# warning "BIOS SCSI drives cannot be guessed in your operating system."
	*name = 0;
#endif
}


































#ifdef __linux__







static void get_ataraid_disk_name(char *name, int unit) {
	sprintf(name, "/dev/ataraid/d%c", unit + '0');
}
#endif


/* Check if DEVICE can be read. If an error occurs, return zero,
 * otherwise return non-zero. */
int check_device(const char *device) {
	char buf[512];
	FILE *fp;

	/* If DEVICE is empty, just return 1. */
	if (*device == 0)
	  return 1;

	fp = fopen(device, "r");
	if (! fp) {
		switch (errno) {
#ifdef ENOMEDIUM
			case ENOMEDIUM:
				/* At the moment, this finds only CDROMS, which can't be
				 * read anyway, so leave it out. Code should be 
				 * reactivated if `removable disks' and CDROMS are
				 * supported. */
				/* Accept it, it may be inserted. */
#endif /* ENOMEDIUM */
				break;
			default:
				/* Break case and leave. */
				break;
		}
		return 0;
	}

	/* Make sure CD-ROMs don't get assigned a BIOS disk number
	 * before SCSI disks! */
#ifdef __linux__
# ifdef CDROM_GET_CAPABILITY
	if (ioctl(fileno(fp), CDROM_GET_CAPABILITY, 0) >= 0)
	  return 0;
# else /* ! CDROM_GET_CAPABILITY */
	/* Check if DEVICE is a CD-ROM drive by the HDIO_GETGEO ioctl. */
	{
		struct hd_geometry hdg;
		struct stat st;

		if (fstat(fileno(fp), &st))
		  return 0;

		/* If it is a block device and isn't a floppy, check if HDIO_GETGEO
		 * succeeds. */
		if (S_ISBLK(st.st_mode) &&
					MAJOR(st.st_rdev) != FLOPPY_MAJOR &&
					ioctl(fileno(fp), HDIO_GETGEO, &hdg))
		  return 0;
	}
# endif /* ! CDROM_GET_CAPABILITY */
#endif /* __linux__ */

	/* Attempt to read the first sector. */
	if (fread(buf, 1, 512, fp) != 512) {
		fclose(fp);
		return 0;
	}

	fclose(fp);
	return 1;
}









/* Read mapping information from FP, and write it to MAP. */
static int read_device_map(FILE *fp, char **map, const char *map_file) {
	auto void show_error(int no, const char *msg);
	auto void show_warning(int no, const char *msg, ...);

	auto void show_error(int no, const char *msg) {
		fprintf(stderr, "%s:%d: error: %s\n", map_file, no, msg);
	}

	auto void show_warning(int no, const char *msg, ...) {
		va_list ap;

		va_start(ap, msg);
		fprintf(stderr, "%s:%d: warning: ", map_file, no);
		vfprintf(stderr, msg, ap);
		va_end(ap);
	}

	/* If there is the device map file, use the data in it instead of
	 * probing devices. */
	char buf[1024];
	int line_number = 0;

	while (fgets(buf, sizeof(buf), fp)) {
		char *ptr, *eptr;
		int drive;
		int is_floppy = 0;

		/* Increase the number of lines. */
		line_number++;

		/* If the first character is '#', skip it. */
		if (buf[0] == '#')
		  continue;

		ptr = buf;
		/* Skip leading spaces. */
		while (*ptr && isspace(*ptr)) {
			ptr++;
		}

		/* Skip empty lines. */
		if (! ptr)
		  continue;

		if (*ptr != '(') {
			show_error(line_number, "No open parenthesis found");
			return 0;
		}

		ptr++;
		if ((*ptr != 'f' && *ptr != 'h') || *(ptr + 1) != 'd') {
			show_error(line_number, "Bad drive name");
			return 0;
		}

		if (*ptr == 'f')
		  is_floppy = 1;

		ptr += 2;
		drive = strtoul(ptr, &ptr, 10);
		if (drive < 0) {
			show_error(line_number, "Bad device number");
			return 0;
		} else if (drive > 127) {
			show_warning(line_number, 
					"Ignoring %cd%d due to a BIOS limitation",
					is_floppy ? 'f' : 'h', drive);
			continue;
		}

		if (! is_floppy)
		  drive += 0x80;

		if (*ptr != ')') {
			show_error(line_number, "No close parenthesis found");
			return 0;
		}

		ptr++;
		/* Skip spaces. */
		while (*ptr && isspace(*ptr)) {
			ptr++;
		}

		if (! *ptr) {
			show_error(line_number, "No filename found");
			return 0;
		}

		/* Terminate the filename. */
		eptr = ptr;
		while (*eptr && ! isspace(*eptr)) {
			eptr++;
		}
		*eptr = 0;

		/* Multiple entries for a given drive is not allowed. */
		if (map[drive]) {
			show_error(line_number, "Duplicated entry found");
			return 0;
		}

		map[drive] = strdup(ptr);
		assert(map[drive]);
	}

	return 1;
}














/* Initialize the device map MAP. *MAP will be allocated from the heap
 * space. If MAP_FILE is not NULL, then read mappings from the file
 * MAP_FILE if it exists, otherwise, write guessed mappings to the file.
 * FLOPPY_DISKS is the number of floppy disk drives which will be probed.
 * If it is zero, don't probe any floppy at all. If it is one, probe one
 * floppy. If it is two, probe two floppies. And so on. */
int init_device_map(char ***map, const char *map_file, int floppy_disks) {
	int i;
	int num_hd = 0;
	FILE *fp = 0;

	assert(map);
	assert(*map == 0);
	*map = malloc(NUM_DISKS * sizeof(char *));
	assert(*map);

	/* Probe devices for creating the device map. */

	/* Initialize DEVICE_MAP. */
	for (i = 0; i < NUM_DISKS; ++i) {
		(*map)[i] = 0;
	}

	if (map_file) {
		/* Open the device map file. */
		fp = fopen(map_file, "r");
		if (fp) {
			int ret;

			ret = read_device_map(fp, *map, map_file);
			fclose(fp);
			return ret;
		}
	}

	/* Print something so that the user does not think GRUB has been
	 * crashed. */
	fprintf(stderr,
		"Probing devices to guess BIOS drives. "
		"This may take a long time.\n");

	if (map_file) {
		/* Try to open the device map file to write the probed data. */
		fp = fopen(map_file, "w");
	}

	/* Floppies. */
	for (i = 0; i < floppy_disks; i++) {
		char name[16];

		get_floppy_disk_name(name, i);
		/* In floppies, write the map, whether check_device succeeds
		 * or not, because the user just does not insert floppies. */
		if (fp)
		  fprintf(fp, "(fd%d)\t%s\n", i, name);
		
		if (check_device(name)) {
			(*map)[i] = strdup(name);
			assert((*map)[i]);
		}
	}
	
	/* IDE disks. */
	for (i = 0; i < 8; i++) {
		char name[16];

		get_ide_disk_name(name, i);
		if (check_device(name)) {
			(*map)[num_hd + 0x80] = strdup(name);
			assert((*map)[num_hd + 0x80]);

			/* If the device map file is opened, write the map. */
			if (fp)
			  fprintf(fp, "(hd%d)\t%s\n", num_hd, name);

			num_hd++;
		}
	}

#ifdef __linux__
	/* ATARAID disks. */
	for (i = 0; i < 8; i++) {
		char name[20];

		get_ataraid_disk_name(name, i);
		if (check_device(name)) {
			(*map)[num_hd + 0x80] = strdup(name);
			assert((*map)[num_hd + 0x80]);

			/* If the device map file is opened, write the map. */
			if (fp)
			  fprintf(fp, "(hd%d)\t%s\n", num_hd, name);

			num_hd++;
		}
	}
#endif /* __linux__ */

	/* The rest is SCSI disks. */
	for (i = 0; i < 16; i++) {
		char name[20];

		get_scsi_disk_name(name, i);
		if (check_device(name)) {
			(*map)[num_hd + 0x80] = strdup(name);
			assert((*map)[num_hd + 0x80]);

			/* If the device map file is opened, write the map. */
			if (fp)
			  fprintf(fp, "(hd%d)\t%s\n", num_hd, name);

			num_hd++;
		}
	}

	/* OK, close the device map file if opened. */
	if (fp)
	  fclose(fp);

	return 1;
}



















/* Restore the memory consumed for MAP. */
void restore_device_map(char **map) {
	int i;

	for (i = 0; i < NUM_DISKS; i++) {
		if (map[i])
		  free(map[i]);
	}
	free(map);
}

#ifdef __linux__
/* Linux-only functions, because Linux has a bug that the disk cache for
 * a whole disk is not consistent with the one for a partition of the
 * disk. */
int is_disk_device(char **map, int drive) {
	struct stat st;

	assert(map[drive] != 0);
	assert(stat(map[drive], &st) == 0);
	/* For now, disk devices under Linux are all block devices. */
	return S_ISBLK(st.st_mode);
}

int write_to_partition(char **map, int drive, int partition,
			int sector, int size, const char *buf) {
	printf("TODO write_to_partition\n");

	return 1;
}
#endif /* __linux__ */
