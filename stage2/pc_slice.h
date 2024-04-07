/*
 *  GRUB  --  GRand Unified Bootloader
 *  Copyright (C) 1999,2000,2001,2003   Free Software Foundation, Inc.
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

#ifndef _PC_SLICE_H
#define _PC_SLICE_H

/*
 * These define the basic PC MBR sector characteristics
 */

#define PC_MBR_SECTOR	0

#define PC_MBR_SIG_OFFSET	510
#define PC_MBR_SIGNATURE	0xaa55

#define PC_SLICE_OFFSET	446
#define PC_SLICE_MAX	4


/*
 * Defines to guarantee structural alignment. 
 */

#define PC_MBR_CHECK_SIG(mbr_ptr) \
	( *( (unsigned short *) (((int) mbr_ptr) + PC_MBR_SIG_OFFSET) ) \
		== PC_MBR_SIGNATURE )

#define PC_MBR_SIG(mbr_ptr) \
	( *( (unsigned short *) (((int) mbr_ptr) + PC_MBR_SIG_OFFSET) ) )

#define PC_SLICE_FLAG(mbr_ptr, part) \
	( *( (unsigned char *) (((int) mbr_ptr) + PC_SLICE_OFFSET \
				+ (part << 4)) ) )

#define PC_SLICE_HEAD(mbr_ptr, part) \
	( *( (unsigned char *) (((int) mbr_ptr) + PC_SLICE_OFFSET + 1 \
				+ (part << 4)) ) )

#define PC_SLICE_SEC(mbr_ptr, part) \
	( *( (unsigned char *) (((int) mbr_ptr) + PC_SLICE_OFFSET + 2 \
				+ (part << 4)) ) )

#define PC_SLICE_CYL(mbr_ptr, part) \
	( *( (unsigned char *) (((int) mbr_ptr) + PC_SLICE_OFFSET + 3 \
				+ (part << 4)) ) )

#define PC_SLICE_TYPE(mbr_ptr, part) \
	( *( (unsigned char *) (((int) mbr_ptr) + PC_SLICE_OFFSET + 4 \
				+ (part << 4)) ) )

#define PC_SLICE_EHEAD(mbr_ptr, part) \
	( *( (unsigned char *) (((int) mbr_ptr) + PC_SLICE_OFFSET + 5 \
				+ (part << 4)) ) )

#define PC_SLICE_ESEC(mbr_ptr, part) \
	( *( (unsigned char *) (((int) mbr_ptr) + PC_SLICE_OFFSET + 6 \
				+ (part << 4)) ) )

#define PC_SLICE_ECYL(mbr_ptr, part) \
	( *( (unsigned char *) (((int) mbr_ptr) + PC_SLICE_OFFSET + 7 \
				+ (part << 4)) ) )

#define PC_SLICE_START(mbr_ptr, part) \
	( *( (unsigned long *) (((int) mbr_ptr) + PC_SLICE_OFFSET + 8 \
				+ (part << 4)) ) )

#define PC_SLICE_LENGTH(mbr_ptr, part) \
	( *( (unsigned long *) (((int) mbr_ptr) + PC_SLICE_OFFSET + 12 \
				+ (part << 4)) ) )


/*
 * PC flag types are defined here.
 */

#define PC_SLICE_FLAG_NONE		0
#define PC_SLICE_FLAG_BOOTABLE	0x80

/* 
 * Known PC partition types are defined here.
 */

/* This is not a flag actually, but used as if it were a flag. */
#define PC_SLICE_TYPE_HIDDEN_FLAG	0x10

#define PC_SLICE_TYPE_NONE			0
#define PC_SLICE_TYPE_FAT12			1
#define PC_SLICE_TYPE_FAT16_LT32M	4
#define PC_SLICE_TYPE_EXTENDED		5
#define PC_SLICE_TYPE_FAT16_GT32M	6
#define PC_SLICE_TYPE_FAT32		0xb
#define PC_SLICE_TYPE_FAT32_LBA		0xc
#define PC_SLICE_TYPE_FAT16_LBA		0xe
#define PC_SLICE_TYPE_WIN95_EXTENDED	0xf
#define PC_SLICE_TYPE_EZD			0x55
#define PC_SLICE_TYPE_MINIX			0x80
#define PC_SLICE_TYPE_LINUX_MINIX	0x81
#define PC_SLICE_TYPE_EXT2FS		0x83
#define PC_SLICE_TYPE_LINUX_EXTENDED	0x85
#define PC_SLICE_TYPE_VSTAFS		0x9e
#define PC_SLICE_TYPE_DELL_UTIL		0xde
#define PC_SLICE_TYPE_LINUX_RAID	0xfd















#define IS_PC_SLICE_TYPE_EXTENDED(type)	\
	(((type) == PC_SLICE_TYPE_EXTENDED)	|| \
	 ((type) == PC_SLICE_TYPE_WIN95_EXTENDED) || \
	 ((type) == PC_SLICE_TYPE_LINUX_EXTENDED))





/* these ones are special, as they use their own partitioning scheme
 * to subdivide the PC partitions from there. */
#define PC_SLICE_TYPE_FREEBSD		0xa5
#define PC_SLICE_TYPE_OPENBSD		0xa6
#define PC_SLICE_TYPE_NETBSD		0xa9

/* For convenience. */
#define IS_PC_SLICE_TYPE_BSD_WITH_FS(type, fs)	\
	((type) == (PC_SLICE_TYPE_FREEBSD | ((fs) << 8)) || \
	 (type) == (PC_SLICE_TYPE_OPENBSD | ((fs) << 8)) || \
	 (type) == (PC_SLICE_TYPE_NETBSD | ((fs) << 8)))

#define IS_PC_SLICE_TYPE_BSD(type)	IS_PC_SLICE_TYPE_BSD_WITH_FS(type, 0)










































































/* possible values for the *BSD-style partition type */










#define FS_OTHER	10	/* in use, but unknown/unsupported */






#define FS_EXT2FS	17	/* Linux Extended 2 file system */






















































































































































































































#endif /* ! _PC_SLICE_H */

