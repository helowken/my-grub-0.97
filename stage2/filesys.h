/* filesys.h - abstract filesystem interface */
/*
 *  GRUB  --  GRand Unified Bootloader
 *  Copyright (C) 1999,2000,2001,2004  Free Software Foundation, Inc.
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

#include "pc_slice.h"

#ifdef FSYS_EXT2FS
#define FSYS_EXT2FS_NUM 1
int ext2fs_mount(void);
int ext2fs_read(char *buf, int len);
int ext2fs_dir(char *dirname);
#else
#define FSYS_EXT2FS_NUM 0
#endif

#ifndef NUM_FSYS
#define NUM_FSYS	\
	(FSYS_EXT2FS_NUM)
#endif

/* defines for the block filesystem info area */
#ifndef NO_BLOCK_FILES
#define BLK_CUR_FILEPOS			(*((int *) FSYS_BUF))
#define BLK_CUR_BLKLIST			(*((int *) (FSYS_BUF + 4)))
#define BLK_CUR_BLKNUM			(*((int *) (FSYS_BUF + 8)))
#define BLK_MAX_ADDR			(FSYS_BUF + 0x7FF9)
#define BLK_BLKSTART(l)			(*((int *) l))
#define BLK_BLKLENGTH(l)		(*((int *) (l + 4)))
#define BLK_BLKLIST_START		(FSYS_BUF + 12)
#define BLK_BLKLIST_INC_VAL		8
#endif /* ! NO_BLOCK_FILES */

/* this next part is pretty ugly, but it keeps it in one place! */

struct fsys_entry {
	char *name;
	int (*mount_func)(void);
	int (*read_func)(char *buf, int len);
	int (*dir_func)(char *dirname);
	void (*close_func)(void);
	int (*embed_func)(int *start_sector, int needed_sectors);
};


#ifdef STAGE1_5
# define print_possibilities	0 
#else
extern int print_possibilities;
#endif /* STAGE1_5 */

extern int fsmax;
extern struct fsys_entry fsys_table[NUM_FSYS + 1];
