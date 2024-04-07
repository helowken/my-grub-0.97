/* boot.c - load and bootstrap a kernel */
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

#include "shared.h"
#include "i386-elf.h"

static int cur_addr;
entry_func entry_addr;
static int linux_mem_size;


/* ======================= Memory Layout =========================
bzImage = setup.bin + vmlinux.bin

          ---------------------------------- 
0x6762000 | moved initramfs                | 
          ---------------------------------- 
  ...     |                                | 
          ---------------------------------- 
0x46a0d0  | initramfs (size: 0x188b0e0)    |      <- cur_addr
          ----------------------------------  ---
0x46a0cf  | '\0'                           |   |   
  ...     | ... noquiet                    |  size: 0x100              
0x469fd0  | cmd_arg: ro root=UUID=...      |   |
          ----------------------------------  ---
  ...     | setup.bin + vmlinux.bin        |   |
		  | (size: data_len=0x5200)        |  size: 0x9000
0x460fd0  | linux_kernel_header            |   |  <- linux_data_tmp_addr
  ...     ----------------------------------  ---
          | (size: text_len=0x360fd0)      |
0x100000  | text_len (vmlinux.bin)         |      <- LINUX_BAIMAGE_ADDR
  ...     ----------------------------------  ---
0x99100   | '\0'                           |   |
  ...     | ... noquiet                    |  size: 0x100
0x99000	  | copied cmd_arg: ...            |   |  <- sp, lh->cmd_line_ptr
          ----------------------------------  --- 
  ...     |                                |   |
		  | (size: data_len=0x5200)        |  size: 0x9000 
0x90200   |    [entry point]               |   |  <- jump to "start_of_setup" (header.S in linux)
  ...	  |	                               |   |
0x90000   | copied linux_kernel_header     |   |  <- linux_data_real_addr
          ----------------------------------  ---

================================================================== */


/* The next two functions, 'load_image' and 'load_module', are the buildinng
 * blocks of the multiboot loader component.  They handle essentially all
 * of the gory details of loading in a bootable image nad the modules.
 */
kernel_t load_image(char *kernel, char *arg, kernel_t suggested_type,
					unsigned long load_flags) {
	int len;
	unsigned long text_len = 0, data_len = 0;
	struct linux_kernel_header *lh;	

	/* presuming that MULTIBOOT_SEARCH is large enough to encompass an
	 * executable header */
	unsigned char buffer[MULTIBOOT_SEARCH];

	if (! grub_open(kernel))
	  return KERNEL_TYPE_NONE;

	if (! (len = grub_read(buffer, MULTIBOOT_SEARCH)) || len < 32) {
		grub_close();

		if (! errnum)
		  errnum = ERR_EXEC_FORMAT;

		return KERNEL_TYPE_NONE;
	}

	/* Use BUFFER as a linux kernel header, if the image is Linux zImage
	 * or bzImage. */
	lh = (struct linux_kernel_header *) buffer;

	if (lh->boot_flag == BOOTSEC_SIGNATURE && 
				lh->setup_sects <= LINUX_MAX_SETUP_SECTS) {
		int big_linux = 0;
		int setup_sects = lh->setup_sects;

		if (lh->header == LINUX_MAGIC_SIGNATURE && lh->version >= 0x0200) {
			big_linux = (lh->loadflags & LINUX_FLAG_BIG_KERNEL);
			lh->type_of_loader = LINUX_BOOT_LOADER_TYPE;

			/* Put the real mode part at as a high location as possible. */
			linux_data_real_addr = (char *) ((mbi.mem_lower << 10) - LINUX_SETUP_MOVE_SIZE);
			/* But it must not exceed the traditional area. */
			if (linux_data_real_addr > (char *) LINUX_OLD_REAL_MODE_ADDR)
			  linux_data_real_addr = (char *) LINUX_OLD_REAL_MODE_ADDR;

			if (lh->version >= 0x0201) {
				lh->heap_end_ptr = LINUX_HEAP_END_OFFSET;
				lh->loadflags |= LINUX_FLAG_CAN_USE_HEAP;
			}

			if (lh->version >= 0x0202)
			  lh->cmd_line_ptr = linux_data_real_addr + LINUX_CL_OFFSET;
			else {
				lh->cl_magic = LINUX_CL_MAGIC;
				lh->cl_offset = LINUX_CL_OFFSET;
				lh->setup_move_size = LINUX_SETUP_MOVE_SIZE;
			}
		} else {
			printf("Your kernel is quite old...\n");
			return KERNEL_TYPE_NONE;
		}

		/* If SETUP_SECTS is not set, set it to the default (4). */
		if (! setup_sects)
		  setup_sects = LINUX_DEFAULT_SETUP_SECTS;

		data_len = setup_sects << 9;
		/* setup_sects = actual setup sectors - 1, so need to subtract SECTOR_SIZE */
		text_len = filemax - data_len - SECTOR_SIZE;	

		linux_data_tmp_addr = (char *) LINUX_BZIMAGE_ADDR + text_len;

		if (! big_linux && 
					text_len > linux_data_real_addr - (char *) LINUX_ZIMAGE_ADDR) {
			grub_printf(" linux 'zImage' kernel too big, try 'make bzImage'\n");
			errnum = ERR_WONT_FIT;
		} else if (linux_data_real_addr + LINUX_SETUP_MOVE_SIZE > 
								RAW_ADDR ((char *) (mbi.mem_lower << 10))) {
			errnum = ERR_WONT_FIT;
		} else {
			grub_printf("   [Linux-%s, setup=0x%x, size=0x%x]\n",
					(big_linux ? "bzImage" : "zImage"), data_len, text_len);

			/* Video mode selection support. What a mess! */
			/* NOTE: Even the word "mess" is not still enough to
			 * represent how wrong and bad the Linux video support is,
			 * but I don't want to hear complaints from Linux fanatics
			 * any more. -okuji */
			{
				char *vga;

				/* Find the substring "vga=". */
				vga = grub_strstr(arg, "vga=");
				if (vga) {
					char *value = vga + 4;
					int vid_mode;

					/* Handle special strings. */
					if (substring("normal", value) < 1)
					  vid_mode = LINUX_VID_MODE_NORMAL;
					else if (substring("ext", value) < 1)
					  vid_mode = LINUX_VID_MODE_EXTENDED;
					else if (substring("ask", value) < 1)
					  vid_mode = LINUX_VID_MODE_ASK;
					else if (safe_parse_maxint(&value, &vid_mode)) {

					} else {
						/* ERRNUM is already set inside the function
						 * safe_parse_maxint. */
						grub_close();
						return KERNEL_TYPE_NONE;
					}

					lh->vid_mode = vid_mode;
				}
			}

			/* Check the mem= option to limit memory used for initrd. */
			{
				char *mem;

				mem = grub_strstr(arg, "mem=");
				if (mem) {
					char *value = mem + 4;

					safe_parse_maxint(&value, &linux_mem_size);
					switch (errnum) {
						case ERR_NUMBER_OVERFLOW:
						/* If and overflow occurs, use the maximum address for
						 * initrd instead. This is good, because MAXINT is
						 * greater than LINUX_INITRD_MAX_ADDRESS. */
							linux_mem_size = LINUX_INITRD_MAX_ADDRESS;
							errnum = ERR_NONE;
							break;

						case ERR_NONE:
						{
							int shift = 0;
							switch (grub_tolower(*value)) {
								case 'g':
									shift += 10;
								case 'm':
									shift += 10;
								case 'k':
									shift += 10;
								default:
									break;
							}

							/* Check an overflow. */
							if (linux_mem_size > (MAXINT >> shift))
							  linux_mem_size = LINUX_INITRD_MAX_ADDRESS;
							else
							  linux_mem_size <<= shift;
						}
							break;
					
						default:
							linux_mem_size = 0;
							errnum = ERR_NONE;
							break;
					}
				} else
				  linux_mem_size = 0;
			}

			/* It is possible that DATA_LEN + SECTOR_SIZE is greater than
			 * MULTIBOOT_SEARCH, so the data may have been read partially. */
			if (data_len + SECTOR_SIZE <= MULTIBOOT_SEARCH)
			  grub_memmove(linux_data_tmp_addr, buffer, data_len + SECTOR_SIZE);
			else {
				grub_memmove(linux_data_tmp_addr, buffer, MULTIBOOT_SEARCH);
				grub_read(linux_data_tmp_addr + MULTIBOOT_SEARCH,
							data_len + SECTOR_SIZE - MULTIBOOT_SEARCH);
			}

			/* Copy command-line plus memory hack to staging area.
			 * NOTE: Linux has a bug that it doesn't handle multiple spaces
			 * between two options and a space after a "mem=" option isn't
			 * removed correctly so the arguments to init could be like
			 * {"init", "", "", NULL}. This affects some not-very-clever
			 * shells. Thus, the code below does a strick to avoid the bug.
			 * That is, copy "mem=XXX" to the end of the command-line, and
			 * avoid to copy spaces unnecessarily. Hell. */
			{
				char *src = skip_to(0, arg);
				char *dest = linux_data_tmp_addr + LINUX_CL_OFFSET;

				while (dest < linux_data_tmp_addr + LINUX_CL_END_OFFSET && *src) {
					*(dest++) = *(src++);
				}

				/* The "mem" option is added if neither of the
				 * following conditions is met:
				 * 1) The "mem" option is already present.
				 * 2) The "kernel" command is used with "--no-mem-option".
				 * 3) GNU GRUB is configured not to pass the "mem" option.
				 * 4) The kernel supports boot protocol 2.03 or newer. */
				if (! grub_strstr(arg, "mem=") && 
							! (load_flags & KERNEL_LOAD_NO_MEM_OPTION) &&
							lh->version < 0x0203 &&		/* kernel version < 2.4.18 */
							dest + 15 < linux_data_tmp_addr + LINUX_CL_END_OFFSET) {
					*dest++ = ' ';
					*dest++ = 'm';
					*dest++ = 'e';
					*dest++ = 'm';
					*dest++ = '=';

					dest = convert_to_ascii(dest, 'u', (extended_memory + 0x400));
					*dest++ = 'K';
				}
				*dest = 0;
			}

			/* offset into file */
			grub_seek(data_len + SECTOR_SIZE);

			cur_addr = (int) linux_data_tmp_addr + LINUX_SETUP_MOVE_SIZE;
			grub_read((char *) LINUX_BZIMAGE_ADDR, text_len);

			if (errnum == ERR_NONE) {
				grub_close();

				/* Sanity check. */
				if (suggested_type != KERNEL_TYPE_NONE &&
							((big_linux && suggested_type != KERNEL_TYPE_BIG_LINUX) ||
							 (! big_linux && suggested_type != KERNEL_TYPE_LINUX))) {
					errnum = ERR_EXEC_FORMAT;
					return KERNEL_TYPE_NONE;
				}

				/* Ugly hack. */
				linux_text_len = text_len;

				return big_linux ? KERNEL_TYPE_BIG_LINUX : KERNEL_TYPE_LINUX;
			}
		}
	} else		/* no recognizable format */
	  errnum = ERR_EXEC_FORMAT;

	/* return if error */
	if (errnum) 
	  grub_close();

	return KERNEL_TYPE_NONE;
}


int load_initrd(char *initrd) {
	int len;
	unsigned long moveto;
	unsigned long max_addr;
	struct linux_kernel_header *lh = 
			(struct linux_kernel_header *) (cur_addr - LINUX_SETUP_MOVE_SIZE);

#ifndef NO_DECOMPRESSION
	no_decompression = 1;
#endif /* ! NO_DECOMPRESSION */

	if (! grub_open(initrd))
	  goto fail;

	len = grub_read((char *) cur_addr, -1);
	if (! len) {
		grub_close();
		goto fail;
	}

	if (linux_mem_size)
	  moveto = linux_mem_size;
	else
	  moveto = (mbi.mem_upper + 0x400) << 10;

	moveto = (moveto - len) & 0xfffff000;
	max_addr = (lh->header == LINUX_MAGIC_SIGNATURE && lh->version >= 0x0203 ?
				lh->initrd_addr_max : LINUX_INITRD_MAX_ADDRESS);
	if (moveto + len >= max_addr)
	  moveto = (max_addr - len) & 0xfffff000;

	/* XXX: Linux 2.3.xx has a bug in the memory range check, so avoid
	 * the last page.
	 * XXX: Linux 2.2.xx has a bug in the memory range check, which is
	 * worse than that of Linux 2.3.xx, so avoid the last 64kb. *sigh* */
	moveto -= 0x10000;
	memmove((void *) RAW_ADDR(moveto), (void *) cur_addr, len);

	printf("   [Linux-initrd @ 0x%x, 0x%x bytes]\n", moveto, len);

	/* FIXME: Should check if the kernel supports INITRD. */
	lh->ramdisk_image = RAW_ADDR(moveto);
	lh->ramdisk_size = len;

	grub_close();

fail:
#ifndef NO_DECOMPRESSION
	no_decompression = 0;
#endif /* ! NO_DECOMPRESSION */

	return ! errnum;
}

