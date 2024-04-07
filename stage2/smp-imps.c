/*
 *  GRUB  --  GRand Unified Bootloader
 *  Copyright (C) 1999,2005  Free Software Foundation, Inc.
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

/*
 *  <Insert copyright here : it must be BSD-like so anyone can use it>
 *
 *  Author:  Erich Boleyn  <erich@uruk.org>   http://www.uruk.org/~erich/
 *
 *  Source file implementing Intel MultiProcessor Specification (MPS)
 *  version 1.1 and 1.4 SMP hardware control for Intel Architecture CPUs,
 *  with hooks for running correctly on a standard PC without the hardware.
 *
 *  This file was created from information in the Intel MPS version 1.4
 *  document, order number 242016-004, which can be ordered from the
 *  Intel literature center.
 *
 *  General limitations of this code:
 *
 *   (1) : This code has never been tested on an MPS-compatible system with
 *           486 CPUs, but is expected to work.
 *   (2) : Presumes "int", "long", and "unsigned" are 32 bits in size, and
 *	     that 32-bit pointers and memory addressing is used uniformly.
 */

#define _SMP_IMPS_C

/*
 * XXXXX  The following absolutely must be defined!!!
 *
 * The "KERNEL_PRINT" could be made a null macro with no danger, of
 * course, but pretty much nothing would work without the other
 * ones defined.
 */

/*
 * This is the Intel MultiProcessor Spec debugging/display code.
 */
#define IMPS_DEBUG
#define KERNEL_PRINT(x)			printf x
#define CMOS_WRITE_BYTE(X, y)	cmos_write_byte(x, y)
#define CMOS_READ_BYTE(x)	cmos_read_byte(x)
#define PHYS_TO_VIRTUAL(x)	(x)
#define VIRTUAL_TO_PHYS(x)	(x)


