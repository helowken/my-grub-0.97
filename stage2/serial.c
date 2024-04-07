/* serial.c - serial device interface */
/*
 *  GRUB  --  GRand Unified Bootloader
 *  Copyright (C) 2000,2001,2002  Free Software Foundation, Inc.
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

#ifdef SUPPORT_SERIAL

#include <shared.h>
#include <serial.h>
#include <term.h>
#include <terminfo.h>










































































































































































































































































/* The serial version of getxy. */
int serial_getkey() {
	//TODO
	return -1;
}











/* The serial version of checkkey. */
int serial_checkkey() {
	//TODO
	return -1;
}





/* The serial version of grub_putchar. */
void serial_putchar(int c) {
	//TODO
}









































































int serial_getxy() {
	//TODO;
	return -1;
}


void serial_gotoxy(int x, int y) {
	//TODO
}








void serial_cls() {
	//TODO
}







void serial_setcolorstate(color_state state) {
	//TODO
}







#endif /* SUPPORT_SERIAL */
