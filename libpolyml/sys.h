/*
    Title:  sys.h

    Copyright (c) 2000-7
        Cambridge University Technical Services Limited
    Further development Copyright David C.J. Matthews 2007-12, 2015-16

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License version 2.1 as published by the Free Software Foundation.
    
    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.
    
    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA

*/

#ifndef _SYS_H
#define _SYS_H

#define EXC_interrupt   1 /* SML90.Interrupt */
#define EXC_syserr      2 /* System call failed. */
/* EXC_size (formerly EXC_range) is raised in a number of places,
   most particularly in alloc_store when given a length which is
   too large.  As "Size" it is used extensively in the Basis library. */
#define EXC_size        4 /* General.Size */
#define EXC_overflow    5 /* General.Overflow */
#define EXC_divide      7 /* General.Div */
#define EXC_conversion  8
/*
  EXC_conversion is used within the compiler and by conversion routines
  added by the compiler in order to signal failure of conversion.
*/
#define EXC_XWindows    10
#define EXC_subscript   11 /* General.Subscript */
#define EXC_thread      12 /* Thread.Thread. */  /* DCJM 13/3/07 */
#define EXC_foreign     23  /* nic 4/5/94 */
#define EXC_Fail        103  /* DCJM 11/5/06 */
// Make sure to add any additional exceptions to make_exn.

#endif

