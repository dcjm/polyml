/*
    Title:  reals.h

    Copyright (c) 2000
        Cambridge University Technical Services Limited
    Further development Copyright David C.J. Matthews 2015

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

#ifndef _REALS_H

#define _REALS_H

// This is used in the interpreter
extern int getrounding();
extern int setrounding(int rounding);

#define POLY_ROUND_TONEAREST    0
#define POLY_ROUND_DOWNWARD     1
#define POLY_ROUND_UPWARD       2
#define POLY_ROUND_TOZERO       3

extern struct _entrypts realsEPT[];

#endif
