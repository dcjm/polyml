/*
    Title:  arb.h - exports signature for arb.c

    Copyright (c) 2000
        Cambridge University Technical Services Limited

    Further modification Copyright 2015-16, 2019-20 David C. J. Matthews

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

#ifndef ARB_H_INCLUDED
#define ARB_H_INCLUDED

class SaveVecEntry;
typedef SaveVecEntry *Handle;
class TaskData; 

// These functions create values of type int from signed or unsigned values.
extern Handle Make_arbitrary_precision(TaskData *taskData, long);
extern Handle Make_arbitrary_precision(TaskData *taskData, unsigned long);

extern Handle Make_arbitrary_precision(TaskData *taskData, int);
extern Handle Make_arbitrary_precision(TaskData *taskData, unsigned);

#ifdef HAVE_LONG_LONG
extern Handle Make_arbitrary_precision(TaskData *taskData, long long);
extern Handle Make_arbitrary_precision(TaskData *taskData, unsigned long long);
#endif

extern Handle Make_arb_from_32bit_pair(TaskData *taskData, uint32_t hi, uint32_t lo);
extern Handle Make_arb_from_pair_scaled(TaskData *taskData, unsigned hi, unsigned lo, unsigned scale);

#if defined(_WIN32)
extern Handle Make_arb_from_Filetime(TaskData *taskData, const FILETIME &ft);
#endif

// Currently used in basicio.cpp
extern Handle div_longc(TaskData *taskData, Handle,Handle);
extern Handle rem_longc(TaskData *taskData, Handle,Handle);

// Used in emulator
extern Handle add_longc(TaskData* taskData, Handle, Handle);
extern Handle sub_longc(TaskData* taskData, Handle, Handle);
extern int compareLong(PolyWord y, PolyWord x);


// Return a uintptr_t/intptr_t value.
extern POLYUNSIGNED     getPolyUnsigned(TaskData *taskData, PolyWord);
extern POLYSIGNED       getPolySigned(TaskData *taskData, PolyWord);

#define get_C_ulong     getPolyUnsigned
extern unsigned short   get_C_ushort(TaskData *taskData, PolyWord);
extern unsigned         get_C_unsigned(TaskData *taskData, PolyWord);

#define get_C_long      getPolySigned
extern short            get_C_short(TaskData *taskData, PolyWord);
extern int              get_C_int(TaskData *taskData, PolyWord);

extern double           get_arbitrary_precision_as_real(PolyWord x);

#if defined(_WIN32)
void getFileTimeFromArb(TaskData *taskData, Handle numHandle, PFILETIME ft);
#endif

extern struct _entrypts arbitraryPrecisionEPT[];

#endif
