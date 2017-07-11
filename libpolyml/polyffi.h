/*
    Title:  New Foreign Function Interface

    Copyright (c) 2015-16 David C.J. Matthews

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


#ifndef _POLYFFI_H_DEFINED
#define _POLYFFI_H_DEFINED 1

class SaveVecEntry;
typedef SaveVecEntry *Handle;
class TaskData;

extern Handle poly_ffi (TaskData *taskData, Handle args, Handle code);

extern Handle cmem_load_8(TaskData *taskData, Handle indexH, Handle offsetH, Handle baseH);
extern Handle cmem_load_16(TaskData *taskData, Handle indexH, Handle offsetH, Handle baseH);
extern Handle cmem_load_32(TaskData *taskData, Handle indexH, Handle offsetH, Handle baseH);
extern Handle cmem_load_float(TaskData *taskData, Handle indexH, Handle offsetH, Handle baseH);
extern Handle cmem_load_double(TaskData *taskData, Handle indexH, Handle offsetH, Handle baseH);
extern Handle cmem_store_8(TaskData *taskData, Handle valueH, Handle indexH, Handle offsetH, Handle baseH);
extern Handle cmem_store_16(TaskData *taskData, Handle valueH, Handle indexH, Handle offsetH, Handle baseH);
extern Handle cmem_store_32(TaskData *taskData, Handle valueH, Handle indexH, Handle offsetH, Handle baseH);

extern Handle cmem_store_float(TaskData *taskData, Handle valueH, Handle indexH, Handle offsetH, Handle baseH);
extern Handle cmem_store_double(TaskData *taskData, Handle valueH, Handle indexH, Handle offsetH, Handle baseH);

#if (SIZEOF_VOIDP == 8)
// 64-bit mode only.
extern Handle cmem_load_64(TaskData *taskData, Handle indexH, Handle offsetH, Handle baseH);
extern Handle cmem_store_64(TaskData *taskData, Handle valueH, Handle indexH, Handle offsetH, Handle baseH);
#endif

extern struct _entrypts polyFFIEPT[];

#endif
