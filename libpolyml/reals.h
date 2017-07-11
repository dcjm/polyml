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

class SaveVecEntry;
typedef SaveVecEntry *Handle;
class TaskData;

extern double real_arg(Handle x); // Also used in "foreign.cpp"
extern Handle real_result(TaskData *mdTaskData, double x); // Also used in "foreign.cpp"

extern struct _entrypts realsEPT[];

#endif
