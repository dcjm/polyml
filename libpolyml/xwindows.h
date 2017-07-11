/*
    Title:  xwindows.h - export signature for xwindows.c, noxwindows.c 

    Copyright (c) 2000
        Cambridge University Technical Services Limited

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.
    
    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.
    
    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA

*/

/* created 26/10/93 - SPF */

#ifndef XWINDOWS_H_INCLUDED
#define XWINDOWS_H_INCLUDED

class SaveVecEntry;
typedef SaveVecEntry *Handle;
class TaskData;

extern Handle XWindows_c(TaskData *taskData, Handle params);

extern struct _entrypts xwindowsEPT[];

#endif
