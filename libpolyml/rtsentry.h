/*
    Title:  rtsentry.h - Entry points to the run-time system

    Copyright (c) 2016 David C. J. Matthews

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

#ifndef RTSENTRY_H_INCLUDED
#define RTSENTRY_H_INCLUDED
class SaveVecEntry;
class TaskData;
class PolyObject;

typedef SaveVecEntry *Handle;

extern Handle creatEntryPointObject(TaskData *taskData, Handle entryH);
extern const char *getEntryPointName(PolyObject *p);
extern bool setEntryPoint(PolyObject *p);

typedef void (*polyRTSFunction)();

typedef struct _entrypts {
    const char *name;
    polyRTSFunction entry;
} *entrypts;

// Ensure that the RTS calls can be found by the linker.
#ifndef POLYEXTERNALSYMBOL
#ifdef _MSC_VER
#define POLYEXTERNALSYMBOL __declspec(dllexport)
#else
#define POLYEXTERNALSYMBOL
#endif
#endif

#endif
