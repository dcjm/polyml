/*
    Title:  objsize.h - export signature for objsize.c

    Copyright (c) 2000
        Cambridge University Technical Services Limited

    Further development David C.J. Matthews 2016

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

#ifndef OBJSIZE_H_DEFINED
#define OBJSIZE_H_DEFINED

class SaveVecEntry;
typedef SaveVecEntry *Handle;
class TaskData;

extern Handle ObjSize(TaskData *taskData, Handle obj);
extern Handle ShowSize(TaskData *taskData, Handle obj);
extern Handle ObjProfile(TaskData *taskData, Handle obj);

extern struct _entrypts objSizeEPT[];

#endif
