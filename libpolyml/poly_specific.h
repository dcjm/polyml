/*
    Title:  poly_specific.h - exports signature for poly_specific.cpp

    Copyright (c) 2006, 2016 David C. J. Matthews

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

/* This module is used for various run-time calls that are either in the
   PolyML structure or otherwise specific to Poly/ML. */

#ifndef POLY_SPECIFIC_H_INCLUDED
#define POLY_SPECIFIC_H_INCLUDED

class SaveVecEntry;
typedef SaveVecEntry *Handle;
class TaskData;

extern Handle poly_dispatch_c(TaskData *mdTaskData, Handle args, Handle code);

extern struct _entrypts polySpecificEPT[];

#endif

