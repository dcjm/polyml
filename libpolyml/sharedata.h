/*
    Title:      Share common immutable data

    Copyright (c) 2000
        Cambridge University Technical Services Limited
    and David C. J. Matthews 2006, 2010-13, 2016

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

#ifndef _SHAREDATA_H
#define _SHAREDATA_H

// Called by the thread to make the request.
void ShareData(TaskData *taskData, Handle root);

extern struct _entrypts shareDataEPT[];

#endif
