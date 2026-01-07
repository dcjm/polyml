/*
    Title:  rts_module.h - Base class for the run-time system modules.

    Copyright (c) 2006, 2011, 2016, 2020 David C.J. Matthews

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

#ifndef RTS_MODULE_H_INCLUDED
#define RTS_MODULE_H_INCLUDED

class ScanAddress;
class TaskData;

class RtsModule
{
public:
    RtsModule() { RegisterModule(); }
    virtual ~RtsModule() {} // To keep GCC happy

    virtual void Init(void) {}
    virtual void Start(void) {}
    virtual void Stop(void) {}
    virtual void GarbageCollect(ScanAddress * /*process*/) {}
    virtual void ForkChild(void) {} // Called in the child process after a Unix fork.
private:
    void RegisterModule(void);
};

void InitModules(void);
void StartModules(void);
void StopModules(void);
void GCModules(ScanAddress *process);
void ForkChildModules(void);

#endif

