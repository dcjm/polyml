/*
    Title:  rts_module.cpp - Base class for the run-time system modules.

    Copyright (c) 2006, 2020 David C.J. Matthews

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

#ifdef HAVE_CONFIG_H
#include "config.h"
#elif defined(_WIN32)
#include "winconfig.h"
#else
#error "No configuration file"
#endif

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#ifdef HAVE_ASSERT_H
#include <assert.h>
#define ASSERT(x) assert(x)

#else
#define ASSERT(x)
#endif

#include "rts_module.h"

#define MAX_MODULES 30

static RtsModule *module_table[MAX_MODULES];
static unsigned modCount = 0;

// Add a module to the table.  This will be done during the static
// initialisation phase.
void RtsModule::RegisterModule(void)
{
    ASSERT(modCount < MAX_MODULES);
    module_table[modCount++] = this;
}

void InitModules(void)
{
    for(unsigned i = 0; i < modCount; i++)
        module_table[i]->Init();
}

void StartModules(void)
{
    for(unsigned i = 0; i < modCount; i++)
        module_table[i]->Start();
}

void StopModules(void)
{
    for(unsigned i = 0; i < modCount; i++)
        module_table[i]->Stop();
}

void GCModules(ScanAddress *process)
{
    for(unsigned i = 0; i < modCount; i++)
        module_table[i]->GarbageCollect(process);
}

// Called on Unix in the child process.
void ForkChildModules(void)
{
    for (unsigned i = 0; i < modCount; i++)
        module_table[i]->ForkChild();
}
