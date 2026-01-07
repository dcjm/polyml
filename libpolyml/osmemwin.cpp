/*
    Title:  osomem.cpp - Interface to OS memory management - Windows version

    Copyright (c) 2006, 2017-18, 2020 David C.J. Matthews

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

#ifdef HAVE_CONFIG_H
#include "config.h"
#elif defined(_WIN32)
#include "winconfig.h"
#else
#error "No configuration file"
#endif

#ifdef HAVE_ASSERT_H
#include <assert.h>
#define ASSERT(x)   assert(x)
#else
#define ASSERT(x)
#endif

#include "osmem.h"
#include "bitmap.h"
#include "locking.h"

// Use Windows memory management.
#include <windows.h>

OSMem::OSMem()
{
}

OSMem::~OSMem()
{
}

bool OSMem::Initialise(enum _MemUsage usage)
{
    memUsage = usage;
    // Get the page size and round up to that multiple.
    SYSTEM_INFO sysInfo;
    GetSystemInfo(&sysInfo);
    // Get the page size.  Put it in a size_t variable otherwise the rounding
    // up of "space" may go wrong on 64-bits.
    pageSize = sysInfo.dwPageSize;
    return true;
}

bool OSMemInRegion::Initialise(enum _MemUsage usage, size_t space /* = 0 */, void** pBase /* = 0 */)
{
    if (!OSMem::Initialise(usage))
        return false;

    memBase = (char*)VirtualAlloc(0, space, MEM_RESERVE, PAGE_NOACCESS);
    if (memBase == 0) return 0;
    // We need the heap to be such that the top 32-bits are non-zero.
    if ((uintptr_t)memBase < ((uintptr_t)1 << 32))
    {
        // Allocate again.
        void* newSpace = VirtualAlloc(0, space, MEM_RESERVE, PAGE_NOACCESS);
        VirtualFree(memBase, 0, MEM_RELEASE); // Free the old area that isn't suitable.
        // Return what we got, or zero if it failed.
        memBase = (char*)newSpace;
    }

    if (pBase != 0) *pBase = memBase;

    // Create a bitmap with a bit for each page.
    if (!pageMap.Create(space / pageSize))
        return false;
    lastAllocated = space / pageSize; // Beyond the last page in the area
    // Set the last bit in the area so that we don't use it.
    // This is effectively a work-around for a problem with the heap.
    // If we have a zero-sized cell at the end of the memory its address is
    // going to be zero.  This causes problems with forwarding pointers.
    // There may be better ways of doing this.
    pageMap.SetBit(space / pageSize - 1);
    return true;
}

void* OSMemInRegion::AllocateDataArea(size_t& space)
{
    char* baseAddr;
    {
        PLocker l(&bitmapLock);
        uintptr_t pages = (space + pageSize - 1) / pageSize;
        // Round up to an integral number of pages.
        space = pages * pageSize;
        // Find some space
        while (pageMap.TestBit(lastAllocated - 1)) // Skip the wholly allocated area.
            lastAllocated--;
        uintptr_t free = pageMap.FindFree(0, lastAllocated, pages);
        if (free == lastAllocated)
            return 0; // Can't find the space.
        pageMap.SetBits(free, pages);
        // TODO: Do we need to zero this?  It may have previously been set.
        baseAddr = memBase + free * pageSize;
    }
    return VirtualAlloc(baseAddr, space, MEM_COMMIT, PAGE_READWRITE);
}

bool OSMemInRegion::FreeDataArea(void* p, size_t space)
{
    char* addr = (char*)p;
    uintptr_t offset = (addr - memBase) / pageSize;
    if (!VirtualFree(p, space, MEM_DECOMMIT))
        return false;
    uintptr_t pages = space / pageSize;
    {
        PLocker l(&bitmapLock);
        pageMap.ClearBits(offset, pages);
        if (offset + pages > lastAllocated) // We allocate from the top down.
            lastAllocated = offset + pages;
    }
    return true;
}

void* OSMemInRegion::AllocateCodeArea(size_t& space, void*& shadowArea)
{
    char* baseAddr;
    {
        PLocker l(&bitmapLock);
        uintptr_t pages = (space + pageSize - 1) / pageSize;
        // Round up to an integral number of pages.
        space = pages * pageSize;
        // Find some space
        while (pageMap.TestBit(lastAllocated - 1)) // Skip the wholly allocated area.
            lastAllocated--;
        uintptr_t free = pageMap.FindFree(0, lastAllocated, pages);
        if (free == lastAllocated)
            return 0; // Can't find the space.
        pageMap.SetBits(free, pages);
        // TODO: Do we need to zero this?  It may have previously been set.
        baseAddr = memBase + free * pageSize;
    }

    void* dataArea =
        VirtualAlloc(baseAddr, space, MEM_COMMIT, memUsage == UsageExecutableCode ? PAGE_EXECUTE_READWRITE : PAGE_READWRITE);
    shadowArea = dataArea;
    return dataArea;
}

bool OSMemInRegion::FreeCodeArea(void* codeAddr, void* dataAddr, size_t space)
{
    ASSERT(codeAddr == dataAddr);
    char* addr = (char*)codeAddr;
    uintptr_t offset = (addr - memBase) / pageSize;
    if (! VirtualFree(codeAddr, space, MEM_DECOMMIT))
        return false;
    uintptr_t pages = space / pageSize;
    {
        PLocker l(&bitmapLock);
        pageMap.ClearBits(offset, pages);
        if (offset + pages > lastAllocated) // We allocate from the top down.
            lastAllocated = offset + pages;
    }
    return true;
}

bool OSMemInRegion::EnableWrite(bool enable, void* p, size_t space)
{
    DWORD oldProtect;
    return VirtualProtect(p, space, enable ? PAGE_READWRITE : PAGE_READONLY, &oldProtect) == TRUE;
}

bool OSMemInRegion::DisableWriteForCode(void* codeAddr, void* dataAddr, size_t space)
{
    ASSERT(codeAddr == dataAddr);
    DWORD oldProtect;
    return VirtualProtect(codeAddr, space,
        memUsage == UsageExecutableCode ? PAGE_EXECUTE_READ : PAGE_READONLY, &oldProtect) == TRUE;
}

// Allocate space and return a pointer to it.  The size is the minimum
// size requested and it is updated with the actual space allocated.
// Returns NULL if it cannot allocate the space.
void *OSMemUnrestricted::AllocateDataArea(size_t &space)
{
    space = (space + pageSize - 1) & ~(pageSize - 1);
    DWORD options = MEM_RESERVE | MEM_COMMIT;
    return VirtualAlloc(0, space, options, PAGE_READWRITE);
}

// Release the space previously allocated.  This must free the whole of
// the segment.  The space must be the size actually allocated.
bool OSMemUnrestricted::FreeDataArea(void *p, size_t space)
{
    return VirtualFree(p, 0, MEM_RELEASE) == TRUE;
}

// Adjust the permissions on a segment.  This must apply to the
// whole of a segment.
bool OSMemUnrestricted::EnableWrite(bool enable, void* p, size_t space)
{
    DWORD oldProtect;
    return VirtualProtect(p, space, enable ? PAGE_READWRITE: PAGE_READONLY, &oldProtect) == TRUE;
}

void* OSMemUnrestricted::AllocateCodeArea(size_t& space, void*& shadowArea)
{
    space = (space + pageSize - 1) & ~(pageSize - 1);
    DWORD options = MEM_RESERVE | MEM_COMMIT;
    void * dataAddr = VirtualAlloc(0, space, options,
        memUsage == UsageExecutableCode ? PAGE_EXECUTE_READWRITE : PAGE_READWRITE);
    shadowArea = dataAddr;
    return dataAddr;
}

bool OSMemUnrestricted::FreeCodeArea(void* codeAddr, void* dataAddr, size_t space)
{
    ASSERT(codeAddr == dataAddr);
    return VirtualFree(codeAddr, 0, MEM_RELEASE) == TRUE;
}

bool OSMemUnrestricted::DisableWriteForCode(void* codeAddr, void* dataAddr, size_t space)
{
    ASSERT(codeAddr == dataAddr);
    DWORD oldProtect;
    return VirtualProtect(codeAddr, space,
        memUsage == UsageExecutableCode ? PAGE_EXECUTE_READ : PAGE_READONLY, &oldProtect) == TRUE;
}
