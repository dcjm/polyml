/*
    Title:  gc_progress.cpp - Garbage collection progress data

    Copyright (c) 2019 David C.J. Matthews

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

#include "statistics.h"
#include "gc_progress.h"

// These values are coded in Statistics.ML
enum {
    GCP_ML = 0,     // In ML Code
    GCP_MINOR,      // In minor GC
    GCP_MAJOR,      // In major GC
    GCP_SHARING,    // In GC Sharing pass
    GCP_OTHER       // In something else that suspends ML e.g. shareCommonData
};


void gcProgressReturnToML()
{
    globalStats.setCount(PSC_GC_STATE, GCP_ML);
    globalStats.setCount(PSC_GC_PERCENT, 0);
}

void gcProgressBeginMinorGC()
{
    globalStats.setCount(PSC_GC_STATE, GCP_MINOR);
    globalStats.setCount(PSC_GC_PERCENT, 0);
}

void gcProgressBeginMajorGC()
{
    globalStats.setCount(PSC_GC_STATE, GCP_MAJOR);
    globalStats.setCount(PSC_GC_PERCENT, 0);
}

void gcProgressBeginSharingGC()
{
    globalStats.setCount(PSC_GC_STATE, GCP_SHARING);
    globalStats.setCount(PSC_GC_PERCENT, 0);
}

void gcProgressBeginOtherGC()
{
    globalStats.setCount(PSC_GC_STATE, GCP_OTHER);
    globalStats.setCount(PSC_GC_PERCENT, 0);
}

void gcProgressSetPercent(unsigned pc)
{
	globalStats.setCount(PSC_GC_PERCENT, pc);
}
