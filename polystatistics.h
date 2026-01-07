/*
    Title:  polystatics.h - Layout of statistics data in shared memory
    Copyright (c) 2011, 2019-20 David C.J. Matthews

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

#ifndef POLY_STATISTICS_INCLUDED
#define POLY_STATISTICS_INCLUDED

#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif

// Name of shared memory file.  This has the process ID appended.
// In Unix this may be in the the $POLYSTATSDIR or $HOME/.polyml directories.
#define POLY_STATS_NAME "poly-stats-"

// ASN1 tags for the statistics
#define POLY_STATS_C_STATISTICS     0x60    // Application 0 - Implicit set
#define POLY_STATS_C_COUNTERSTAT    0x61    // Application 1 - Implicit sequence
#define POLY_STATS_C_SIZESTAT       0x62    // Application 2 - Implicit sequence
#define POLY_STATS_C_TIMESTAT       0x63    // Application 3 - Implicit sequence
#define POLY_STATS_C_IDENTIFIER     0x44    // Application 4 - Implicit integer
#define POLY_STATS_C_NAME           0x45    // Application 5 - Implicit visible string
#define POLY_STATS_C_COUNTER_VALUE  0x46    // Application 6 - Implicit integer
#define POLY_STATS_C_BYTE_COUNT     0x47    // Application 7 - Implicit integer
#define POLY_STATS_C_TIME           0x68    // Application 8 - Implicit sequence
#define POLY_STATS_C_SECONDS        0x49    // Application 9 - Implicit integer
#define POLY_STATS_C_MICROSECS      0x4A    // Application 10 - Implicit integer
#define POLY_STATS_C_USERSTAT       0x6B    // Application 11 - Implicit sequence

// Identifiers for the particular statistics
#define POLY_STATS_ID_THREADS                 1   // Total number of threads
#define POLY_STATS_ID_THREADS_IN_ML           2   // Threads running ML code
#define POLY_STATS_ID_THREADS_WAIT_IO         3   // Threads waiting for IO
#define POLY_STATS_ID_THREADS_WAIT_MUTEX      4   // Threads waiting for a mutex
#define POLY_STATS_ID_THREADS_WAIT_CONDVAR    5   // Threads waiting for a condition var
#define POLY_STATS_ID_THREADS_WAIT_SIGNAL     6   // Special case - signal handling thread
#define POLY_STATS_ID_GC_FULLGC               7   // Number of full garbage collections
#define POLY_STATS_ID_GC_PARTIALGC            8   // Number of partial GCs

#define POLY_STATS_ID_TOTAL_HEAP              9   // Total size of the local heap
#define POLY_STATS_ID_AFTER_LAST_GC          10   // Space free after last GC
#define POLY_STATS_ID_AFTER_LAST_FULLGC      11    // Space free after the last full GC
#define POLY_STATS_ID_ALLOCATION             12    // Size of allocation space
#define POLY_STATS_ID_ALLOCATION_FREE        13    // Space available in allocation area

#define POLY_STATS_ID_NONGC_UTIME            14
#define POLY_STATS_ID_NONGC_STIME            15
#define POLY_STATS_ID_GC_UTIME               16
#define POLY_STATS_ID_GC_STIME               17

#define POLY_STATS_ID_USER0                  18
#define POLY_STATS_ID_USER1                  19
#define POLY_STATS_ID_USER2                  20
#define POLY_STATS_ID_USER3                  21
#define POLY_STATS_ID_USER4                  22
#define POLY_STATS_ID_USER5                  23
#define POLY_STATS_ID_USER6                  24
#define POLY_STATS_ID_USER7                  25

#define POLY_STATS_ID_NONGC_RTIME            26     // Real time apart from GC
#define POLY_STATS_ID_GC_RTIME               27     // Real time in GC
#define POLY_STATS_ID_GC_SHARING             28     // Number of sharing passes
#define POLY_STATS_ID_CODE_SPACE             29     // Space occupied by code
#define POLY_STATS_ID_STACK_SPACE            30     // Space occupied by stacks

#define POLY_STATS_ID_GC_STATE               31
#define POLY_STATS_ID_GC_PERCENT             32

#endif // POLY_STATISTICS_INCLUDED


