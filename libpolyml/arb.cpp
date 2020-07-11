/*
    Title:      Arbitrary Precision Package.
    Author:     Dave Matthews, Cambridge University Computer Laboratory

    Further modification Copyright 2010, 2012, 2015, 2017 David C. J. Matthews

    Copyright (c) 2000
        Cambridge University Technical Services Limited

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

/*
Arbitrary precision package in C.

Integers are held in two formats in this system, long-form and short-form.
The two are distinquished by the integer tag bit, short-form having the tag
bit set and pointers to long-form being untagged.
The long-form integers use the standard Poly format for multi-word
objects, with the length count and flags in the word just before the object
pointed to.  The sign of long-form integers is coded in one of the flag bits.
Short integers are signed quantities, and can be directly
manipulated by the relevant instructions, but if overflow occurs then the full
long versions of the operations will need to be called.
There are two versions of long-form integers depending on whether the GMP
library is available.  If it is then the byte cells contain "limbs",
typically native 32 or 64-bit words.  If it is not, the fall-back
Poly code is used in which long-form integers are vectors of
bytes (i.e. unsigned char).
Integers are always stored in the least possible number of words, and
will be shortened to the short-form when possible.

Thanks are due to D. Knuth for the long division algorithm.
*/

#ifdef HAVE_CONFIG_H
#include "config.h"
#elif defined(_WIN32)
#include "winconfig.h"
#else
#error "No configuration file"
#endif

#ifdef HAVE_STDIO_H
#include <stdio.h>
#endif

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#ifdef HAVE_ALLOCA_H
#include <alloca.h>
#endif

#ifdef HAVE_MALLOC_H
#include <malloc.h>
#endif

#ifdef HAVE_ASSERT_H
#include <assert.h>
#define ASSERT(x)   assert(x)
#else
#define ASSERT(x)
#endif

#ifdef HAVE_GMP_H
#include <gmp.h>
#define USE_GMP 1
#endif

#include "globals.h"
#include "sys.h"
#include "run_time.h"
#include "arb.h"
#include "save_vec.h"
#include "processes.h"
#include "memmgr.h"
#include "rtsentry.h"
#include "profiling.h"

extern "C" {
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyAddArbitrary(PolyObject *threadId, PolyWord arg1, PolyWord arg2);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolySubtractArbitrary(PolyObject *threadId, PolyWord arg1, PolyWord arg2);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyMultiplyArbitrary(PolyObject *threadId, PolyWord arg1, PolyWord arg2);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyDivideArbitrary(PolyObject *threadId, PolyWord arg1, PolyWord arg2);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyRemainderArbitrary(PolyObject *threadId, PolyWord arg1, PolyWord arg2);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyQuotRemArbitrary(PolyObject *threadId, PolyWord arg1, PolyWord arg2, PolyWord arg3);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyQuotRemArbitraryPair(PolyObject *threadId, PolyWord arg1, PolyWord arg2);
    POLYEXTERNALSYMBOL POLYSIGNED PolyCompareArbitrary(PolyWord arg1, PolyWord arg2);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyGCDArbitrary(PolyObject *threadId, PolyWord arg1, PolyWord arg2);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyLCMArbitrary(PolyObject *threadId, PolyWord arg1, PolyWord arg2);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyGetLowOrderAsLargeWord(PolyObject *threadId, PolyWord arg);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyOrArbitrary(PolyObject *threadId, PolyWord arg1, PolyWord arg2);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyAndArbitrary(PolyObject *threadId, PolyWord arg1, PolyWord arg2);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyXorArbitrary(PolyObject *threadId, PolyWord arg1, PolyWord arg2);
}

static Handle mult_longc(TaskData* taskData, Handle, Handle);
static Handle or_longc(TaskData *taskData, Handle,Handle);
static Handle and_longc(TaskData *taskData, Handle,Handle);
static Handle xor_longc(TaskData *taskData, Handle,Handle);
static Handle neg_longc(TaskData *taskData, Handle);

static Handle gcd_arbitrary(TaskData *taskData, Handle,Handle);
static Handle lcm_arbitrary(TaskData *taskData, Handle,Handle);

// Number of bits in a Poly word.  N.B.  This is not necessarily the same as SIZEOF_VOIDP.
#define BITS_PER_POLYWORD (SIZEOF_POLYWORD*8)

#ifdef USE_GMP
#if (BITS_PER_POLYWORD > GMP_LIMB_BITS)
// We're assuming that every GMP limb occupies at least one word
#error "Size of GMP limb is less than a Poly word"
#endif
#endif

#ifdef USE_GMP
#define DEREFLIMBHANDLE(_x)      ((mp_limb_t *)DEREFHANDLE(_x))

// Returns the length of the argument with trailing zeros removed.
static mp_size_t numLimbs(PolyWord x)
{
    POLYUNSIGNED numWords = OBJECT_LENGTH(x);
#if BITS_PER_POLYWORD != GMP_LIMB_BITS
    ASSERT((numWords & (sizeof(mp_limb_t)/sizeof(PolyWord)-1)) == 0);
#endif
    mp_size_t lu = numWords*sizeof(PolyWord)/sizeof(mp_limb_t);
    mp_limb_t *u = (mp_limb_t *)x.AsObjPtr();
    while (lu > 0 && u[lu-1] == 0) lu--;
    return lu;
}

#else
// Returns the length of the argument with trailing zeros removed.

static POLYUNSIGNED get_length(PolyWord x)
{
    byte *u = (byte *)x.AsObjPtr();
    POLYUNSIGNED  lu  = OBJECT_LENGTH(x)*sizeof(PolyWord);

    for( ; (lu > 0) && (u[lu-1] == 0); lu--)
    {
        /* do nothing */
    }

    return lu;
}
#endif

// Return a uintptr_t value i.e. unsigned 32-bits on 32-bit architecture and 64-bits on 64-bit architecture.
POLYUNSIGNED getPolyUnsigned(TaskData *taskData, PolyWord number)
{
    if ( IS_INT(number) )
    {
        POLYSIGNED i = UNTAGGED(number);
        if ( i < 0 ) raise_exception0(taskData, EXC_size );
        return i;
    }
    else
    {
        if (OBJ_IS_NEGATIVE(GetLengthWord(number))) raise_exception0(taskData, EXC_size );
#ifdef USE_GMP
        unsigned length = numLimbs(number);
        if (length > 1) raise_exception0(taskData, EXC_size);
        mp_limb_t first = *(mp_limb_t*)number.AsCodePtr();
#if (BITS_PER_POLYWORD < GMP_NUMB_BITS)
        if (first > (mp_limb_t)1 << BITS_PER_POLYWORD)
            raise_exception0(taskData, EXC_size);
#endif
        return first;
#else
        byte *ptr = number.AsCodePtr();
        POLYUNSIGNED length = get_length(number);
        if (length > sizeof(PolyWord) ) raise_exception0(taskData, EXC_size);
        POLYSIGNED c = 0;
        while ( length-- ) c = (c << 8) | ((byte *) ptr)[length];
        return c;
#endif
    }
}

#define MAX_INT_PLUS1   ((POLYUNSIGNED)0x80 << ( (sizeof(PolyWord)-1) *8))
// Return an intptr_t value i.e. signed 32-bits on 32-bit architecture and 64-bits on 64-bit architecture.
POLYSIGNED getPolySigned(TaskData *taskData, PolyWord number)
{
    if ( IS_INT(number) )
    {
        return UNTAGGED(number);
    }
    else
    {
        int sign   = OBJ_IS_NEGATIVE(GetLengthWord(number)) ? -1 : 0;
#ifdef USE_GMP
        unsigned length = numLimbs(number);
        if (length > 1) raise_exception0(taskData, EXC_size);
        mp_limb_t c = *(mp_limb_t*)number.AsCodePtr();
#else
        POLYUNSIGNED length = get_length(number);
        POLYUNSIGNED c = 0;
        byte *ptr = number.AsCodePtr();

        if ( length > sizeof(PolyWord) ) raise_exception0(taskData, EXC_size );

        while ( length-- )
        {
            c = (c << 8) | ptr[length];
        }
#endif
        if ( sign == 0 && c <  MAX_INT_PLUS1) return   (POLYSIGNED)c;
        if ( sign != 0 && c <= MAX_INT_PLUS1) return -((POLYSIGNED)c);

        raise_exception0(taskData, EXC_size );
        /*NOTREACHED*/
        return 0;
    }
}

short get_C_short(TaskData *taskData, PolyWord number)
{
    int i = (int)get_C_long(taskData, number);

    if ( i <= 32767 && i >= -32768 ) return i;

    raise_exception0(taskData, EXC_size );
    /*NOTREACHED*/
    return 0;
}

unsigned short get_C_ushort(TaskData *taskData, PolyWord number)
{
    POLYUNSIGNED u = get_C_ulong(taskData, number );

    if ( u <= 65535 ) return (short)u;

    raise_exception0(taskData, EXC_size );
    /*NOTREACHED*/
    return 0;
}

#if (SIZEOF_LONG == SIZEOF_POLYWORD)
unsigned get_C_unsigned(TaskData *taskData, PolyWord number)
{
    return get_C_ulong(taskData, number);
}

int get_C_int(TaskData *taskData, PolyWord number)
{
    return get_C_long(taskData, number);
}
#else
// Poly words are the same size as a pointer but that may
// not be the same as int or long.
unsigned get_C_unsigned(TaskData *taskData, PolyWord number)
{
    POLYUNSIGNED res = get_C_ulong(taskData, number);
    unsigned result = (unsigned)res;
    if ((POLYUNSIGNED)result != res)
        raise_exception0(taskData, EXC_size);
    return result;
}

int get_C_int(TaskData *taskData, PolyWord number)
{
    POLYSIGNED res = get_C_long(taskData, number);
    int result = (int)res;
    if ((POLYSIGNED)result != res)
        raise_exception0(taskData, EXC_size);
    return result;

}
#endif

// Convert short values to long.  Returns a pointer to the memory.
// This is generally called before allocating memory for the result.
// It is unsafe to use the result after the allocation if the value is
// an address because it may have been moved by a GC.
#ifdef USE_GMP
static mp_limb_t *convertToLong(Handle x, mp_limb_t *extend, mp_size_t *length, int *sign)
{
    if (IS_INT(x->Word()))
    {
        // Short form - put it in the temporary.
        POLYSIGNED x_v = UNTAGGED(DEREFWORD(x));
        if (x_v < 0) x_v = -x_v;
        *extend = x_v;
        if (x_v == 0) *length = 0; else *length = 1;
        if (sign) *sign = UNTAGGED(x->Word()) >= 0 ? 0 : -1;
        return extend;
    }
    else
    {
        *length = numLimbs(x->Word());
        if (sign) *sign = OBJ_IS_NEGATIVE(GetLengthWord(x->Word())) ? -1 : 0;
        return DEREFLIMBHANDLE(x);
    }
}
#else
static byte *convertToLong(Handle x, byte *extend, POLYUNSIGNED *length, int *sign)
{
    if (IS_INT(x->Word()))
    {
        // Short form - put it in the temporary.
        POLYSIGNED x_v = UNTAGGED(DEREFWORD(x));
        if (x_v < 0) x_v = -x_v;
        /* Put into extend buffer, low order byte first. */
        *length = 0;
        for (unsigned i = 0; i < sizeof(PolyWord); i++)
        {
            if (x_v != 0) *length = i + 1;
            extend[i] = x_v & 0xff;
            x_v = x_v >> 8;
        }
        if (sign) *sign = UNTAGGED(x->Word()) >= 0 ? 0 : -1;
        return extend;
    }
    else
    {
        *length = get_length(DEREFWORD(x));
        if (sign) *sign = OBJ_IS_NEGATIVE(GetLengthWord(x->Word())) ? -1 : 0;
        return DEREFBYTEHANDLE(x);
    }
}
#endif

/* make_canonical is used to force a result into its shortest form,
   in the style of get_length, but also may convert its argument
   from long to short integer */
static Handle make_canonical(TaskData *taskData, Handle x, int sign)
{
#ifdef USE_GMP
    unsigned size = numLimbs(DEREFWORD(x));
    if (size <= 1) // May be zero if the result is zero.
    {
        mp_limb_t r = *DEREFLIMBHANDLE(x);
        if (r <= MAXTAGGED || (r == MAXTAGGED+1 && sign < 0))
        {
            if (sign < 0)
                return taskData->saveVec.push(TAGGED(-(POLYSIGNED)r));
            else
                return taskData->saveVec.push(TAGGED(r));
        }
    }
    // Throw away any unused words.
    DEREFWORDHANDLE(x)->SetLengthWord(WORDS(size*sizeof(mp_limb_t)), F_BYTE_OBJ | (sign < 0 ? F_NEGATIVE_BIT: 0));
    return x;
#else
    /* get length in BYTES */
    POLYUNSIGNED size = get_length(DEREFWORD(x));

    // We can use the short representation if it will fit in a word.
    if (size <= sizeof(PolyWord))
    {
        /* Convert the digits. */
        byte    *u = DEREFBYTEHANDLE(x);
        POLYUNSIGNED r = 0;
        for (unsigned i=0; i < sizeof(PolyWord); i++)
        {
            r |= ((POLYUNSIGNED)u[i]) << (8*i);
        }

        /* Check for MAXTAGGED+1 before subtraction
           in case MAXTAGGED is 0x7fffffff */

        if (r <= MAXTAGGED || (r == MAXTAGGED+1 && sign < 0))
        {
            if (sign < 0)
                return taskData->saveVec.push(TAGGED(-(POLYSIGNED)r));
            else
                return taskData->saveVec.push(TAGGED(r));
        }
    }

    /* The length word of the object is changed to reflect the new length.
       This is safe because any words thrown away must be zero. */
    DEREFWORDHANDLE(x)->SetLengthWord(WORDS(size), F_BYTE_OBJ | (sign < 0 ? F_NEGATIVE_BIT: 0));

    return x;
#endif
}

Handle ArbitraryPrecionFromSigned(TaskData *taskData, POLYSIGNED val)
/* Called from routines in the run-time system to generate an arbitrary
   precision integer from a word value. */
{
    if (val <= MAXTAGGED && val >= -MAXTAGGED-1) /* No overflow */
        return taskData->saveVec.push(TAGGED(val));

    POLYUNSIGNED uval = val < 0 ? -val : val;
#ifdef USE_GMP
    Handle y = alloc_and_save(taskData, WORDS(sizeof(mp_limb_t)), ((val < 0) ? F_NEGATIVE_BIT : 0)| F_BYTE_OBJ);
    mp_limb_t *v = DEREFLIMBHANDLE(y);
    *v = uval;
#else
    Handle y = alloc_and_save(taskData, 1, ((val < 0) ? F_NEGATIVE_BIT : 0)| F_BYTE_OBJ);
    byte *v = DEREFBYTEHANDLE(y);
    for (POLYUNSIGNED i = 0; uval != 0; i++)
    {
        v[i] = (byte)(uval & 0xff);
        uval >>= 8;
    }
#endif
    return y;
}

Handle ArbitraryPrecionFromUnsigned(TaskData *taskData, POLYUNSIGNED uval)
/* Called from routines in the run-time system to generate an arbitrary
   precision integer from an unsigned value. */
{
    if (uval <= MAXTAGGED) return taskData->saveVec.push(TAGGED(uval));
 #ifdef USE_GMP
    Handle y = alloc_and_save(taskData, WORDS(sizeof(mp_limb_t)), F_BYTE_OBJ);
    mp_limb_t *v = DEREFLIMBHANDLE(y);
    *v = uval;
#else
    Handle y = alloc_and_save(taskData, 1, F_BYTE_OBJ);
    byte *v = DEREFBYTEHANDLE(y);
    for (POLYUNSIGNED i = 0; uval != 0; i++)
    {
        v[i] = (byte)(uval & 0xff);
        uval >>= 8;
    }
#endif
    return y;
}


Handle Make_arbitrary_precision(TaskData *taskData, int val)
{
    return ArbitraryPrecionFromSigned(taskData, val);
}

Handle Make_arbitrary_precision(TaskData *taskData, unsigned uval)
{
    return ArbitraryPrecionFromUnsigned(taskData, uval);
}

#if (SIZEOF_LONG <= SIZEOF_POLYWORD)
Handle Make_arbitrary_precision(TaskData *taskData, long val)
{
    return ArbitraryPrecionFromSigned(taskData, val);
}

Handle Make_arbitrary_precision(TaskData *taskData, unsigned long uval)
{
    return ArbitraryPrecionFromUnsigned(taskData, uval);
}
#else
// This is needed in Unix in 32-in-64.
Handle Make_arbitrary_precision(TaskData *taskData, long val)
{
    if (val <= (long)(MAXTAGGED) && val >= -((long)(MAXTAGGED))-1) /* No overflow */
        return taskData->saveVec.push(TAGGED((POLYSIGNED)val));
    // Recursive call to handle the high-order part
    Handle hi = Make_arbitrary_precision(taskData, val >> (sizeof(int32_t) * 8));
    // The low-order part is treated as UNsigned.
    Handle lo = Make_arbitrary_precision(taskData, (uint32_t)val);
    Handle twoTo16 = taskData->saveVec.push(TAGGED(65536));
    Handle twoTo32 = mult_longc(taskData, twoTo16, twoTo16);
    return add_longc(taskData, mult_longc(taskData, hi, twoTo32), lo);
}

Handle Make_arbitrary_precision(TaskData *taskData, unsigned long uval)
{
    if (uval <= (unsigned long)(MAXTAGGED))
        return taskData->saveVec.push(TAGGED((POLYUNSIGNED)uval));
    // Recursive call to handle the high-order part
    Handle hi = Make_arbitrary_precision(taskData, uval >> (sizeof(uint32_t) * 8));
    Handle lo = Make_arbitrary_precision(taskData, (uint32_t)uval);
    Handle twoTo16 = taskData->saveVec.push(TAGGED(65536));
    Handle twoTo32 = mult_longc(taskData, twoTo16, twoTo16);
    return add_longc(taskData, mult_longc(taskData, hi, twoTo32), lo);
}

#endif

#ifdef HAVE_LONG_LONG
#if (SIZEOF_LONG_LONG <= SIZEOF_POLYWORD)
Handle Make_arbitrary_precision(TaskData *taskData, long long val)
{
    return ArbitraryPrecionFromSigned(taskData, val);
}

Handle Make_arbitrary_precision(TaskData *taskData, unsigned long long uval)
{
    return ArbitraryPrecionFromUnsigned(taskData, uval);
}
#else
// 32-bit implementation.
Handle Make_arbitrary_precision(TaskData *taskData, long long val)
{
    if (val <= (long long)(MAXTAGGED) && val >= -((long long)(MAXTAGGED))-1) /* No overflow */
        return taskData->saveVec.push(TAGGED((POLYSIGNED)val));
    // Recursive call to handle the high-order part
    Handle hi = Make_arbitrary_precision(taskData, val >> (sizeof(int32_t) * 8));
    // The low-order part is treated as UNsigned.
    Handle lo = Make_arbitrary_precision(taskData, (uint32_t)val);
    Handle twoTo16 = taskData->saveVec.push(TAGGED(65536));
    Handle twoTo32 = mult_longc(taskData, twoTo16, twoTo16);
    return add_longc(taskData, mult_longc(taskData, hi, twoTo32), lo);
}

Handle Make_arbitrary_precision(TaskData *taskData, unsigned long long uval)
{
    if (uval <= (unsigned long long)(MAXTAGGED))
        return taskData->saveVec.push(TAGGED((POLYUNSIGNED)uval));
    // Recursive call to handle the high-order part
    Handle hi = Make_arbitrary_precision(taskData, uval >> (sizeof(uint32_t) * 8));
    Handle lo = Make_arbitrary_precision(taskData, (uint32_t)uval);
    Handle twoTo16 = taskData->saveVec.push(TAGGED(65536));
    Handle twoTo32 = mult_longc(taskData, twoTo16, twoTo16);
    return add_longc(taskData, mult_longc(taskData, hi, twoTo32), lo);
}
#endif
#endif

#if defined(_WIN32)
// Creates an arbitrary precision number from two words.
// Used only in Windows for FILETIME and file-size.
Handle Make_arb_from_32bit_pair(TaskData *taskData, uint32_t hi, uint32_t lo)
{
    Handle hHi = Make_arbitrary_precision(taskData, hi);
    Handle hLo = Make_arbitrary_precision(taskData, lo);
    Handle twoTo16 = taskData->saveVec.push(TAGGED(65536));
    Handle twoTo32 = mult_longc(taskData, twoTo16, twoTo16);
    return add_longc(taskData, mult_longc(taskData, hHi, twoTo32), hLo);
}

// Convert a Windows FILETIME into an arbitrary precision integer
Handle Make_arb_from_Filetime(TaskData *taskData, const FILETIME &ft)
{
    return Make_arb_from_32bit_pair(taskData, ft.dwHighDateTime, ft.dwLowDateTime);
}
#endif

/* Returns hi*scale+lo as an arbitrary precision number.  Currently used
   for Unix time values where the time is returned as two words, a number
   of seconds and a number of microseconds and we wish to return the
   result as a number of microseconds. */
Handle Make_arb_from_pair_scaled(TaskData *taskData, unsigned hi, unsigned lo, unsigned scale)
{
   /* We might be able to compute the number as a 64 bit quantity and
      then convert it but this is probably more portable. It does risk
      overflowing the save vector. */
    Handle hHi = Make_arbitrary_precision(taskData, hi);
    Handle hLo = Make_arbitrary_precision(taskData, lo);
    Handle hScale = Make_arbitrary_precision(taskData, scale);
    return add_longc(taskData, mult_longc(taskData, hHi, hScale), hLo);
}

Handle neg_longc(TaskData *taskData, Handle x)
{
    if (IS_INT(DEREFWORD(x)))
    {
        POLYSIGNED s = UNTAGGED(DEREFWORD(x));
        if (s != -MAXTAGGED-1) // If it won't overflow
            return taskData->saveVec.push(TAGGED(-s));
    }

    // Either overflow or long argument - convert to long form.
    int sign_x;
#if USE_GMP
    mp_limb_t    x_extend;
    mp_size_t lx;
    (void)convertToLong(x, &x_extend, &lx, &sign_x);
#else
    byte    x_extend[sizeof(PolyWord)];
    POLYUNSIGNED lx;
    (void)convertToLong(x, x_extend, &lx, &sign_x);
#endif

#ifdef USE_GMP
    POLYUNSIGNED bytes = lx*sizeof(mp_limb_t);
#else
    POLYUNSIGNED bytes = lx;
#endif

    Handle long_y = alloc_and_save(taskData, WORDS(bytes), F_MUTABLE_BIT|F_BYTE_OBJ);
    byte *v = DEREFBYTEHANDLE(long_y);
    if (IS_INT(DEREFWORD(x)))
        memcpy(v, &x_extend, bytes);
    else memcpy(v, DEREFBYTEHANDLE(x), bytes);
#ifndef USE_GMP
    // Make sure the last word is zero.  We may have unused bytes there.
    memset(v+bytes, 0, WORDS(bytes)*sizeof(PolyWord)-lx);
#endif

    /* Return the value with the sign changed. */
    return make_canonical(taskData, long_y, sign_x ^ -1);
} /* neg_longc */

#ifdef USE_GMP
static Handle add_unsigned_long(TaskData *taskData, Handle x, Handle y, int sign)
{
    /* find the longer number */
    mp_size_t lx, ly;
    mp_limb_t    x_extend, y_extend;
    mp_limb_t *xb = convertToLong(x, &x_extend, &lx, NULL);
    mp_limb_t *yb = convertToLong(y, &y_extend, &ly, NULL);

    mp_limb_t *u; /* limb-pointer for longer number  */
    mp_limb_t *v; /* limb-pointer for shorter number */
    Handle z;
    mp_size_t lu;   /* length of u in limbs */
    mp_size_t lv;   /* length of v in limbs */

    if (lx < ly)
    {
        // Get result vector. It must be 1 limb longer than u
        // to have space for any carry.
        z = alloc_and_save(taskData, WORDS((ly+1)*sizeof(mp_limb_t)), F_MUTABLE_BIT|F_BYTE_OBJ);

        /* now safe to dereference pointers */
        u = IS_INT(DEREFWORD(y)) ? yb : DEREFLIMBHANDLE(y);
        v = IS_INT(DEREFWORD(x)) ? xb : DEREFLIMBHANDLE(x);
        lu = ly;
        lv = lx;
    }
    else
    {
        // Get result vector. It must be 1 limb longer than u
        // to have space for any carry.
        z = alloc_and_save(taskData, WORDS((lx+1)*sizeof(mp_limb_t)), F_MUTABLE_BIT|F_BYTE_OBJ);

        /* now safe to dereference pointers */
        u = IS_INT(DEREFWORD(x)) ? xb : DEREFLIMBHANDLE(x);
        v = IS_INT(DEREFWORD(y)) ? yb : DEREFLIMBHANDLE(y);
        lu = lx;
        lv = ly;
    }

    mp_limb_t *w = DEREFLIMBHANDLE(z);
    // Do the addition.
    mp_limb_t carry = 0;
    if (lv != 0) carry = mpn_add_n(w, u, v, lv);
    // Add the carry to the rest of the longer number.
    if (lu != lv) carry = mpn_add_1(w+lv, u+lv, lu-lv, carry);
    // Put the remaining carry in the final limb.
    w[lu] = carry;
    return make_canonical(taskData, z, sign);
}
#else
static Handle add_unsigned_long(TaskData *taskData, Handle x, Handle y, int sign)
{
    byte    x_extend[sizeof(PolyWord)], y_extend[sizeof(PolyWord)];
    POLYUNSIGNED lx;   /* length of u in bytes */
    POLYUNSIGNED ly;   /* length of v in bytes */

    byte *xb = convertToLong(x, x_extend, &lx, NULL);
    byte *yb = convertToLong(y, y_extend, &ly, NULL);
    Handle z;
    byte *u;  /* byte-pointer for longer number  */
    byte *v; /* byte-pointer for shorter number */
    POLYUNSIGNED lu, lv;

    /* Make ``u'' the longer. */
    if (lx < ly)
    {
        // Get result vector. It must be 1 byte longer than u
        // to have space for any carry.
        z = alloc_and_save(taskData, WORDS(ly+1), F_MUTABLE_BIT|F_BYTE_OBJ);

        /* now safe to dereference pointers */
        u = IS_INT(DEREFWORD(y)) ? yb : DEREFBYTEHANDLE(y);
        v = IS_INT(DEREFWORD(x)) ? xb : DEREFBYTEHANDLE(x);
        lu = ly;
        lv = lx;
    }

    else
    {
        // Get result vector. It must be 1 byte longer than u
        // to have space for any carry, plus one byte for the sign.
        z = alloc_and_save(taskData, WORDS(lx+2), F_MUTABLE_BIT|F_BYTE_OBJ);

        /* now safe to dereference pointers */
        u = IS_INT(DEREFWORD(x)) ? xb : DEREFBYTEHANDLE(x);
        v = IS_INT(DEREFWORD(y)) ? yb : DEREFBYTEHANDLE(y);
        lu = lx;
        lv = ly;
    }

    /* do the actual addition */
    byte  *w = DEREFBYTEHANDLE(z);
    unsigned carry = 0;
    POLYUNSIGNED i     = 0;

    /* Do the additions */
    for( ; i < lv; i++)
    {
        carry += u[i] + v[i];
        w[i] = carry & 0xff;
        carry >>= 8;
    }

    /* Add the carry to the rest of ``u''. */
    for( ; i < lu; i++)
    {
        carry += u[i];
        w[i] = carry & 0xff;
        carry >>= 8;
    }

    /* Finally put the carry into the last byte */
    w[i] = (byte)carry;

    return make_canonical(taskData, z, sign);
} /* add_unsigned_long */
#endif

#ifdef USE_GMP
static Handle sub_unsigned_long(TaskData *taskData, Handle x, Handle y, int sign)
{
    mp_limb_t *u; /* limb-pointer alias for larger number  */
    mp_limb_t *v; /* limb-pointer alias for smaller number */
    mp_size_t lu;   /* length of u in limbs */
    mp_size_t lv;   /* length of v in limbs */
    Handle z;

    /* get the larger argument into ``u'' */
    /* This is necessary so that we can discard */
    /* the borrow at the end of the subtraction */
    mp_size_t lx, ly;
    mp_limb_t    x_extend, y_extend;
    mp_limb_t *xb = convertToLong(x, &x_extend, &lx, NULL);
    mp_limb_t *yb = convertToLong(y, &y_extend, &ly, NULL);

    // Find the larger number.  Check the lengths first and if they're equal check the values.
    int res;
    if (lx < ly) res = -1;
    else if (lx > ly) res = 1;
    else res = mpn_cmp(xb, yb, lx);

    // If they're equal the result is zero.
    if (res == 0) return taskData->saveVec.push(TAGGED(0)); /* They are equal */

    if (res < 0)
    {
        sign ^= -1; /* swap sign of result */
        z = alloc_and_save(taskData, WORDS(ly*sizeof(mp_limb_t)), F_MUTABLE_BIT|F_BYTE_OBJ);

        /* now safe to dereference pointers */
        u = IS_INT(DEREFWORD(y)) ? yb : DEREFLIMBHANDLE(y);
        v = IS_INT(DEREFWORD(x)) ? xb : DEREFLIMBHANDLE(x);
        lu = ly;
        lv = lx;
    }
    else
    {
        z = alloc_and_save(taskData, WORDS(lx*sizeof(mp_limb_t)), F_MUTABLE_BIT|F_BYTE_OBJ);

        /* now safe to dereference pointers */
        u = IS_INT(DEREFWORD(x)) ? xb : DEREFLIMBHANDLE(x);
        v = IS_INT(DEREFWORD(y)) ? yb : DEREFLIMBHANDLE(y);
        lu = lx;
        lv = ly;
    }

    mp_limb_t *w = DEREFLIMBHANDLE(z);
    // Do the subtraction.
    mp_limb_t borrow = 0;
    if (lv != 0) borrow = mpn_sub_n(w, u, v, lv);
    // Subtract the borrow from the rest of the longer number.
    if (lu != lv) borrow = mpn_sub_1(w+lv, u+lv, lu-lv, borrow);
    return make_canonical(taskData, z, sign);
}
#else
static Handle sub_unsigned_long(TaskData *taskData, Handle x, Handle y, int sign)
{
    byte    x_extend[sizeof(PolyWord)], y_extend[sizeof(PolyWord)];
    /* This is necessary so that we can discard */
    /* the borrow at the end of the subtraction */
    POLYUNSIGNED lx, ly;
    byte *xb = convertToLong(x, x_extend, &lx, NULL);
    byte *yb = convertToLong(y, y_extend, &ly, NULL);

    byte *u; /* byte-pointer alias for larger number  */
    byte *v; /* byte-pointer alias for smaller number */
    POLYUNSIGNED lu;   /* length of u in bytes */
    POLYUNSIGNED lv;   /* length of v in bytes */
    Handle z;

    /* get the larger argument into ``u'' */
    if (lx < ly)
    {
        sign ^= -1; // swap sign of result
        z = alloc_and_save(taskData, WORDS(ly+1), F_MUTABLE_BIT|F_BYTE_OBJ);

        /* now safe to dereference pointers */
        u = IS_INT(DEREFWORD(y)) ? yb : DEREFBYTEHANDLE(y);
        v = IS_INT(DEREFWORD(x)) ? xb : DEREFBYTEHANDLE(x);
        lu = ly;
        lv = lx;
    }
    else if (ly < lx)
    {
        z = alloc_and_save(taskData, WORDS(lx+1), F_MUTABLE_BIT|F_BYTE_OBJ);

        /* now safe to dereference pointers */
        u = IS_INT(DEREFWORD(x)) ? xb : DEREFBYTEHANDLE(x);
        v = IS_INT(DEREFWORD(y)) ? yb : DEREFBYTEHANDLE(y);
        lu = lx;
        lv = ly;
    }

    else /* lx == ly */
    { /* Must look at the numbers to decide which is bigger. */
        POLYUNSIGNED i = lx;
        while (i > 0 && xb[i-1] == yb[i-1]) i--;

        if (i == 0) return taskData->saveVec.push(TAGGED(0)); /* They are equal */

        if (xb[i-1] < yb[i-1])
        {
            sign ^= -1; /* swap sign of result SPF 21/1/94 */
            z = alloc_and_save(taskData, WORDS(ly+1), F_MUTABLE_BIT|F_BYTE_OBJ);

            /* now safe to dereference pointers */
            u = IS_INT(DEREFWORD(y)) ? yb : DEREFBYTEHANDLE(y);
            v = IS_INT(DEREFWORD(x)) ? xb : DEREFBYTEHANDLE(x);
            lu = ly;
            lv = lx;
        }
        else
        {
            z = alloc_and_save(taskData, WORDS(lx+1), F_MUTABLE_BIT|F_BYTE_OBJ);

            /* now safe to dereference pointers */
            u = IS_INT(DEREFWORD(x)) ? xb : DEREFBYTEHANDLE(x);
            v = IS_INT(DEREFWORD(y)) ? yb : DEREFBYTEHANDLE(y);
            lu = lx;
            lv = ly;
        }
    }

    byte        *w = DEREFBYTEHANDLE(z);
    unsigned    borrow = 1; /* Becomes 0 if there is a borrow */
    POLYUNSIGNED i = 0;

    /* Do the subtractions */
    for( ; i < lv; i++)
    {
        borrow += 255 + u[i] - v[i];
        w[i] = borrow & 0xff;
        borrow >>= 8;
    }

    /* Add the borrow into the rest of ``u''. */
    for( ; i < lu; i++)
    {
        borrow += 255 + u[i];
        w[i] = borrow & 0xff;
        borrow >>= 8;
    }

    return make_canonical(taskData, z, sign);
} /* sub_unsigned_long */
#endif

Handle add_longc(TaskData *taskData, Handle y, Handle x)
{
    if (IS_INT(DEREFWORD(x)) && IS_INT(DEREFWORD(y)))
    {  /* Both short */
       /* The easiest way to do the addition is simply *x-1+*y, but that
        makes it more difficult to check for overflow. */
        POLYSIGNED t = UNTAGGED(DEREFWORD(x)) + UNTAGGED(DEREFWORD(y));
        if (t <= MAXTAGGED && t >= -MAXTAGGED-1) /* No overflow */
        {
            return taskData->saveVec.push(TAGGED(t));
        }
    }

    int sign_x, sign_y;

    if (IS_INT(DEREFWORD(x))) sign_x = UNTAGGED(DEREFWORD(x)) >= 0 ? 0 : -1;
    else sign_x = OBJ_IS_NEGATIVE(GetLengthWord(DEREFWORD(x))) ? -1 : 0;

    if (IS_INT(DEREFWORD(y))) sign_y = UNTAGGED(DEREFWORD(y)) >= 0 ? 0 : -1;
    else sign_y = OBJ_IS_NEGATIVE(GetLengthWord(DEREFWORD(y))) ? -1 : 0;

    /* Work out whether to add or subtract */
    if ((sign_y ^ sign_x) >= 0) /* signs the same? */
        /* sign(x) * (abs(x) + abs(y)) */
        return add_unsigned_long(taskData, x, y, sign_x);
    else
        /* sign(x) * (abs(x) - abs(y)) */
        return sub_unsigned_long(taskData, x, y, sign_x);
} /* add_longc */

Handle sub_longc(TaskData *taskData, Handle y, Handle x)
{
    if (IS_INT(DEREFWORD(x)) &&
        IS_INT(DEREFWORD(y))) /* Both short */
    {
    /* The easiest way to do the subtraction is simply *x-*y+1, but that
        makes it more difficult to check for overflow. */
        POLYSIGNED t = UNTAGGED(DEREFWORD(x)) - UNTAGGED(DEREFWORD(y));
        if (t <= MAXTAGGED && t >= -MAXTAGGED-1) /* No overflow */
            return taskData->saveVec.push(TAGGED(t));
    }

    int sign_x, sign_y;

    if (IS_INT(DEREFWORD(x))) sign_x = UNTAGGED(DEREFWORD(x)) >= 0 ? 0 : -1;
    else sign_x = OBJ_IS_NEGATIVE(GetLengthWord(DEREFWORD(x))) ? -1 : 0;

    if (IS_INT(DEREFWORD(y))) sign_y = UNTAGGED(DEREFWORD(y)) >= 0 ? 0 : -1;
    else sign_y = OBJ_IS_NEGATIVE(GetLengthWord(DEREFWORD(y))) ? -1 : 0;

    /* If the signs are different add the two values. */
    if ((sign_y ^ sign_x) < 0) /* signs differ */
    { /* sign(x) * (abs(x) + abs(y)) */
        return add_unsigned_long(taskData, x, y, sign_x);
    }
    else
    { /* sign(x) * (abs(x) - abs(y)) */
        return sub_unsigned_long(taskData, x, y, sign_x);
    }
} /* sub_longc */


Handle mult_longc(TaskData *taskData, Handle y, Handle x)
{
    int sign_x, sign_y;
#if USE_GMP
    mp_limb_t    x_extend, y_extend;
    mp_size_t lx, ly;
    (void)convertToLong(x, &x_extend, &lx, &sign_x);
    (void)convertToLong(y, &y_extend, &ly, &sign_y);
#else
    byte    x_extend[sizeof(PolyWord)], y_extend[sizeof(PolyWord)];
    POLYUNSIGNED lx, ly;
    (void)convertToLong(x, x_extend, &lx, &sign_x);
    (void)convertToLong(y, y_extend, &ly, &sign_y);
#endif
    // Check for zero args.
    if (lx == 0 || ly == 0) return taskData->saveVec.push(TAGGED(0));

#if USE_GMP
    Handle z = alloc_and_save(taskData, WORDS((lx+ly)*sizeof(mp_limb_t)), F_MUTABLE_BIT|F_BYTE_OBJ);
    mp_limb_t *w = DEREFLIMBHANDLE(z);
    mp_limb_t *u = IS_INT(DEREFWORD(x)) ? &x_extend : DEREFLIMBHANDLE(x);
    mp_limb_t *v = IS_INT(DEREFWORD(y)) ? &y_extend : DEREFLIMBHANDLE(y);

    // The first argument must be the longer.
    if (lx < ly) mpn_mul(w, v, ly, u, lx);
    else mpn_mul(w, u, lx, v, ly);

    return make_canonical(taskData, z, sign_x ^ sign_y);
#else
    /* Get space for result */
    Handle long_z = alloc_and_save(taskData, WORDS(lx+ly+1), F_MUTABLE_BIT|F_BYTE_OBJ);

    /* Can now load the actual addresses because they will not change now. */
    byte *u = IS_INT(DEREFWORD(x)) ? x_extend : DEREFBYTEHANDLE(x);
    byte *v = IS_INT(DEREFWORD(y)) ? y_extend : DEREFBYTEHANDLE(y);
    byte *w = DEREFBYTEHANDLE(long_z);

    for(POLYUNSIGNED i = 0; i < lx; i++)
    {
        POLYUNSIGNED j;
        long r = 0; /* Set the carry to zero */
        for(j = 0; j < ly; j++)
        {
            /* Compute the product. */
            r += u[i] * v[j];
            /* Now add in to the result. */
            r += w[i+j];
            w[i+j] = r & 0xff;
            r >>= 8;
        }
        /* Put in any carry. */
        w[i+j] = (byte)r;
    }

    return make_canonical(taskData, long_z, sign_x ^ sign_y);
#endif
} /* mult_long */

#ifndef USE_GMP
static void div_unsigned_long(byte *u, byte *v, byte *remres, byte *divres, POLYUNSIGNED lu, POLYUNSIGNED lv)
// Unsigned division. This is the main divide and remainder routine.
// remres must be at least lu+1 bytes long
// divres must be at least lu-lv+1 bytes long but can be zero if not required
{
    POLYUNSIGNED i,j;
    long r;

    /* Find out how far to shift v to get a 1 in the top bit. */
    int bits = 0;
    for(r = v[lv-1]; r < 128; r <<= 1) bits++; /* 128 ??? */

    /* Shift u that amount into res. We have allowed enough room for
       overflow. */
    r = 0;
    for (i = 0; i < lu; i++)
    {
        r |= u[i] << bits; /*``Or in'' the new bits after shifting*/
        remres[i] = r & 0xff;    /* Put into the destination. */
        r >>= 8;                /* and shift down the carry. */
    }
    remres[i] = (byte)r; /* Put in the carry */

    /* And v that amount. It has already been copied. */
    if ( bits )
    {
        r = 0;
        for (i = 0; i < lv; i++)
        { r |= v[i] << bits; v[i] = r & 0xff; r >>= 8; }
        /* No carry */
    }

    for(j = lu; j >= lv; j--)
    {
    /* j iterates over the higher digits of the dividend until we are left
        with a number which is less than the divisor. This is the remainder. */
        long quotient, dividend, r;
        dividend = remres[j]*256 + remres[j-1];
        quotient = (remres[j] == v[lv-1]) ? 255 : dividend/(long)v[lv-1];

        if (lv != 1)
        {
            while ((long)v[lv-2]*quotient >
                (dividend - quotient*(long)v[lv-1])*256 + (long)remres[j-2])
            {
                quotient--;
            }
        }

        /* The quotient is at most 1 too large */
        /* Subtract the product of this with ``v'' from ``res''. */
        r = 1; /* Initial borrow */
        for(i = 0; i < lv; i++)
        {
            r += 255 + remres[j-lv+i] - quotient * v[i];
            remres[j-lv+i] = r & 0xff;
            r >>= 8;
        }

        r += remres[j]; /* Borrow from leading digit. */
                     /* If we are left with a borrow when the subtraction is complete the
                     quotient must have been too big. We add ``v'' to the dividend and
        subtract 1 from the quotient. */
        if (r == 0 /* would be 1 if there were no borrow */)
        {
            quotient --;
            r = 0;
            for (i = 0; i < lv; i++)
            {
                r += v[i] + remres[j-lv+i];
                remres[j-lv+i] = r & 0xff;
                r >>= 8;
            }
        }
        /* Place the next digit of quotient in result */
        if (divres) divres[j-lv] = (byte)quotient;
    }

    /* Likewise the remainder. */
    if (bits)
    {
        r = 0;
        j = lv;
        while (j > 0)
        {
            j--;
            r |= remres[j];
            remres[j] = (r >> bits) & 0xff;
            r = (r & 0xff) << 8;
        }
    }
} /* div_unsigned_long */
#endif

// Common code for div and mod.  Returns handles to the results.
static void quotRem(TaskData *taskData, Handle y, Handle x, Handle &remHandle, Handle &divHandle)
{
    if (IS_INT(DEREFWORD(x)) &&
        IS_INT(DEREFWORD(y))) /* Both short */
    {
        POLYSIGNED xs = UNTAGGED(DEREFWORD(x));
        POLYSIGNED ys = UNTAGGED(DEREFWORD(y));
        /* Raise exceptions if dividing by zero. */
        if (ys == 0)
            raise_exception0(taskData, EXC_divide);

        /* Only possible overflow is minint div -1 */
        if (xs != -MAXTAGGED-1 || ys != -1) {
            divHandle = taskData->saveVec.push(TAGGED(xs / ys));
            remHandle = taskData->saveVec.push(TAGGED(xs % ys));
            return;
        }
    }

    int sign_x, sign_y;

#if USE_GMP
    mp_limb_t    x_extend, y_extend;
    mp_size_t lx, ly;
    (void)convertToLong(x, &x_extend, &lx, &sign_x);
    (void)convertToLong(y, &y_extend, &ly, &sign_y);

    // If length of v is zero raise divideerror.
    if (ly == 0) raise_exception0(taskData, EXC_divide);
    if (lx < ly) {
        divHandle = taskData->saveVec.push(TAGGED(0));
        remHandle = x; /* When x < y remainder is x. */
        return;
    }

    Handle remRes = alloc_and_save(taskData, WORDS(ly * sizeof(mp_limb_t)), F_MUTABLE_BIT | F_BYTE_OBJ);
    Handle divRes = alloc_and_save(taskData, WORDS((lx - ly + 1) * sizeof(mp_limb_t)), F_MUTABLE_BIT | F_BYTE_OBJ);

    mp_limb_t *u = IS_INT(DEREFWORD(x)) ? &x_extend : DEREFLIMBHANDLE(x);
    mp_limb_t *v = IS_INT(DEREFWORD(y)) ? &y_extend : DEREFLIMBHANDLE(y);
    mp_limb_t *quotient = DEREFLIMBHANDLE(divRes);
    mp_limb_t *remainder = DEREFLIMBHANDLE(remRes);

    // Do the division.
    mpn_tdiv_qr(quotient, remainder, 0, u, lx, v, ly);

    // Return the results.
    remHandle = make_canonical(taskData, remRes, sign_x /* Same sign as dividend */);
    divHandle = make_canonical(taskData, divRes, sign_x ^ sign_y);
#else
    byte    x_extend[sizeof(PolyWord)], y_extend[sizeof(PolyWord)];
    POLYUNSIGNED lx, ly;
    (void)convertToLong(x, x_extend, &lx, &sign_x);
    (void)convertToLong(y, y_extend, &ly, &sign_y);

    /* If length of y is zero raise divideerror */
    if (ly == 0) raise_exception0(taskData, EXC_divide);
    // If the length of divisor is less than the dividend the quotient is zero.
    if (lx < ly) {
        divHandle = taskData->saveVec.push(TAGGED(0));
        remHandle = x; /* When x < y remainder is x. */
        return;
    }

    /* copy in case it needs shifting */
    Handle longCopyHndl = alloc_and_save(taskData, WORDS(ly), F_BYTE_OBJ | F_MUTABLE_BIT);
    byte *u = IS_INT(DEREFWORD(y)) ? y_extend : DEREFBYTEHANDLE(y);
    memcpy(DEREFBYTEHANDLE(longCopyHndl), u, ly);

    Handle divRes = alloc_and_save(taskData, WORDS(lx-ly+1), F_MUTABLE_BIT|F_BYTE_OBJ);
    Handle remRes = alloc_and_save(taskData, WORDS(lx+1), F_MUTABLE_BIT|F_BYTE_OBJ);

    byte *long_x = IS_INT(DEREFWORD(x)) ? x_extend : DEREFBYTEHANDLE(x);
    div_unsigned_long
        (long_x,
        DEREFBYTEHANDLE(longCopyHndl),
        DEREFBYTEHANDLE(remRes), DEREFBYTEHANDLE(divRes),
        lx, ly);

    /* Clear the rest */
    for(POLYUNSIGNED i=ly; i < lx+1; i++)
    {
        DEREFBYTEHANDLE(remRes)[i] = 0;
    }

    remHandle = make_canonical(taskData, remRes, sign_x /* Same sign as dividend */ );
    divHandle = make_canonical(taskData, divRes, sign_x ^ sign_y);
#endif
}

// This returns x divided by y.  This always rounds towards zero so
// corresponds to Int.quot in ML not Int.div.
Handle div_longc(TaskData *taskData, Handle y, Handle x)
{
    Handle remHandle, divHandle;
    quotRem(taskData, y, x, remHandle, divHandle);
    return divHandle;
}

Handle rem_longc(TaskData *taskData, Handle y, Handle x)
{
    Handle remHandle, divHandle;
    quotRem(taskData, y, x, remHandle, divHandle);
    return remHandle;
}

#if defined(_WIN32)
// Return a FILETIME from an arbitrary precision number.  On both 32-bit and 64-bit Windows
// this is a pair of 32-bit values.
void getFileTimeFromArb(TaskData *taskData, Handle numHandle, PFILETIME ft)
{
    Handle twoTo16 = taskData->saveVec.push(TAGGED(65536));
    Handle twoTo32 = mult_longc(taskData, twoTo16, twoTo16);
    Handle highPart, lowPart;
    quotRem(taskData, twoTo32, numHandle, lowPart, highPart);
    ft->dwLowDateTime = get_C_unsigned(taskData, lowPart->Word());
    ft->dwHighDateTime  = get_C_unsigned(taskData, highPart->Word());
}
#endif

/* compare_unsigned is passed LONG integers only */
static int compare_unsigned(PolyWord x, PolyWord y)
{
#ifdef USE_GMP
    mp_size_t lx = numLimbs(x);
    mp_size_t ly = numLimbs(y);

    if (lx != ly)  /* u > v if u longer than v */
    {
        return (lx > ly ? 1 : -1);
    }
    return mpn_cmp((mp_limb_t *)x.AsCodePtr(), (mp_limb_t *)y.AsCodePtr(), lx);
#else
    /* First look at the lengths */
    POLYUNSIGNED lx = get_length(x);
    POLYUNSIGNED ly = get_length(y);

    if (lx != ly)  /* u > v if u longer than v */
    {
        return (lx > ly ? 1 : -1);
    }

    // Same length - look at the values. */
    byte *u = x.AsCodePtr();
    byte *v = y.AsCodePtr();

    POLYUNSIGNED i = lx;
    while (i > 0)
    {
        i--;
        if (u[i] != v[i])
        {
            return u[i] > v[i] ? 1 : -1;
        }
    }
    /* Must be equal */
    return 0;
#endif
}

int compareLong(PolyWord y, PolyWord x)
{
    // Test if the values are bitwise equal.  If either is short
    // this is the only case where the values could be equal.
    if (x == y) // Equal
        return 0;

    if (x.IsTagged())
    {
        // x is short.
        if (y.IsTagged()) {
            // Both short.  We've already tested for equality.
            if (x.UnTagged() < y.UnTagged())
                return -1; // Less
            else return 1; // Greater
        }
        // y is not short.  Just test the sign.  If it's negative
        // it must be less than any short value and if it's positive
        // it must be greater.
        if (OBJ_IS_NEGATIVE(GetLengthWord(y)))
            return 1; // x is greater
        else return -1; // x is less
    }

    // x is not short
    if (y.IsTagged())
    {
        // y is short.  Just test the sign of x
        if (OBJ_IS_NEGATIVE(GetLengthWord(x)))
            return -1; // x is less
        else return 1; // x is greater
    }

    // Must both be long.  We may be able to determine the result based purely on the sign bits.
    if (! OBJ_IS_NEGATIVE(GetLengthWord(x))) /* x is positive */
    {
        if (! OBJ_IS_NEGATIVE(GetLengthWord(y))) /* y also positive */
        {
            return compare_unsigned(x, y);
        }
        else /* y negative so x > y */
        {
            return 1;
        }
    }
    else
    { /* x is negative */
        if (OBJ_IS_NEGATIVE(GetLengthWord(y))) /* y also negative */
        {
            return compare_unsigned(y, x);
        }
        else /* y positive so x < y */
        {
            return -1;
        }
    }
} /* compareLong */

/* logical_long.  General purpose function for binary logical operations. */
static Handle logical_long(TaskData *taskData, Handle x, Handle y,
                           unsigned(*op)(unsigned, unsigned))
{
    int signX, signY;
#if USE_GMP
    mp_limb_t   x_extend, y_extend;
    mp_size_t   lx, ly;

    (void)convertToLong(x, &x_extend, &lx, &signX);
    (void)convertToLong(y, &y_extend, &ly, &signY);
    lx = lx*sizeof(mp_limb_t); // We want these in bytes
    ly = ly*sizeof(mp_limb_t);
#else
    byte    x_extend[sizeof(PolyWord)], y_extend[sizeof(PolyWord)];
    POLYUNSIGNED    lx, ly;
    (void)convertToLong(x, x_extend, &lx, &signX);
    (void)convertToLong(y, y_extend, &ly, &signY);
#endif

    byte *u; /* byte-pointer for longer number  */
    byte *v; /* byte-pointer for shorter number */
    Handle z;
    int sign, signU, signV;

    POLYUNSIGNED lu;   /* length of u in bytes */
    POLYUNSIGNED lv;   /* length of v in bytes */

    /* find the longer number */

    /* Make ``u'' the longer. */
    if (lx < ly)
    {
        // Get result vector. There can't be any carry at the end so
        // we just need to make this as large as the larger number.
        z = alloc_and_save(taskData, WORDS(ly), F_MUTABLE_BIT|F_BYTE_OBJ);

        /* now safe to dereference pointers */
        u = IS_INT(DEREFWORD(y)) ? (byte*)&y_extend : DEREFBYTEHANDLE(y); lu = ly;
        v = IS_INT(DEREFWORD(x)) ? (byte*)&x_extend : DEREFBYTEHANDLE(x); lv = lx;
        signU = signY; signV = signX;
    }

    else
    {
        /* Get result vector. */
#if USE_GMP
        // Add one limb
        z = alloc_and_save(taskData, WORDS(lx+sizeof(mp_limb_t)), F_MUTABLE_BIT|F_BYTE_OBJ);
#else
        // Add one word.  Actually we just want one more byte.
        z = alloc_and_save(taskData, WORDS(lx+sizeof(PolyWord)), F_MUTABLE_BIT|F_BYTE_OBJ);
#endif

        /* now safe to dereference pointers */
        u = IS_INT(DEREFWORD(x)) ? (byte*)&x_extend : DEREFBYTEHANDLE(x); lu = lx;
        v = IS_INT(DEREFWORD(y)) ? (byte*)&y_extend : DEREFBYTEHANDLE(y); lv = ly;
        signU = signX; signV = signY;
    }

    sign = (*op)(signU, signV); /* -1 if negative, 0 if positive. */

    { /* do the actual operations */
        byte  *w = DEREFBYTEHANDLE(z);
        int borrowU = 1, borrowV = 1, borrowW = 1;
        POLYUNSIGNED i = 0;

        /* Do the operations. */
        for( ; i < lv; i++)
        {
            int wI;
            /* Have to convert negative values to twos complement. */
            if (signU) borrowU += 255 - u[i];
            else borrowU = u[i];
            if (signV) borrowV += 255 - v[i];
            else borrowV = v[i];
            wI = (*op)(borrowU, borrowV) & 255;
            if (sign)
            {
                /* Have to convert the result back to twos complement. */
                borrowW += 255 - wI;
                w[i] = borrowW & 255;
                borrowW >>= 8;
            }
            else w[i] = wI;
            borrowU >>= 8;
            borrowV >>= 8;
        }
        /* At this point the borrow of V should be zero. */
        ASSERT(signV == 0 || borrowV == 0);

        /* Continue with ``u''. */
        for( ; i < lu; i++)
        {
            int wI;
            if (signU) borrowU += 255 - u[i];
            else borrowU = u[i];
            if (signV) borrowV = 255; else borrowV = 0;
            wI = (*op)(borrowU, borrowV) & 255;
            if (sign)
            {
                /* Have to convert the result back to twos complement. */
                borrowW += 255 - wI;
                w[i] = borrowW & 255;
                borrowW >>= 8;
            }
            else w[i] = wI;
            borrowU >>= 8;
            borrowV >>= 8;
        }
        /* We should now no longer have any borrows. */
        ASSERT(signU == 0 || borrowU == 0);
        ASSERT(sign == 0 || borrowW == 0);
    }

    return make_canonical(taskData, z, sign);
} /* logical_long */

static unsigned doAnd(unsigned i, unsigned j)
{
    return i & j;
}

static unsigned doOr(unsigned i, unsigned j)
{
    return i | j;
}

static unsigned doXor(unsigned i, unsigned j)
{
    return i ^ j;
}

Handle and_longc(TaskData *taskData, Handle y, Handle x)
{
    if (IS_INT(DEREFWORD(x)) &&
        IS_INT(DEREFWORD(y))) /* Both short */
    {
       /* There's no problem with overflow so we can just AND together
           the values. */
        POLYSIGNED t = UNTAGGED(DEREFWORD(x)) & UNTAGGED(DEREFWORD(y));
        return taskData->saveVec.push(TAGGED(t));
    }

    return logical_long(taskData, x, y, doAnd);
}

Handle or_longc(TaskData *taskData, Handle y, Handle x)
{
    if (IS_INT(DEREFWORD(x)) &&
        IS_INT(DEREFWORD(y))) /* Both short */
    {
    /* There's no problem with overflow so we can just OR together
        the values. */
        POLYSIGNED t = UNTAGGED(DEREFWORD(x)) | UNTAGGED(DEREFWORD(y));
        return taskData->saveVec.push(TAGGED(t));
    }

    return logical_long(taskData, x, y, doOr);
}

Handle xor_longc(TaskData *taskData, Handle y, Handle x)
{
    if (IS_INT(DEREFWORD(x)) &&
        IS_INT(DEREFWORD(y))) /* Both short */
    {
    /* There's no problem with overflow so we can just XOR together
        the values. */
        POLYSIGNED t = UNTAGGED(DEREFWORD(x)) ^ UNTAGGED(DEREFWORD(y));
        return taskData->saveVec.push(TAGGED(t));
    }
    return logical_long(taskData, x, y, doXor);
}

// Convert a long precision value to floating point
double get_arbitrary_precision_as_real(PolyWord x)
{
    if (IS_INT(x)) {
        POLYSIGNED t = UNTAGGED(x);
        return (double)t;
    }
    double acc = 0;
#if USE_GMP
    mp_limb_t *u = (mp_limb_t *)(x.AsObjPtr());
    mp_size_t lx = numLimbs(x);
    for ( ; lx > 0; lx--) {
        int ll = sizeof(mp_limb_t);
        for ( ; ll > 0 ; ll-- ) {
            acc = acc * 256;
        }
        acc = acc + (double)u[lx-1];
    }
#else
    byte *u = (byte *)(x.AsObjPtr());
    POLYUNSIGNED lx = OBJECT_LENGTH(x)*sizeof(PolyWord);
    for( ; lx > 0; lx--) {
        acc = acc * 256 + (double)u[lx-1];
    }
#endif
    if (OBJ_IS_NEGATIVE(GetLengthWord(x)))
        return -acc;
    else return acc;
}


/*  Arbitrary precision GCD function.  This is really included to make
    use of GMP's GCD function that selects an algorithm based on the
    length of the arguments. */

#ifdef USE_GMP
Handle gcd_arbitrary(TaskData *taskData, Handle x, Handle y)
{
    /* mpn_gcd requires that each argument is odd and its first argument must be
       no longer than its second.  This requires shifting before the call and after
       the result has been returned.  This code is modelled roughly on the high level
       mpz_gcd call in GMP. */
    mp_limb_t    x_extend, y_extend;
    int sign_x, sign_y; // Signs are ignored - the result is always positive.
    mp_size_t lx, ly;

    mp_limb_t *longX = convertToLong(x, &x_extend, &lx, &sign_x);
    mp_limb_t *longY = convertToLong(y, &y_extend, &ly, &sign_y);

    // Test for zero length and therefore zero argument
    if (lx == 0)
    {
        // GCD(0,y) = abs(y)
        if (sign_y)
            return neg_longc(taskData, y);
        else return y;
    }
    if (ly == 0)
    {
        // GCD(x,0 = abs(x)
        if (sign_x)
            return neg_longc(taskData, x);
        else return x;
    }
    // If one of the arguments is a single limb we can use the special case.
    // This doesn't require shifting.  It also doesn't say that it could
    // overwrite the arguments.
    if (lx == 1 || ly == 1)
    {
        mp_limb_t g = (lx == 1) ? mpn_gcd_1(longY, ly, *longX) : mpn_gcd_1(longX, lx, *longY);
        if (g <= MAXTAGGED)
            return taskData->saveVec.push(TAGGED(g));
        // Need to allocate space.
        Handle r = alloc_and_save(taskData, WORDS(sizeof(mp_limb_t)), F_BYTE_OBJ);
        *(DEREFLIMBHANDLE(r)) = g;
        return r;
    }

    // Memory for result.  This can be up to the shorter of the two.
    // We rely on this zero the memory because we may not set every word here.
    Handle r = alloc_and_save(taskData, WORDS((lx < ly ? lx : ly)*sizeof(mp_limb_t)), F_BYTE_OBJ|F_MUTABLE_BIT);
    // Can now dereference the handles.
    mp_limb_t *xl = IS_INT(DEREFWORD(x)) ? &x_extend : DEREFLIMBHANDLE(x);
    mp_limb_t *yl = IS_INT(DEREFWORD(y)) ? &y_extend : DEREFLIMBHANDLE(y);
    mp_limb_t *rl = DEREFLIMBHANDLE(r);

    unsigned xZeroLimbs = 0, xZeroBits = 0;
    // Remove whole limbs of zeros.  There must be a word which is non-zero.
    while (*xl == 0) { xl++; xZeroLimbs++; lx--; }
    // Count the low-order bits and shift by that amount.
    mp_limb_t t = *xl;
    while ((t & 1) == 0) { t = t >> 1; xZeroBits++; }
    // Copy the non-zero limbs into a temporary, shifting if necessary.
    mp_limb_t *xC = (mp_limb_t*)alloca(lx * sizeof(mp_limb_t));
    if (xZeroBits != 0)
    {
        mpn_rshift(xC, xl, lx, xZeroBits);
        if (xC[lx-1] == 0) lx--;
    }
    else memcpy(xC, xl, lx * sizeof(mp_limb_t));

    unsigned yZeroLimbs = 0, yZeroBits = 0;
    while (*yl == 0) { yl++; yZeroLimbs++; ly--; }
    t = *yl;
    while ((t & 1) == 0) { t = t >> 1; yZeroBits++; }
    mp_limb_t *yC = (mp_limb_t*)alloca(ly * sizeof(mp_limb_t));
    if (yZeroBits != 0)
    {
        mpn_rshift(yC, yl, ly, yZeroBits);
        if (yC[ly-1] == 0) ly--;
    }
    else memcpy(yC, yl, ly * sizeof(mp_limb_t));

    // The result length and shift is the smaller of these
    unsigned rZeroLimbs, rZeroBits;
    if (xZeroLimbs < yZeroLimbs || (xZeroLimbs == yZeroLimbs && xZeroBits < yZeroBits))
    {
        rZeroLimbs = xZeroLimbs;
        rZeroBits = xZeroBits;
    }
    else
    {
        rZeroLimbs = yZeroLimbs;
        rZeroBits = yZeroBits;
    }
    // Now actually compute the GCD
    if (lx < ly || (lx == ly && xC[lx-1] < yC[ly-1]))
        lx = mpn_gcd(xC, yC, ly, xC, lx);
    else
        lx = mpn_gcd(xC, xC, lx, yC, ly);
    // Shift the temporary result into the final area.
    if (rZeroBits != 0)
    {
        t = mpn_lshift(rl+rZeroLimbs, xC, lx, rZeroBits);
        if (t != 0)
            rl[rZeroLimbs+lx] = t;
    }
    else memcpy(rl+rZeroLimbs, xC, lx * sizeof(mp_limb_t));

    return make_canonical(taskData, r, false);
}

#else
// Fallback version for when GMP is not defined.
static Handle gxd(TaskData *taskData, Handle x, Handle y)
{
    Handle marker = taskData->saveVec.mark();
    while (1)
    {
        if (DEREFWORD(y) == TAGGED(0))
            return x;

        Handle res = rem_longc(taskData, y, x);
        PolyWord newY = res->Word();
        PolyWord newX = y->Word();
        taskData->saveVec.reset(marker);
        y = taskData->saveVec.push(newY);
        x = taskData->saveVec.push(newX);
    }
}

static Handle absValue(TaskData *taskData, Handle x)
{
    if (IS_INT(DEREFWORD(x)))
    {
        if (UNTAGGED(DEREFWORD(x)) < 0)
            return neg_longc(taskData, x);
    }
    else if (OBJ_IS_NEGATIVE(GetLengthWord(DEREFWORD(x))))
        return neg_longc(taskData, x);
    return x;
}

Handle gcd_arbitrary(TaskData *taskData, Handle x, Handle y)
{
    x = absValue(taskData, x);
    y = absValue(taskData, y);

    if (compareLong(y->Word(), x->Word()) < 0)
        return gxd(taskData, y, x);
    else return gxd(taskData, x, y);
}
#endif

// This is provided as an adjunct to GCD.  Using this saves the RTS
// calls necessary for the division and multiplication.
Handle lcm_arbitrary(TaskData *taskData, Handle x, Handle y)
{
    Handle g = gcd_arbitrary(taskData, x, y);
    return mult_longc(taskData, x, div_longc(taskData, g, y));
}

POLYUNSIGNED PolyAddArbitrary(PolyObject *threadId, PolyWord arg1, PolyWord arg2)
{
    TaskData *taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    Handle reset = taskData->saveVec.mark();
    Handle pushedArg1 = taskData->saveVec.push(arg1);
    Handle pushedArg2 = taskData->saveVec.push(arg2);
    Handle result = 0;
    
    if (profileMode == kProfileEmulation)
        taskData->addProfileCount(1);

    try {
        // Could raise an exception if out of memory.
        result = add_longc(taskData, pushedArg2, pushedArg1);
    }
    catch (...) { } // If an ML exception is raised

    taskData->saveVec.reset(reset); // Ensure the save vec is reset
    taskData->PostRTSCall();
    if (result == 0) return TAGGED(0).AsUnsigned();
    else return result->Word().AsUnsigned();
}

POLYUNSIGNED PolySubtractArbitrary(PolyObject *threadId, PolyWord arg1, PolyWord arg2)
{
    TaskData *taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    Handle reset = taskData->saveVec.mark();
    Handle pushedArg1 = taskData->saveVec.push(arg1);
    Handle pushedArg2 = taskData->saveVec.push(arg2);
    Handle result = 0;
    
    if (profileMode == kProfileEmulation)
        taskData->addProfileCount(1);

    try {
        result = sub_longc(taskData, pushedArg2, pushedArg1);
    } catch (...) { } // If an ML exception is raised

    taskData->saveVec.reset(reset); // Ensure the save vec is reset
    taskData->PostRTSCall();
    if (result == 0) return TAGGED(0).AsUnsigned();
    else return result->Word().AsUnsigned();
}

POLYUNSIGNED PolyMultiplyArbitrary(PolyObject *threadId, PolyWord arg1, PolyWord arg2)
{
    TaskData *taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    Handle reset = taskData->saveVec.mark();
    Handle pushedArg1 = taskData->saveVec.push(arg1);
    Handle pushedArg2 = taskData->saveVec.push(arg2);
    Handle result = 0;
    
    if (profileMode == kProfileEmulation)
        taskData->addProfileCount(1);

    try {
        result = mult_longc(taskData, pushedArg2, pushedArg1);
    } catch (...) { } // If an ML exception is raised

    taskData->saveVec.reset(reset); // Ensure the save vec is reset
    taskData->PostRTSCall();
    if (result == 0) return TAGGED(0).AsUnsigned();
    else return result->Word().AsUnsigned();
}

POLYUNSIGNED PolyDivideArbitrary(PolyObject *threadId, PolyWord arg1, PolyWord arg2)
{
    TaskData *taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    Handle reset = taskData->saveVec.mark();
    Handle pushedArg1 = taskData->saveVec.push(arg1);
    Handle pushedArg2 = taskData->saveVec.push(arg2);
    Handle result = 0;
    
    if (profileMode == kProfileEmulation)
        taskData->addProfileCount(1);

    try {
        // May raise divide exception
        result = div_longc(taskData, pushedArg2, pushedArg1);
    } catch (...) { } // If an ML exception is raised

    taskData->saveVec.reset(reset); // Ensure the save vec is reset
    taskData->PostRTSCall();
    if (result == 0) return TAGGED(0).AsUnsigned();
    else return result->Word().AsUnsigned();
}

POLYUNSIGNED PolyRemainderArbitrary(PolyObject *threadId, PolyWord arg1, PolyWord arg2)
{
    TaskData *taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    Handle reset = taskData->saveVec.mark();
    Handle pushedArg1 = taskData->saveVec.push(arg1);
    Handle pushedArg2 = taskData->saveVec.push(arg2);
    Handle result = 0;
    
    if (profileMode == kProfileEmulation)
        taskData->addProfileCount(1);

    try {
        result = rem_longc(taskData, pushedArg2, pushedArg1);
    } catch (...) { } // If an ML exception is raised

    taskData->saveVec.reset(reset); // Ensure the save vec is reset
    taskData->PostRTSCall();
    if (result == 0) return TAGGED(0).AsUnsigned();
    else return result->Word().AsUnsigned();
}

// This is the older version that took a container as an argument.
POLYUNSIGNED PolyQuotRemArbitrary(PolyObject *threadId, PolyWord arg1, PolyWord arg2, PolyWord arg3)
{
    TaskData *taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    Handle reset = taskData->saveVec.mark();
    Handle pushedArg1 = taskData->saveVec.push(arg1);
    Handle pushedArg2 = taskData->saveVec.push(arg2);
    // arg3 is an address on the stack.  It is not a PolyWord.

    if (profileMode == kProfileEmulation)
        taskData->addProfileCount(1);

    try {
        // The result handle will almost certainly point into the stack.
        // This should now be safe within the GC.
        Handle remHandle, divHandle;
        quotRem(taskData, pushedArg2, pushedArg1, remHandle, divHandle);

        arg3.AsObjPtr()->Set(0, divHandle->Word());
        arg3.AsObjPtr()->Set(1, remHandle->Word());
    } catch (...) { } // If an ML exception is raised

    taskData->saveVec.reset(reset); // Ensure the save vec is reset
    taskData->PostRTSCall();
    return 0; // Result is unit
}

// This is the newer version that returns a pair.  It's simpler and works with 32-in-64.
POLYUNSIGNED PolyQuotRemArbitraryPair(PolyObject *threadId, PolyWord arg1, PolyWord arg2)
{
    TaskData *taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    Handle reset = taskData->saveVec.mark();
    Handle pushedArg1 = taskData->saveVec.push(arg1);
    Handle pushedArg2 = taskData->saveVec.push(arg2);
    Handle result = 0;
    // arg3 is an address on the stack.  It is not a PolyWord.

    if (profileMode == kProfileEmulation)
        taskData->addProfileCount(1);

    try {
        // The result handle will almost certainly point into the stack.
        // This should now be safe within the GC.
        Handle remHandle, divHandle;
        quotRem(taskData, pushedArg2, pushedArg1, remHandle, divHandle);

        result = alloc_and_save(taskData, 2);

        result->WordP()->Set(0, divHandle->Word());
        result->WordP()->Set(1, remHandle->Word());
    }
    catch (...) {} // If an ML exception is raised

    taskData->saveVec.reset(reset); // Ensure the save vec is reset
    taskData->PostRTSCall();
    if (result == 0) return TAGGED(0).AsUnsigned();
    else return result->Word().AsUnsigned();
}

// This can be a fast call.  It does not need to allocate or use handles.
POLYSIGNED PolyCompareArbitrary(PolyWord arg1, PolyWord arg2)
{
    return TAGGED(compareLong(arg2, arg1)).AsSigned();
}

POLYUNSIGNED PolyGCDArbitrary(PolyObject *threadId, PolyWord arg1, PolyWord arg2)
{
    TaskData *taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    Handle reset = taskData->saveVec.mark();
    Handle pushedArg1 = taskData->saveVec.push(arg1);
    Handle pushedArg2 = taskData->saveVec.push(arg2);
    Handle result = 0;

    try {
        result = gcd_arbitrary(taskData, pushedArg2, pushedArg1);
        // Generally shouldn't raise an exception but might run out of store.
    } catch (...) { } // If an ML exception is raised

    taskData->saveVec.reset(reset); // Ensure the save vec is reset
    taskData->PostRTSCall();
    if (result == 0) return TAGGED(0).AsUnsigned();
    else return result->Word().AsUnsigned();
}

POLYUNSIGNED PolyLCMArbitrary(PolyObject *threadId, PolyWord arg1, PolyWord arg2)
{
    TaskData *taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    Handle reset = taskData->saveVec.mark();
    Handle pushedArg1 = taskData->saveVec.push(arg1);
    Handle pushedArg2 = taskData->saveVec.push(arg2);
    Handle result = 0;

    try {
        result = lcm_arbitrary(taskData, pushedArg2, pushedArg1);
        // Generally shouldn't raise an exception but might run out of store.
    } catch (...) { } // If an ML exception is raised

    taskData->saveVec.reset(reset); // Ensure the save vec is reset
    taskData->PostRTSCall();
    if (result == 0) return TAGGED(0).AsUnsigned();
    else return result->Word().AsUnsigned();
}

// Extract the low order part of an arbitrary precision value as a boxed LargeWord.word
// value.  If the value is negative it is treated as a twos complement value.
// This is used Word.fromLargeInt and LargeWord.fromLargeInt with long-form
// arbitrary precision values.
POLYUNSIGNED PolyGetLowOrderAsLargeWord(PolyObject *threadId, PolyWord arg)
{
    TaskData *taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    Handle reset = taskData->saveVec.mark();
    uintptr_t p = 0;

    if (arg.IsTagged())
        p = arg.UnTagged();
    else
    {
        bool negative = OBJ_IS_NEGATIVE(GetLengthWord(arg)) ? true : false;
#ifdef USE_GMP
        mp_limb_t c = *(mp_limb_t*)arg.AsCodePtr();
        p = c;
#else
        POLYUNSIGNED length = get_length(arg);
        if (length > sizeof(uintptr_t)) length = sizeof(uintptr_t);
        byte *ptr = arg.AsCodePtr();
        while (length--)
        {
            p = (p << 8) | ptr[length];
        }
#endif
        if (negative) p = 0-p;
    }

    Handle result = 0;
    try {
        result = Make_sysword(taskData, p);
    }
    catch (...) {} // We could run out of memory.

    taskData->saveVec.reset(reset); // Ensure the save vec is reset
    taskData->PostRTSCall();
    if (result == 0) return TAGGED(0).AsUnsigned();
    else return result->Word().AsUnsigned();
}

POLYUNSIGNED PolyOrArbitrary(PolyObject *threadId, PolyWord arg1, PolyWord arg2)
{
    TaskData *taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    Handle reset = taskData->saveVec.mark();
    Handle pushedArg1 = taskData->saveVec.push(arg1);
    Handle pushedArg2 = taskData->saveVec.push(arg2);
    Handle result = 0;

    try {
        // Could raise an exception if out of memory.
        result = or_longc(taskData, pushedArg2, pushedArg1);
    }
    catch (...) { } // If an ML exception is raised

    taskData->saveVec.reset(reset); // Ensure the save vec is reset
    taskData->PostRTSCall();
    if (result == 0) return TAGGED(0).AsUnsigned();
    else return result->Word().AsUnsigned();
}

POLYUNSIGNED PolyAndArbitrary(PolyObject *threadId, PolyWord arg1, PolyWord arg2)
{
    TaskData *taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    Handle reset = taskData->saveVec.mark();
    Handle pushedArg1 = taskData->saveVec.push(arg1);
    Handle pushedArg2 = taskData->saveVec.push(arg2);
    Handle result = 0;

    try {
        // Could raise an exception if out of memory.
        result = and_longc(taskData, pushedArg2, pushedArg1);
    }
    catch (...) { } // If an ML exception is raised

    taskData->saveVec.reset(reset); // Ensure the save vec is reset
    taskData->PostRTSCall();
    if (result == 0) return TAGGED(0).AsUnsigned();
    else return result->Word().AsUnsigned();
}

POLYUNSIGNED PolyXorArbitrary(PolyObject *threadId, PolyWord arg1, PolyWord arg2)
{
    TaskData *taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    Handle reset = taskData->saveVec.mark();
    Handle pushedArg1 = taskData->saveVec.push(arg1);
    Handle pushedArg2 = taskData->saveVec.push(arg2);
    Handle result = 0;

    try {
        // Could raise an exception if out of memory.
        result = xor_longc(taskData, pushedArg2, pushedArg1);
    }
    catch (...) { } // If an ML exception is raised

    taskData->saveVec.reset(reset); // Ensure the save vec is reset
    taskData->PostRTSCall();
    if (result == 0) return TAGGED(0).AsUnsigned();
    else return result->Word().AsUnsigned();
}

struct _entrypts arbitraryPrecisionEPT[] =
{
    { "PolyAddArbitrary",               (polyRTSFunction)&PolyAddArbitrary},
    { "PolySubtractArbitrary",          (polyRTSFunction)&PolySubtractArbitrary},
    { "PolyMultiplyArbitrary",          (polyRTSFunction)&PolyMultiplyArbitrary},
    { "PolyDivideArbitrary",            (polyRTSFunction)&PolyDivideArbitrary},
    { "PolyRemainderArbitrary",         (polyRTSFunction)&PolyRemainderArbitrary},
    { "PolyQuotRemArbitrary",           (polyRTSFunction)&PolyQuotRemArbitrary},
    { "PolyQuotRemArbitraryPair",       (polyRTSFunction)&PolyQuotRemArbitraryPair },
    { "PolyCompareArbitrary",           (polyRTSFunction)&PolyCompareArbitrary},
    { "PolyGCDArbitrary",               (polyRTSFunction)&PolyGCDArbitrary},
    { "PolyLCMArbitrary",               (polyRTSFunction)&PolyLCMArbitrary},
    { "PolyGetLowOrderAsLargeWord",     (polyRTSFunction)&PolyGetLowOrderAsLargeWord},
    { "PolyOrArbitrary",                (polyRTSFunction)&PolyOrArbitrary},
    { "PolyAndArbitrary",               (polyRTSFunction)&PolyAndArbitrary},
    { "PolyXorArbitrary",               (polyRTSFunction)&PolyXorArbitrary},

    { NULL, NULL} // End of list.
};
