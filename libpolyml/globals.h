/*
    Title:  Globals for the system.
    Author:     Dave Matthews, Cambridge University Computer Laboratory

    Copyright (c) 2000-7
        Cambridge University Technical Services Limited

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

#ifndef _GLOBALS_H
#define _GLOBALS_H

/*
    Poly words, pointers and objects.

    The garbage collector needs to be able to distinguish different uses of a
    memory word.  We need to be able find which words are pointers to other
    objects and which are simple integers.  The simple distinction is between
    integers, which are tagged by having the bottom bit set, and Addresses
    which are word aligned (bottom 2 bits set on a 32 bit machine, bottom 3
    bits on a 64 bit machine).  There is a third case that can arise where
    we have a return address to a piece of code.  These
    are generally aligned on a word+two-byte boundary except that they can
    appear with any alignment in the saved PC field of a stack.  Normally
    these will only appear on the stack or in a register saved to the stack
    but they can be passed as an argument to SetCodeConstant.

    Addresses always point to the start of objects.  There is one exception:
    a stack object can contain addresses within the current stack.  The
    preceding word of a stack object is the length word.  This contains the
    length of the object in words in the low-order 3 (7 on a 64-bit machine)
    bytes and a flag byte in the top byte.  The flags give information about
    the type of the object.  The length word is also used by the garbage
    collector and other object processors.
*/

/* Generally we use a tagged integer representation with 31 bits of
   precision and the low-order bit as a tag bit.  one represents a tagged
   integer, zero a pointer.  Code pointers are always aligned onto a
   word + 2 byte boundary.
   The exception is the Sparc which has special tagged add and subtract
   instructions.  These cause a trap if given a value which is not a
   multiple of 4.  To make use of these instructions we represent tagged
   integers using 30 bits of precision and with the low order bits
   containing 01. 
   DCJM 27/9/00.
 */

#if HAVE_STDINT_H
#  include <stdint.h>
#endif

#if HAVE_INTTYPES_H
#  ifndef __STDC_FORMAT_MACROS
#    define __STDC_FORMAT_MACROS
#  endif
#  include <inttypes.h>
#endif

#ifdef HAVE_STDDEF_H
#  include <stddef.h>
#endif

// Integers (non-pointers) have the bottom bit set and the rest of the word
// contains the significant bits.  The original native code Sparc version
// shifted by two so that it could use the Taddcc/Tsubcc instructions.
#define POLY_TAGSHIFT    1

#if (defined(_WIN32) && ! defined(__CYGWIN__))
#  include <windows.h>
#endif

typedef intptr_t        POLYSIGNED;
typedef uintptr_t       POLYUNSIGNED;

// libpolyml uses printf-style I/O instead of C++ standard IOstreams,
// so we need specifier to format POLYUNSIGNED/POLYSIGNED values.
#ifdef PRIuPTR
#  define POLYUFMT PRIuPTR
#  define POLYSFMT PRIdPTR
#elif (defined(_MSC_VER) && (SIZEOF_VOIDP == 8))
#  define POLYUFMT "llu"
#  define POLYSFMT "lld"
#else
#  define POLYUFMT "lu"         // as before.  Cross your fingers.
#  define POLYSFMT "ld"         // idem.
#endif

typedef unsigned char   byte;

class PolyObject;
typedef PolyObject *POLYOBJPTR;

typedef byte *POLYCODEPTR;

class PolyWord {
public:
    // An object pointer can become a word directly.
    PolyWord(POLYOBJPTR p) { contents.objectPtr = p; }

    // Initialise to TAGGED(0).  This is very rarely used.
    PolyWord() { contents.unsignedInt = 1; }

    // Integers need to be tagged.
    static PolyWord TaggedInt(POLYSIGNED s) { return PolyWord((s << POLY_TAGSHIFT) | (POLYSIGNED)0x01); }
    static PolyWord TaggedUnsigned(POLYUNSIGNED u) { return PolyWord((u << POLY_TAGSHIFT) | 0x01); }

    static PolyWord FromStackAddr(PolyWord *sp) { return PolyWord(sp); }
    static PolyWord FromCodePtr(POLYCODEPTR p) { return PolyWord(p); }

    // Tests for the various cases.
    bool IsTagged(void) const { return (contents.unsignedInt & 1) != 0; }
    bool IsDataPtr(void) const { return (contents.unsignedInt & (sizeof(PolyWord)-1)) == 0; }

    // Extract the various cases.
    POLYSIGNED UnTagged(void) const { return contents.signedInt >> POLY_TAGSHIFT; }
    POLYUNSIGNED UnTaggedUnsigned(void) const { return contents.unsignedInt >> POLY_TAGSHIFT; }
    POLYOBJPTR AsObjPtr(void) const { return contents.objectPtr; }
    PolyWord *AsStackAddr(void) const { return contents.stackAddr; }
    POLYCODEPTR AsCodePtr(void) const { return contents.codePtr; }

    void *AsAddress(void)const { return AsCodePtr(); }

    // There are a few cases where we need to store and extract untagged values
    static PolyWord FromUnsigned(POLYUNSIGNED u) { return PolyWord(u); }
    static PolyWord FromSigned(POLYSIGNED s) { return PolyWord(s); }
    POLYUNSIGNED AsUnsigned(void) const { return contents.unsignedInt; }
    POLYSIGNED AsSigned(void) const { return contents.signedInt; }

protected:
    PolyWord(POLYSIGNED s) { contents.signedInt = s; }
    PolyWord(POLYUNSIGNED u) { contents.unsignedInt = u; }

public:
    bool operator == (PolyWord b) const { return contents.unsignedInt == b.contents.unsignedInt; }
    bool operator != (PolyWord b) const { return contents.unsignedInt != b.contents.unsignedInt; }

protected:
    PolyWord(PolyWord *sp) { contents.stackAddr = sp; }
    PolyWord(POLYCODEPTR p) { contents.codePtr = p; }
    union {
        POLYSIGNED      signedInt;      // A tagged integer - lowest bit set
        POLYUNSIGNED    unsignedInt;    // A tagged integer - lowest bit set
        POLYOBJPTR      objectPtr;      // Object pointer   - two lowest bits clear.
        POLYCODEPTR     codePtr;        // Address within code - lowest bits contain 10.
        PolyWord        *stackAddr;     // Address within current stack - two lowest bits clear.
    } contents;
};

//typedef PolyWord POLYWORD;

inline bool OBJ_IS_AN_INTEGER(const PolyWord & a)           { return a.IsTagged(); }
inline bool OBJ_IS_DATAPTR(const PolyWord & a)              { return a.IsDataPtr(); }
// The maximum tagged signed number is one less than 0x80 shifted into the top byte then shifted down
// by the tag shift.
#define MAXTAGGED                                   (((POLYSIGNED)0x80 << (POLYSIGNED)(8*(sizeof(PolyWord)-1) -POLY_TAGSHIFT)) -1)
inline PolyWord TAGGED(POLYSIGNED a)                { return PolyWord::TaggedInt(a); }
inline POLYSIGNED UNTAGGED(PolyWord a)              { return a.UnTagged(); }
inline POLYUNSIGNED UNTAGGED_UNSIGNED(PolyWord a)   { return a.UnTaggedUnsigned(); }


#define IS_INT(x) ((x).IsTagged())

/************************************************************************
 *
 * Low level definitions.
 *
 ************************************************************************/


/* length word flags */

/*
The following encodings are used:

(1) OBJ_PRIVATE_GC_BIT = 0
       A normal length word i.e. 7 more flags + 24 bit length.
       
(2) OBJ_PRIVATE_GC_BIT = 1, OBJ_PRIVATE_MUTABLE_BIT = 0
       A "normal" tombstone. Remaining 30 bits are (word-aligned) pointer >> 2.

(3) OBJ_PRIVATE_GC_BIT = 1, OBJ_PRIVATE_MUTABLE_BIT = 1
       A "depth" tombstone; used by share program. Remaining 30 bits are
       an unsigned integer representing the (approximate) depth of the object
       in the data-structure. 

SPF 24/1/95
*/
#define OBJ_PRIVATE_FLAGS_SHIFT     (8 * (sizeof(PolyWord) - 1))

#define _TOP_BYTE(x)                ((POLYUNSIGNED)(x) << OBJ_PRIVATE_FLAGS_SHIFT)

// Bottom two bits define the content format.
// Zero bits mean ordinary word object containing addresses or tagged integers.
#define F_BYTE_OBJ                  0x01  /* byte object (contains no pointers) */
#define F_CODE_OBJ                  0x02  /* code object (mixed bytes and words) */
#define F_GC_MARK                   0x04  // Used during the GC marking phase
#define F_NO_OVERWRITE              0x08  /* don't overwrite when loading - mutables only. */
// This bit is overloaded and has different meanings depending on what other bits are set.
// For byte objects it is the sign bit for arbitrary precision ints.
// For other data it indicates either that the object is a profile block or contains
// information for allocation profiling.
#define F_NEGATIVE_BIT              0x10  // Sign bit for arbitrary precision ints (byte segs only)
#define F_PROFILE_BIT               0x10  // Object has a profile pointer (word segs only)
#define F_WEAK_BIT                  0x20  /* object contains weak references to option values. */
#define F_MUTABLE_BIT               0x40  /* object is mutable */
#define F_PRIVATE_GC_BIT            0x80  /* object is (pointer or depth) tombstone */
#define F_PRIVATE_FLAGS_MASK        0xFF

#define _OBJ_BYTE_OBJ                _TOP_BYTE(0x01)  /* byte object (contains no pointers) */
#define _OBJ_CODE_OBJ                _TOP_BYTE(0x02)  /* code object (mixed bytes and words) */
#define _OBJ_GC_MARK                 _TOP_BYTE(0x04)  // Mark bit
#define _OBJ_NO_OVERWRITE            _TOP_BYTE(0x08)  /* don't overwrite when loading - mutables only. */
#define _OBJ_NEGATIVE_BIT            _TOP_BYTE(0x10)  /* sign bit for arbitrary precision ints */
#define _OBJ_PROFILE_BIT             _TOP_BYTE(0x10)  /* sign bit for arbitrary precision ints */
#define _OBJ_WEAK_BIT                _TOP_BYTE(0x20)
#define _OBJ_MUTABLE_BIT             _TOP_BYTE(0x40)  /* object is mutable */
#define _OBJ_PRIVATE_GC_BIT          _TOP_BYTE(0x80)  /* object is (pointer or depth) tombstone */
#define _OBJ_PRIVATE_FLAGS_MASK      _TOP_BYTE(0xFF)
#define _OBJ_PRIVATE_LENGTH_MASK     ((-1) ^ _OBJ_PRIVATE_FLAGS_MASK)
#define MAX_OBJECT_SIZE              _OBJ_PRIVATE_LENGTH_MASK

#define _OBJ_PRIVATE_DEPTH_MASK      (_OBJ_PRIVATE_GC_BIT|_OBJ_MUTABLE_BIT)

/* case 1 - proper length words */
inline bool OBJ_IS_LENGTH(POLYUNSIGNED L)               { return ((L & _OBJ_PRIVATE_GC_BIT) == 0); }

/* these should only be applied to proper length words */
/* discards GC flag, mutable bit and weak bit. */
inline byte GetTypeBits(POLYUNSIGNED L)             { return (byte)(L >> OBJ_PRIVATE_FLAGS_SHIFT) & 0x03; }

inline POLYUNSIGNED OBJ_OBJECT_LENGTH(POLYUNSIGNED L)   { return L & _OBJ_PRIVATE_LENGTH_MASK; }
inline bool OBJ_IS_BYTE_OBJECT(POLYUNSIGNED L)          { return (GetTypeBits(L) == F_BYTE_OBJ); }
inline bool OBJ_IS_CODE_OBJECT(POLYUNSIGNED L)          { return (GetTypeBits(L) == F_CODE_OBJ); }
inline bool OBJ_IS_NO_OVERWRITE(POLYUNSIGNED L)         { return ((L & _OBJ_NO_OVERWRITE) != 0); }
inline bool OBJ_IS_NEGATIVE(POLYUNSIGNED L)             { return ((L & _OBJ_NEGATIVE_BIT) != 0); }
inline bool OBJ_HAS_PROFILE(POLYUNSIGNED L)             { return ((L & _OBJ_PROFILE_BIT) != 0); }
inline bool OBJ_IS_MUTABLE_OBJECT(POLYUNSIGNED L)       { return ((L & _OBJ_MUTABLE_BIT) != 0); }
inline bool OBJ_IS_WEAKREF_OBJECT(POLYUNSIGNED L)       { return ((L & _OBJ_WEAK_BIT) != 0); }

/* Don't need to worry about whether shift is signed, 
   because OBJ_PRIVATE_USER_FLAGS_MASK removes the sign bit.
   We don't want the GC bit (which should be 0) anyway.
*/
#define OBJ_PRIVATE_USER_FLAGS_MASK     _TOP_BYTE(0x7F)

#define OBJ_IS_WORD_OBJECT(L)           (GetTypeBits(L) == 0)

/* case 2 - forwarding pointer */
inline bool OBJ_IS_POINTER(POLYUNSIGNED L)  { return (L & _OBJ_PRIVATE_DEPTH_MASK) == _OBJ_PRIVATE_GC_BIT; }
inline PolyObject *OBJ_GET_POINTER(POLYUNSIGNED L) { return (PolyObject*)(( L & ~_OBJ_PRIVATE_DEPTH_MASK) <<2); }
inline POLYUNSIGNED OBJ_SET_POINTER(PolyObject *pt) { return ((POLYUNSIGNED)pt >> 2) | _OBJ_PRIVATE_GC_BIT; }

/* case 3 - depth */
inline bool OBJ_IS_DEPTH(POLYUNSIGNED L)  { return (L & _OBJ_PRIVATE_DEPTH_MASK) == _OBJ_PRIVATE_DEPTH_MASK; }
inline POLYUNSIGNED OBJ_GET_DEPTH(POLYUNSIGNED L) { return L & ~_OBJ_PRIVATE_DEPTH_MASK; }
inline POLYUNSIGNED OBJ_SET_DEPTH(POLYUNSIGNED n) { return n | _OBJ_PRIVATE_DEPTH_MASK; }
// or sharing chain
inline bool OBJ_IS_CHAINED(POLYUNSIGNED L)  { return (L & _OBJ_PRIVATE_DEPTH_MASK) == _OBJ_PRIVATE_DEPTH_MASK; }
inline PolyObject *OBJ_GET_CHAIN(POLYUNSIGNED L) { return (PolyObject*)(( L & ~_OBJ_PRIVATE_DEPTH_MASK) <<2); }
inline POLYUNSIGNED OBJ_SET_CHAIN(PolyObject *pt) { return ((POLYUNSIGNED)pt >> 2) | _OBJ_PRIVATE_DEPTH_MASK; }

// An object i.e. a piece of allocated memory in the heap.  In the simplest case this is a
// tuple, a list cons cell, a string or a ref.  Every object has a length word in the word before
// where its address points.  The top byte of this contains flags. 
class PolyObject {
public:
    byte *AsBytePtr(void)const { return (byte*)this; }
    PolyWord *AsWordPtr(void)const { return (PolyWord*)this; }
    POLYUNSIGNED LengthWord(void)const { return ((PolyWord*)this)[-1].AsUnsigned(); }
    POLYUNSIGNED Length(void)const { return OBJ_OBJECT_LENGTH(LengthWord()); }

    // Get and set a word
    PolyWord Get(POLYUNSIGNED i) const { return ((PolyWord*)this)[i]; }
    void Set(POLYUNSIGNED i, PolyWord v) { ((PolyWord*)this)[i] = v; }
    PolyWord *Offset(POLYUNSIGNED i) const { return ((PolyWord*)this)+i; }

    // Create a length word from a length and the flags in the top byte. 
    void SetLengthWord(POLYUNSIGNED l, byte f)
        { ((POLYUNSIGNED*)this)[-1] = l | ((POLYUNSIGNED)f << OBJ_PRIVATE_FLAGS_SHIFT); }
    void SetLengthWord(POLYUNSIGNED l) { ((PolyWord*)this)[-1] = PolyWord::FromUnsigned(l); }

    bool IsByteObject(void) const { return OBJ_IS_BYTE_OBJECT(LengthWord()); }
    bool IsCodeObject(void) const { return OBJ_IS_CODE_OBJECT(LengthWord()); }
    bool IsWordObject(void) const { return OBJ_IS_WORD_OBJECT(LengthWord()); }
    bool IsMutable(void) const { return OBJ_IS_MUTABLE_OBJECT(LengthWord()); }
    bool IsWeakRefObject(void) const { return OBJ_IS_WEAKREF_OBJECT(LengthWord()); }
    bool IsNoOverwriteObject(void) const { return OBJ_IS_NO_OVERWRITE(LengthWord()); }

    bool ContainsForwardingPtr(void) const { return OBJ_IS_POINTER(LengthWord()); }
    PolyObject *GetForwardingPtr(void) const { return OBJ_GET_POINTER(LengthWord()); }
    void SetForwardingPtr(PolyObject *newp) { ((PolyWord*)this)[-1] = PolyWord::FromUnsigned(OBJ_SET_POINTER(newp)); }

    bool ContainsShareChain(void) const { return OBJ_IS_CHAINED(LengthWord()); }
    PolyObject *GetShareChain(void) const { return OBJ_GET_CHAIN(LengthWord()); }
    void SetShareChain(PolyObject *newp) { ((PolyWord*)this)[-1] = PolyWord::FromUnsigned(OBJ_SET_CHAIN(newp)); }

    bool ContainsNormalLengthWord(void) const { return OBJ_IS_LENGTH(LengthWord()); }

    // Find the start of the constant section for a piece of code.
    // The first of these is really only needed because we may have objects whose length
    // words have been overwritten.
    void GetConstSegmentForCode(POLYUNSIGNED obj_length, PolyWord * &cp, POLYUNSIGNED &count) const
    {
        PolyWord *last_word  = Offset(obj_length - 1); // Last word in the code
        count = last_word->AsUnsigned(); // This is the number of consts
        cp = last_word - count;
    }
    void GetConstSegmentForCode(PolyWord * &cp, POLYUNSIGNED &count) const
    {
        GetConstSegmentForCode(Length(), cp, count);
    }
    PolyWord *ConstPtrForCode(void) const
    {
        PolyWord *cp; POLYUNSIGNED count;
        GetConstSegmentForCode(cp, count);
        return cp;
    }
    // Follow a chain of forwarding pointers
    PolyObject *FollowForwardingChain(void)
    {
        if (ContainsForwardingPtr())
            return GetForwardingPtr()->FollowForwardingChain();
        else return this;
    }
};

/* There was a problem with version 2.95 on Sparc/Solaris at least.  The PolyObject
   class has no members so classes derived from it e.g. ML_Cons_Cell should begin at
   the beginning of the object.  Later versions of GCC get this right. */
#if defined(__GNUC__) && (__GNUC__ <= 2)
#error Poly/ML requires GCC version 3 or newer
#endif

inline POLYUNSIGNED GetLengthWord(PolyWord p) { return p.AsObjPtr()->LengthWord(); }
// Get the length of an object.
inline POLYUNSIGNED OBJECT_LENGTH(PolyWord p) { return OBJ_OBJECT_LENGTH(GetLengthWord(p)); }

// A list cell.  This can be passed to or returned from certain RTS functions.
class ML_Cons_Cell: public PolyObject {
public:
    PolyWord    h;
    PolyWord    t;

#define ListNull (TAGGED(0))

    static bool IsNull(PolyWord p) { return p == ListNull; }
};

/* An exception packet.  This contains an identifier (either a tagged integer for RTS
   exceptions or the address of a mutable for those created within ML), a string
   name for printing and an exception argument value. */
class PolyException: public PolyObject {
public:
    PolyWord    ex_id; /* Exc identifier */
    PolyWord    ex_name;/* Exc name */
    PolyWord    arg; /* Exc arguments */
    PolyWord    ex_location; // Location of "raise".  Always zero for RTS exceptions.
};

typedef PolyException poly_exn;

/* 
 * Stream tokens are pointers to UNTAGGED C integers 
 * (stored in byte objects) 
 */
class StreamToken: public PolyObject
{
public:
    POLYUNSIGNED streamNo;
};

/* Macro to round a number of bytes up to a number of words. */
#define WORDS(s) ((s+sizeof(PolyWord)-1)/sizeof(PolyWord))

/**********************************************************************
 *
 * Representation of option type.
 *
 **********************************************************************/
#define NONE_VALUE      (TAGGED(0))
/* SOME x is represented by a single word cell containing x. */

#if (defined(_WIN32))
/* Windows doesn't include 0x in %p format. */
#define ZERO_X  "0x"
#else
#define ZERO_X  ""
#endif

#endif
