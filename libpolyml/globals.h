/*
    Title:  Globals for the system.
    Author:     Dave Matthews, Cambridge University Computer Laboratory

    Copyright David C. J. Matthews 2017-22, 2026

    Copyright (c) 2000-7
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

#ifndef _GLOBALS_H
#define _GLOBALS_H

/*
    Poly words, pointers and cells (objects).

    The garbage collector needs to be able to distinguish different uses of a
    memory word.  We need to be able find which words are pointers to other
    objects and which are simple integers.  The simple distinction is between
    integers, which are tagged by having the bottom bit set, and Addresses
    which are word aligned (bottom 2 bits zero on a 32 bit machine, bottom 3
    bits on a 64 bit machine, bottom bit in 32-in-64).  

    Addresses always point to the start of cells.  The preceding word of a
    cell is the length word.  This contains the
    length of the cell in words in the low-order 3 (7 in native 64-bits)
    bytes and a flag byte in the top byte.  The flags give information about
    the type of the object.  The length word is also used by the garbage
    collector and other object processors.
*/

#if HAVE_STDINT_H
#  include <stdint.h>
#endif

#if HAVE_INTTYPES_H
#  ifndef __STDC_FORMAT_MACROS
#    define __STDC_FORMAT_MACROS
#  endif
#  include <inttypes.h>

#elif (defined(_MSC_VER) && (_MSC_VER >= 1900))
// In VS 2015 and later we need to use <cinttypes>
#  include <cinttypes>  
#endif

#ifdef HAVE_STDDEF_H
#  include <stddef.h>
#endif

#define POLY_TAGSHIFT    1

#if (defined(_WIN32))
#  include <windows.h>
#endif

#ifdef POLYML32IN64
typedef int32_t         POLYSIGNED;
typedef uint32_t        POLYUNSIGNED;
#define SIZEOF_POLYWORD 4
#else
typedef intptr_t        POLYSIGNED;
typedef uintptr_t       POLYUNSIGNED;
#define SIZEOF_POLYWORD SIZEOF_VOIDP
#endif

// libpolyml uses printf-style I/O instead of C++ standard IOstreams,
// so we need specifier to format POLYUNSIGNED/POLYSIGNED values.
#ifdef POLYML32IN64
#if (POLYML32IN64 != 2 && POLYML32IN64 != 4)
#error The value of POLYML32IN64 must be 2 (default space size) or 4 (large space size)
#endif
#if (defined(PRIu32))
#  define POLYUFMT PRIu32
#  define POLYSFMT PRId32
#elif (defined(_MSC_VER))
#  define POLYUFMT "lu"
#  define POLYSFMT "ld"
#else
#  define POLYUFMT "u"
#  define POLYSFMT "d"
#endif
#elif (defined(PRIuPTR))
#  define POLYUFMT PRIuPTR
#  define POLYSFMT PRIdPTR
#elif (defined(_MSC_VER) && (SIZEOF_POLYWORD == 8))
#  define POLYUFMT "llu"
#  define POLYSFMT "lld"
#else
#  define POLYUFMT "lu"         // as before.  Cross your fingers.
#  define POLYSFMT "ld"         // idem.
#endif

// We can use the C99 %zu in most cases except MingW since it uses
// the old msvcrt and that only supports C89.
#if (defined(_WIN32) && (! defined(_MSC_VER) || _MSC_VER < 1800))
#  if (SIZEOF_VOIDP == 8)
#    define PRI_SIZET PRIu64
#  else
#    define PRI_SIZET PRIu32
#  endif
#else
#   define PRI_SIZET "zu"
#endif

typedef unsigned char   byte;

class PolyObject;
typedef PolyObject *POLYOBJPTR;

#ifdef POLYML32IN64
class PolyWord;
extern PolyWord *globalHeapBase, *globalCodeBase;
typedef uint32_t POLYOBJECTPTR; // This is an index into globalHeapBase

// If a 64-bit value if in the range of the object pointers.
inline bool IsHeapAddress(void *addr) { return (uintptr_t)addr <= 0xffffffff; }
#else
typedef POLYOBJPTR POLYOBJECTPTR;
inline bool IsHeapAddress(void *) { return true; }
#endif

typedef byte *POLYCODEPTR;

class PolyWord {
public:
    // Initialise to TAGGED(0).  This is very rarely used.
    PolyWord() { contents.unsignedInt = 1; }

    // Integers need to be tagged.
    static PolyWord TaggedInt(POLYSIGNED s) { return PolyWord((s << POLY_TAGSHIFT) | (POLYSIGNED)0x01); }
    static PolyWord TaggedUnsigned(POLYUNSIGNED u) { return PolyWord((u << POLY_TAGSHIFT) | 0x01); }

    static PolyWord FromStackAddr(PolyWord *sp) { return PolyWord(sp); }
    static PolyWord FromCodePtr(POLYCODEPTR p) { return PolyWord(p); }

    // Tests for the various cases.
    bool IsTagged(void) const { return (contents.unsignedInt & 1) != 0; }
#ifndef POLYML32IN64
    // In native 32-bit and 64-bit addresses are on word boundaries
    bool IsDataPtr(void) const { return (contents.unsignedInt & (sizeof(PolyWord) - 1)) == 0; }
#else
    // In 32-in-64 addresses are anything that isn't tagged.
    bool IsDataPtr(void) const { return (contents.unsignedInt & 1) == 0; }
#ifdef POLYML32IN64DEBUG
    static POLYOBJECTPTR AddressToObjectPtr(void *address);
#else
    static POLYOBJECTPTR AddressToObjectPtr(void *address)
        { return (POLYOBJECTPTR)(((PolyWord*)address - globalHeapBase)/(POLYML32IN64/2)); }
#endif
#endif

    // Extract the various cases.
    POLYSIGNED UnTagged(void) const { return contents.signedInt >> POLY_TAGSHIFT; }
    POLYUNSIGNED UnTaggedUnsigned(void) const { return contents.unsignedInt >> POLY_TAGSHIFT; }
#ifdef POLYML32IN64
    PolyWord(POLYOBJPTR p) { contents.objectPtr = AddressToObjectPtr(p); }
    PolyWord *AsStackAddr(PolyWord *base = globalHeapBase) const { return base + (((uintptr_t)contents.objectPtr) * (POLYML32IN64/2)); }
    POLYOBJPTR AsObjPtr(PolyWord *base = globalHeapBase) const { return (POLYOBJPTR)AsStackAddr(base); }
#else
    // An object pointer can become a word directly.
    PolyWord(POLYOBJPTR p) { contents.objectPtr = p; }
    POLYOBJPTR AsObjPtr(PolyWord *base = 0) const { return contents.objectPtr; }
    PolyWord *AsStackAddr(PolyWord *base=0) const { return (PolyWord *)contents.objectPtr; }
#endif
    POLYCODEPTR AsCodePtr(void) const { return (POLYCODEPTR)AsObjPtr(); }
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
#ifdef POLYML32IN64
    PolyWord(PolyWord *sp) { contents.objectPtr = AddressToObjectPtr(sp); }
    PolyWord(POLYCODEPTR p) { contents.objectPtr = AddressToObjectPtr(p); }
#else
    PolyWord(PolyWord *sp) { contents.objectPtr = (PolyObject*)sp; }
    PolyWord(POLYCODEPTR p) { contents.objectPtr = (PolyObject*)p; }
#endif
    union {
        POLYSIGNED      signedInt;      // A tagged integer - lowest bit set
        POLYUNSIGNED    unsignedInt;    // A tagged integer - lowest bit set
        POLYOBJECTPTR   objectPtr;      // Object pointer   - lowest bit clear.
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

/* length word flags */
#define OBJ_PRIVATE_FLAGS_SHIFT     (8 * (sizeof(PolyWord) - 1))

#define _TOP_BYTE(x)                ((POLYUNSIGNED)(x) << OBJ_PRIVATE_FLAGS_SHIFT)

// Bottom two bits define the content format.
// Zero bits mean ordinary word object containing addresses or tagged integers.
#define F_BYTE_OBJ                  0x01  /* byte object (contains no pointers) */
#define F_CODE_OBJ                  0x02  /* code object (mixed bytes and words) */
#define F_CLOSURE_OBJ               0x03  /* closure (32-in-64 only).  First word is code addr. */
#define F_GC_MARK                   0x04  // Used during the GC marking phase
#define F_NO_OVERWRITE              0x08  /* don't overwrite when loading - mutables only. */
// This bit is overloaded and has different meanings depending on what other bits are set.
// For byte objects it is the sign bit for arbitrary precision ints.
// For other data it indicates either that the object is a profile block or contains
// information for allocation profiling.
#define F_NEGATIVE_BIT              0x10  // Sign bit for arbitrary precision ints (byte segs only)
#define F_PROFILE_BIT               0x10  // Object has a profile pointer (word segs only)
#define F_WEAK_BIT                  0x20  /* object contains weak references to option values. */
// The Weak bit is only used on mutables.  The data sharing (sharedata.cpp) uses this with
// immutables to indicate that the length field is being used to store the "depth".
#define F_MUTABLE_BIT               0x40  /* object is mutable */
#define F_TOMBSTONE_BIT             0x80  // Object is a forwarding pointer
#define F_PRIVATE_FLAGS_MASK        0xFF

// Shifted bits
#define _OBJ_BYTE_OBJ                _TOP_BYTE(F_BYTE_OBJ)  /* byte object (contains no pointers) */
#define _OBJ_CODE_OBJ                _TOP_BYTE(F_CODE_OBJ)  /* code object (mixed bytes and words) */
#define _OBJ_CLOSURE_OBJ             _TOP_BYTE(F_CLOSURE_OBJ)  // closure (32-in-64 only).  First word is code addr.
#define _OBJ_GC_MARK                 _TOP_BYTE(F_GC_MARK)  // Mark bit
#define _OBJ_NO_OVERWRITE            _TOP_BYTE(F_NO_OVERWRITE)  /* don't overwrite when loading - mutables only. */
#define _OBJ_NEGATIVE_BIT            _TOP_BYTE(F_NEGATIVE_BIT)  /* sign bit for arbitrary precision ints */
#define _OBJ_PROFILE_BIT             _TOP_BYTE(F_PROFILE_BIT)  /* sign bit for arbitrary precision ints */
#define _OBJ_WEAK_BIT                _TOP_BYTE(F_WEAK_BIT)
#define _OBJ_MUTABLE_BIT             _TOP_BYTE(F_MUTABLE_BIT)  /* object is mutable */
#define _OBJ_TOMBSTONE_BIT           _TOP_BYTE(F_TOMBSTONE_BIT)  // object is a tombstone.
#define _OBJ_PRIVATE_FLAGS_MASK      _TOP_BYTE(F_PRIVATE_FLAGS_MASK)
#define _OBJ_PRIVATE_LENGTH_MASK     ((-1) ^ _OBJ_PRIVATE_FLAGS_MASK)
#define MAX_OBJECT_SIZE              _OBJ_PRIVATE_LENGTH_MASK

// 
inline bool OBJ_IS_LENGTH(POLYUNSIGNED L)               { return ((L & _OBJ_TOMBSTONE_BIT) == 0); }

/* these should only be applied to proper length words */
/* discards GC flag, mutable bit and weak bit. */
inline byte GetTypeBits(POLYUNSIGNED L)             { return (byte)(L >> OBJ_PRIVATE_FLAGS_SHIFT) & 0x03; }

inline POLYUNSIGNED OBJ_OBJECT_LENGTH(POLYUNSIGNED L)   { return L & _OBJ_PRIVATE_LENGTH_MASK; }
inline bool OBJ_IS_BYTE_OBJECT(POLYUNSIGNED L)          { return (GetTypeBits(L) == F_BYTE_OBJ); }
inline bool OBJ_IS_CODE_OBJECT(POLYUNSIGNED L)          { return (GetTypeBits(L) == F_CODE_OBJ); }
inline bool OBJ_IS_CLOSURE_OBJECT(POLYUNSIGNED L)       { return (GetTypeBits(L) == F_CLOSURE_OBJ); }
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
inline bool OBJ_IS_POINTER(POLYUNSIGNED L)  { return (L & _OBJ_TOMBSTONE_BIT) != 0; }
#ifdef POLYML32IN64
inline PolyObject* OBJ_GET_POINTER(POLYUNSIGNED L) { return (PolyObject*)(globalHeapBase + (((uintptr_t)L & ~_OBJ_TOMBSTONE_BIT) * POLYML32IN64)); }
inline POLYUNSIGNED OBJ_SET_POINTER(PolyObject* pt) { return PolyWord::AddressToObjectPtr(pt) >> 1 | _OBJ_TOMBSTONE_BIT; }
#else
inline PolyObject *OBJ_GET_POINTER(POLYUNSIGNED L) { return (PolyObject*)(( L & ~_OBJ_TOMBSTONE_BIT) <<2); }
inline POLYUNSIGNED OBJ_SET_POINTER(PolyObject *pt) { return ((POLYUNSIGNED)pt >> 2) | _OBJ_TOMBSTONE_BIT; }
#endif

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
    bool IsClosureObject(void) const { return OBJ_IS_CLOSURE_OBJECT(LengthWord()); }
    bool IsWordObject(void) const { return OBJ_IS_WORD_OBJECT(LengthWord()); }
    bool IsMutable(void) const { return OBJ_IS_MUTABLE_OBJECT(LengthWord()); }
    bool IsWeakRefObject(void) const { return OBJ_IS_WEAKREF_OBJECT(LengthWord()); }
    bool IsNoOverwriteObject(void) const { return OBJ_IS_NO_OVERWRITE(LengthWord()); }

    bool ContainsForwardingPtr(void) const { return OBJ_IS_POINTER(LengthWord()); }
    PolyObject *GetForwardingPtr(void) const { return OBJ_GET_POINTER(LengthWord()); }
    void SetForwardingPtr(PolyObject *newp) { ((PolyWord*)this)[-1] = PolyWord::FromUnsigned(OBJ_SET_POINTER(newp)); }

    bool ContainsNormalLengthWord(void) const { return OBJ_IS_LENGTH(LengthWord()); }

    // Follow a chain of forwarding pointers
    PolyObject *FollowForwardingChain(void)
    {
        if (ContainsForwardingPtr())
            return GetForwardingPtr()->FollowForwardingChain();
        else return this;
    }
};

// Stacks are native-words size even in 32-in-64.
union stackItem
{
    stackItem(PolyWord v) { argValue = v.AsUnsigned(); }
    stackItem() { argValue = TAGGED(0).AsUnsigned(); }

    // These return the low order word.
    PolyWord w()const { return PolyWord::FromUnsigned((POLYUNSIGNED)argValue); }
    operator PolyWord () { return PolyWord::FromUnsigned((POLYUNSIGNED)argValue); }
    POLYCODEPTR codeAddr; // Return addresses
    stackItem* stackAddr; // Stack addresses
    uintptr_t argValue; // Treat an address as an int
    PolyObject* absAddress; // It could be an absolute address to the heap
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


// ARM instructions are always little-endian even in big-endian mode
#ifdef WORDS_BIGENDIAN
inline uint32_t reverseBytes32(uint32_t value)
{
    return (((value & 0x000000ff) << 24) |
        ((value & 0x0000ff00) << 8) |
        ((value & 0x00ff0000) >> 8) |
        ((value & 0xff000000) >> 24));
}
inline uint32_t fromARMInstr(uint32_t instr) { return reverseBytes32(instr); }
inline uint32_t toARMInstr(uint32_t instr) { return reverseBytes32(instr); }
#else
inline uint32_t fromARMInstr(uint32_t instr) { return instr; }
inline uint32_t toARMInstr(uint32_t instr) { return instr; }
#endif

#endif
