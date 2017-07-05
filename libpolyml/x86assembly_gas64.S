/*
   Title:  Assembly code routines for the poly system.
   Author:    David Matthews
   Copyright (c) David C. J. Matthews 2000-2016
 
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
   This is the 64-bit gas version of the assembly code file.
   There are separate versions of 32/64 and MAMS (Intel syntax) and
   and GCC (gas syntax).
*/

/*
 Registers used :-

  %rax: First argument to function.  Result of function call.
  %rbx: Second argument to function.
  %rcx: General register
  %rdx: Closure pointer in call.
  %rbp: Points to memory used for extra registers
  %rsi: General register.
  %rdi: General register.
  %rsp: Stack pointer.
  %r8:   Third argument to function
  %r9:   Fourth argument to function
  %r10:  Fifth argument to function
  %r11:  General register
  %r12:  General register
  %r13:  General register
  %r14:  General register
  %r15:  Memory allocation pointer
*/


#include "config.h"
#ifdef SYMBOLS_REQUIRE_UNDERSCORE
#define EXTNAME(x)  _##x
#else
#define EXTNAME(x)  x
#endif

/* Macro to begin the hand-coded functions */
#ifdef MACOSX
#define GLOBAL .globl
#else
#define GLOBAL .global
#endif

#define INLINE_ROUTINE(id) \
GLOBAL EXTNAME(id); \
EXTNAME(id):

.set    TAG,        1
.set    TAGSHIFT,   1
.set    TAGMULT,    (1 << TAGSHIFT)

#define TAGGED(i) ((i << TAGSHIFT) | TAG)
#define MAKETAGGED(from,to)     leaq    TAG(,from,2),to

#define POLYWORDSIZE    8

#define NIL         TAGGED(0)
#define UNIT        TAGGED(0)
#define ZERO        TAGGED(0)
#define FALSE       TAGGED(0)
#define TRUE        TAGGED(1)
#define MINUS1      TAGGED(0-1)

#define B_bytes     0x01
#define B_typeBits  0x03
#define B_mutable   0x40

#define Max_Length  0x00ffffffffffffff

/* The "stack frame" pointed to by rbp acts as the "memory registers". */
#define Fr_RaiseOverflow        -120
#define Fr_HeapOverflow         -104
#define Fr_StackOverflow        -96
#define Fr_StackOverflowEx      -88
#define Fr_HandlerRegister      -80
#define Fr_LocalMbottom         -72
#define Fr_StackLimit           -64

/* The Windows calling convention reserves space for arguments in the
   call.  In Unix we instead save the argument in an unused save location. */
#ifdef _WIN32
#define Fr_Argument             16
#else
#define Fr_Argument             -48
#endif

/* Extra entries on the C stack */
#define Fr_SavedMLSP            -128

#define Fr_Size                 144         /* Must be multiple of 16 to get alignment correct */

/* This is the argument vector passed in to X86AsmSwitchToPoly
   It is used to initialise the frame.  A few values are updated
   when ML returns. */
#define Arg_LocalMpointer       0x0
#define Arg_HandlerRegister     0x8
#define Arg_LocalMbottom        0x10
#define Arg_StackLimit          0x18
#define Arg_ExceptionPacket     0x20  /* Address of packet to raise */
#define Arg_RequestCode         0x28  /* Byte: Io function to call. */
#define Arg_ReturnReason        0x2a  /* Byte: Reason for returning from ML. */
#define Arg_FullRestore         0x2b  /* Byte: Full/partial restore */
#define Arg_PolyStack           0x30  /* Current stack base */
#define Arg_ThreadId            0x38  /* My thread id */
#define Arg_StackPtr            0x40  /* Stack Pointer */
#define Arg_ProgramCtr          0x48
#define Arg_SaveRAX             0x50
#define Arg_SaveRBX             0x58
#define Arg_SaveRCX             0x60
#define Arg_SaveRDX             0x68
#define Arg_SaveRSI             0x70
#define Arg_SaveRDI             0x78
#define Arg_SaveR8              0x80
#define Arg_SaveR9              0x88
#define Arg_SaveR10             0x90
#define Arg_SaveR11             0x98
#define Arg_SaveR12             0xa0
#define Arg_SaveR13             0xa8
#define Arg_SaveR14             0xb0
#define Arg_SaveXMM0            0xb8
#define Arg_SaveXMM1            0xc0
#define Arg_SaveXMM2            0xc8
#define Arg_SaveXMM3            0xd0
#define Arg_SaveXMM4            0xd8
#define Arg_SaveXMM5            0xe0
#define Arg_SaveXMM6            0xe8

/* IO function numbers.  These are functions that are called
   to handle special cases in this code */
#include "sys.h"

#define RETURN_HEAP_OVERFLOW        1
#define RETURN_STACK_OVERFLOW       2
#define RETURN_STACK_OVERFLOWEX     3
#define RETURN_CALLBACK_RETURN      6
#define RETURN_CALLBACK_EXCEPTION   7
#define RETURN_RAISE_OVERFLOW       8

# Mark the stack as non-executable when supported
#ifdef HAVE_GNU_STACK
.section .note.GNU-stack, "", @progbits
#endif

#
# CODE STARTS HERE
#
    .text

/* Define standard call macro. CALL_IO ioCallNo  where ioCallNo is the io function to call.
   We need to include M_Redx in the register sets.  MD_set_for_retry may modify it
   if the function was called directly and not via the closure register. */

#define CALL_IO(index) \
        movq  Fr_Argument(%rbp),%rcx; \
        movb  $index,Arg_RequestCode(%rcx); \
        jmp   SaveStateAndReturnLocal;

#define CALL_EXTRA(index) \
        pushq %rcx; \
        movq  Fr_Argument(%rbp),%rcx; \
        movb  $index,Arg_ReturnReason(%rcx); \
        popq  %rcx; \
        jmp   SaveFullState;


/* Load the registers from the ML stack and jump to the code.
  This is used to start ML code.
  The argument is the address of the MemRegisters struct and goes into %rbp.
  This is the general code for switching control to ML.  There are a number of cases to consider:
  1.  Initial entry to root function or a new thread.  Needs to load EDX at least.
  2.  Normal return from an RTS call.  Could just do a simple return.
  3.  Exception raised in RTS call.
  4.  Callback from C to an ML function.  In effect this is a coroutine. Similar to 1.
  5.  Return from "trap" i.e. Heap/Stack overflow.  Stack-overflow can result in an exception
      either because the stack can't be grown or because Interrupt has been raised. */
INLINE_ROUTINE(X86AsmSwitchToPoly)
    pushq   %rbp                            # Standard entry sequence
    movq    %rsp,%rbp
    pushq   %rbx
    pushq   %r12
    pushq   %r13
    pushq   %r14
    pushq   %r15
#ifdef _WIN32
    pushq   %rdi                            # Callee save in Windows
    pushq   %rsi
    subq    $(Fr_Size-56),%rsp              # Argument is already in %rcx
#else
    subq    $(Fr_Size-40),%rsp
    movq    %rdi,%rcx                       # Argument is in %rdi
#endif

    movq    %rcx,Fr_Argument(%rbp)
    movq    Arg_StackLimit(%rcx),%rax
    movq    %rax,Fr_StackLimit(%rbp)
    movq    Arg_LocalMbottom(%rcx),%rax
    movq    %rax,Fr_LocalMbottom(%rbp)
    movq    Arg_HandlerRegister(%rcx),%rax
    movq    %rax,Fr_HandlerRegister(%rbp)
    lea     callExStackEx(%rip),%rax
    movq    %rax,Fr_StackOverflowEx(%rbp)
    lea     callExStack(%rip),%rax
    movq    %rax,Fr_StackOverflow(%rbp)
    lea     callExHeap(%rip),%rax
    movq    %rax,Fr_HeapOverflow(%rbp)
    lea     callExRaiseOverflow(%rip),%rax
    movq    %rax,Fr_RaiseOverflow(%rbp)
    movq    Arg_LocalMpointer(%rcx),%r15
    movq    Arg_StackPtr(%rcx),%rsp               # Set the new stack ptr
    movq    Arg_ExceptionPacket(%rcx),%rax
    cmpq    $1,%rax                             # Did we raise an exception?
    jnz     raisexLocal
    testb   $1,Arg_FullRestore(%rcx)            # Should we restore or clear the regs?
    jnz     sw2polyfull
/* We're returning from an RTS call. */
    movq    Arg_SaveRAX(%rcx),%rax
    cld                                         # Clear this just in case
    jmp     *Arg_ProgramCtr(%rcx)                # Jump to code address

sw2polyfull:
    movsd   Arg_SaveXMM0(%rcx),%xmm0
    movsd   Arg_SaveXMM1(%rcx),%xmm1
    movsd   Arg_SaveXMM2(%rcx),%xmm2
    movsd   Arg_SaveXMM3(%rcx),%xmm3
    movsd   Arg_SaveXMM4(%rcx),%xmm4
    movsd   Arg_SaveXMM5(%rcx),%xmm5
    movsd   Arg_SaveXMM6(%rcx),%xmm6
    movq    Arg_SaveRAX(%rcx),%rax              # Load the registers
    movq    Arg_SaveRBX(%rcx),%rbx
    movq    Arg_SaveRDX(%rcx),%rdx
    movq    Arg_SaveRSI(%rcx),%rsi
    movq    Arg_SaveRDI(%rcx),%rdi
    movq    Arg_SaveR8(%rcx),%r8
    movq    Arg_SaveR9(%rcx),%r9
    movq    Arg_SaveR10(%rcx),%r10
    movq    Arg_SaveR11(%rcx),%r11
    movq    Arg_SaveR12(%rcx),%r12
    movq    Arg_SaveR13(%rcx),%r13
    movq    Arg_SaveR14(%rcx),%r14
    cld                                     # Clear this just in case
    push    Arg_ProgramCtr(%rcx)
    movq    Arg_SaveRCX(%rcx),%rcx
    ret                                     # Jump to code address

/* This is exactly the same as raisex but seems to be needed to work round a PIC problem. */
raisexLocal:
    movq    Fr_HandlerRegister(%rbp),%rcx    # Get next handler into %rcx
    jmp     *(%rcx)

/* Code to save the state and switch to C
   This saves the full register state. */
SaveFullState:
    pushq   %rax                                # Save rax
    movq    Fr_Argument(%rbp),%rax
    movq    %rbx,Arg_SaveRBX(%rax)
    movq    %rcx,Arg_SaveRCX(%rax)
    movq    %rdx,Arg_SaveRDX(%rax)
    movq    %rsi,Arg_SaveRSI(%rax)
    movq    %rdi,Arg_SaveRDI(%rax)
    movsd   %xmm0,Arg_SaveXMM0(%rax)
    movsd   %xmm1,Arg_SaveXMM1(%rax)
    movsd   %xmm2,Arg_SaveXMM2(%rax)
    movsd   %xmm3,Arg_SaveXMM3(%rax)
    movsd   %xmm4,Arg_SaveXMM4(%rax)
    movsd   %xmm5,Arg_SaveXMM5(%rax)
    movsd   %xmm6,Arg_SaveXMM6(%rax)
    movq    %r8,Arg_SaveR8(%rax)
    movq    %r9,Arg_SaveR9(%rax)
    movq    %r10,Arg_SaveR10(%rax)
    movq    %r11,Arg_SaveR11(%rax)
    movq    %r12,Arg_SaveR12(%rax)
    movq    %r13,Arg_SaveR13(%rax)
    movq    %r14,Arg_SaveR14(%rax)
    popq    %rbx                                # Get old rax value
    movq    %rbx,Arg_SaveRAX(%rax)
    movq    %rsp,Arg_StackPtr(%rax)
    movq    Fr_Argument(%rbp),%rax
    movq    %r15,Arg_LocalMpointer(%rax)        # Save back heap pointer
    movq    Fr_HandlerRegister(%rbp),%rbx       # and handler pointer
    movq    %rbx,Arg_HandlerRegister(%rax)
    movq    %rbp,%rsp                           # Restore C stack pointer
#ifdef _WIN32
    subq    $56,%rsp
    popq    %rsi
    popq    %rdi
#else
    subq    $40,%rsp
#endif
    popq    %r15                                # Restore callee-save registers
    popq    %r14
    popq    %r13
    popq    %r12
    popq    %rbx
    popq    %rbp
    ret

callExHeap:
    CALL_EXTRA(RETURN_HEAP_OVERFLOW)
callExStack:
    CALL_EXTRA(RETURN_STACK_OVERFLOW)
callExStackEx:
    CALL_EXTRA(RETURN_STACK_OVERFLOWEX)
callExRaiseOverflow:
    CALL_EXTRA(RETURN_RAISE_OVERFLOW)

# As X86AsmSaveFullState but only save what is necessary for an RTS call.
INLINE_ROUTINE(X86AsmSaveStateAndReturn)
SaveStateAndReturnLocal: # This is necessary so that the jmps use a PC-relative address
    jmp SaveFullState

/* Used when entering new code.  The argument and closure are on the stack
   in case there is a GC before we enter the code. */
INLINE_ROUTINE(X86AsmPopArgAndClosure)
    popq    %rdx
    popq    %rax
    jmp     *(%rdx)

/* CallcodeTupled.  This is currently only used to "fold" RTS functions that are applied
   to constant arguments.  Oddly, compared with other RTS functions, it takes a single
   argument that is a pair containing the function closure and an argument vector. */
INLINE_ROUTINE(callcodeTupled)
    movq    (%rax),%rdx                         # closure
    movq    POLYWORDSIZE(%rax),%rsi            # address of arg vector
    cmpq    $NIL,%rsi                      # If calling a function without args this could be nil
    je      cct2
    movq    $Max_Length,%rcx               # Mask for length removing any flags
    andq    (-POLYWORDSIZE)(%rsi),%rcx          # Load and mask length
    jz      cct2
    movq    (%rsi),%rax                         # First argument
    addq    $POLYWORDSIZE,%rsi
    subq    $1,%rcx
    jz      cct2
    movq    (%rsi),%rbx                         # Second argument
    addq    $POLYWORDSIZE,%rsi
    subq    $1,%rcx
    jz      cct2
    movq    (%rsi),%r8                           # Third argument
    addq    $POLYWORDSIZE,%rsi
    subq    $1,%rcx
    jz      cct2
    movq    (%rsi),%r9                           # Fourth argument
    addq    $POLYWORDSIZE,%rsi
    subq    $1,%rcx
    jz      cct2
    movq    (%rsi),%r10                          # Fifth argument
    addq    $POLYWORDSIZE,%rsi
    subq    $1,%rcx
    jz      cct2
    popq    %rdi                                # Get the return address
cct1:                                           # Push the remaining args to the stack
    pushq   (%rsi)
    addq    $POLYWORDSIZE,%rsi
    LOOP    cct1
    pushq   %rdi                                # Push the return address
cct2:                                           # Finished - enter function
    movq    $UNIT,%rsi                     # Clobber, for the moment
    movq    $UNIT,%rcx
    jmp     *(%rdx)



/*
A number of functions implemented in Assembly for efficiency reasons
*/

INLINE_ROUTINE(int_to_word)
 # Extract the low order bits from a word.
    testq   $TAG,%rax
    jz      get_first_long_word_a1
    ret                 # Return the argument

 # This is now used in conjunction with isShort in Word.fromInt.
INLINE_ROUTINE(get_first_long_word_a)
get_first_long_word_a1:
    testb   $16,(-1)(%rax)     # 16 is the "negative" bit
    movq    (%rax),%rax     # Extract the word which is already little-endian
    jz      gfw1
    negq    %rax            # We can ignore overflow
gfw1:
    MAKETAGGED(%rax,%rax)
    ret



INLINE_ROUTINE(move_bytes)
/* Move a segment of memory from one location to another.
   Must deal with the case of overlapping segments correctly.
   (source, sourc_offset, destination, dest_offset, length) */

 # Assume that the offsets and length are all short integers.
    movq    %r8,%rdi               # Destination address
    movq    %r9,%rcx                # Destination offset, untagged
    shrq    $TAGSHIFT,%rcx
    addq    %rcx,%rdi
    movq    %rax,%rsi                   # Source address
    shrq    $TAGSHIFT,%rbx
    addq    %rbx,%rsi
    movq    %r10,%rcx                # Get the length to move
    shrq    $TAGSHIFT,%rcx
    cld                             # Default to increment %rdi,%rsi
    cmpq    %rdi,%rsi                   # Check for potential overlap
 # If dest > src then use decrementing moves else
 # use incrementing moves.
    ja      mvb1
    std                             # Decrement %rdi,%rsi
    leaq    (-1)(%rsi,%rcx),%rsi
    leaq    (-1)(%rdi,%rcx),%rdi
mvb1:
    rep; movsb                      # Copy the bytes
    movq    $UNIT,%rax              # The function returns unit
    movq    %rax,%rbx               # Clobber bad value in %rbx
    movq    %rax,%rcx               # and %rcx
    movq    %rax,%rdi
    movq    %rax,%rsi
 # Visual Studio 5 C++ seems to assume that the direction flag
 # is cleared.  I think that`s a bug but we have to go along with it.
    cld
    ret



INLINE_ROUTINE(move_words)
/* Move a segment of memory from one location to another.
   Must deal with the case of overlapping segments correctly.
   (source, source_offset, destination, dest_offset, length)
   Assume that the offsets and length are all short integers. */
    movq    %r8,%rdi               # Destination address
    movq    %r9,%rcx                # Destination offset
    leaq    (-4)(%rdi,%rcx,4),%rdi      # Destination address plus offset
    leaq    (-4)(%rax,%rbx,4),%rsi      # Source address plus offset
    movq    %r10,%rcx                # Get the length to move (words)
    shrq    $TAGSHIFT,%rcx
    cld                             # Default to increment %rdi,%rsi
    cmpq    %rdi,%rsi                   # Check for potential overlap
 # If dest > src then use decrementing moves else
 # use incrementing moves.
    ja      mvw1
    std                             # Decrement %rdi,%rsi

    leaq    (-POLYWORDSIZE)(%rsi,%rcx,POLYWORDSIZE),%rsi
    leaq    (-POLYWORDSIZE)(%rdi,%rcx,POLYWORDSIZE),%rdi

mvw1:
    rep; movsq                           # Copy the words
    movq    $UNIT,%rax             # The function returns unit
    movq    %rax,%rcx               # Clobber bad values
    movq    %rax,%rdi
    movq    %rax,%rsi
 # Visual Studio 5 C++ seems to assume that the direction flag
 # is cleared.  I think that`s a bug but we have to go along with it.
    cld
    ret


#

RetFalse:
    movq    $FALSE,%rax
    ret

RetTrue:
    movq    $TRUE,%rax
    ret

INLINE_ROUTINE(not_bool)
    xorq    $(TRUE-TAG),%rax   # Change the value but leave the tag
    ret


 # or, and, xor shift etc. assume the values are tagged integers
INLINE_ROUTINE(or_word)
    orq     %rbx,%rax
    ret

INLINE_ROUTINE(and_word)
    andq    %rbx,%rax
    ret


INLINE_ROUTINE(xor_word)
    xorq    %rbx,%rax
    orq     $TAG,%rax  # restore the tag
    ret


INLINE_ROUTINE(shift_left_word)
 # Assume that both args are tagged integers
 # Word.<<(a,b) is defined to return 0 if b > Word.wordSize
    cmpq    $TAGGED(63),%rbx
    jb      slw1
    movq    $ZERO,%rax
    ret
slw1:
    movq    %rbx,%rcx
    shrq    $TAGSHIFT,%rcx # remove tag
    subq    $TAG,%rax
    shlq    %cl,%rax
    orq     $TAG,%rax  # restore the tag
    movq    %rax,%rcx       # clobber %rcx
    ret


INLINE_ROUTINE(shift_right_word)
 # Word.>>(a,b) is defined to return 0 if b > Word.wordSize
    cmpq    $TAGGED(63),%rbx
    jb      srw1
    movq    $ZERO,%rax
    ret
srw1:
    movq    %rbx,%rcx
    shrq    $TAGSHIFT,%rcx # remove tag
    shrq    %cl,%rax
    orq     $TAG,%rax  # restore the tag
    movq    %rax,%rcx       # clobber %rcx
    ret


INLINE_ROUTINE(shift_right_arith_word)
 # Word.~>>(a,b) is defined to return 0 or ~1 if b > Word.wordSize
 # The easiest way to do that is to set the shift to 31.
    cmpq    $TAGGED(63),%rbx
    jb      sra1
    movq    $TAGGED(63),%rbx
sra1:
    movq    %rbx,%rcx
    shrq    $TAGSHIFT,%rcx # remove tag
    sarq    %cl,%rax
    orq     $TAG,%rax  # restore the tag
    movq    %rax,%rcx       # clobber %rcx
    ret


# Clears the "mutable" bit on a segment
INLINE_ROUTINE(locksega)
    andb    $(0xff-B_mutable),-1(%rax)
    movq     $TAGGED(0),%rax   # Return Unit,
    ret


INLINE_ROUTINE(get_length_a)
    movq    (-POLYWORDSIZE)(%rax),%rax
    shlq    $8,%rax            # Clear top byte
    shrq    $(8-TAGSHIFT),%rax  # Make it a tagged integer
    orq $TAG,%rax
    ret



INLINE_ROUTINE(is_shorta)
# Returns true if the argument is tagged
    andq    $TAG,%rax
    jz      RetFalse
    jmp     RetTrue


INLINE_ROUTINE(string_length)
    testq   $TAG,%rax  # Single char strings are represented by the
    jnz     RetOne      # character.
    movq    (%rax),%rax # Get length field
    MAKETAGGED(%rax,%rax)
    ret
RetOne: movq    $TAGGED(1),%rax
    ret


 # Store the length of a string in the first word.
INLINE_ROUTINE(set_string_length_a)
    shrq    $TAGSHIFT,%rbx # Untag the length
    movq    %rbx,(%rax)
    movq    $UNIT,%rax     # Return unit
    movq    %rax,%rbx           # Clobber untagged value
    ret


# raisex is only used in "raiseWithLocation" to bypass the normal ML "raise".
INLINE_ROUTINE(raisex)
    movq    Fr_HandlerRegister(%rbp),%rcx    # Get next handler into %rcx
    jmp     *(%rcx)

INLINE_ROUTINE(load_byte)
    movq    %rbx,%rdi
    shrq    $TAGSHIFT,%rdi
    movzbq  (%rax,%rdi,1),%rdi
    MAKETAGGED(%rdi,%rax)
    movq    %rax,%rdi       # Clobber bad value in %rdi
    ret


INLINE_ROUTINE(load_word)
    movq    (-4)(%rax,%rbx,4),%rax
    movq    %rax,%rbx
    ret


INLINE_ROUTINE(assign_byte)
# We can assume that the data value will not overflow 30 bits (it is only 1 byte!)
    movq    %r8,%rcx
    shrq    $TAGSHIFT,%rcx       # Remove tags from data value

# We can assume that the index will not overflow 30 bits i.e. it is a tagged short
    shrq    $TAGSHIFT,%rbx     # Remove tags from offset
    movb    %cl,(%rax,%rbx)

    movq    $UNIT,%rax             # The function returns unit
    movq    %rax,%rbx                   # Clobber bad value in %rbx
    movq    %rax,%rcx                   # and %rcx
    ret



INLINE_ROUTINE(assign_word)
    movq    %r8,(-4)(%rax,%rbx,4)      # The offset is tagged already
    movq    $UNIT,%rax           # The function returns unit
    ret


/* Allocate a piece of memory that does not need to be initialised.
   We can't actually risk leaving word objects uninitialised so for the
   moment we always initialise. */
INLINE_ROUTINE(alloc_uninit)
    movq    $ZERO,%r8

# Drop through into alloc_store


/* alloc(size, flags, initial).  Allocates a segment of a given size and
   initialises it.
 
  This is primarily used for arrays and for strings.  Refs are
  allocated using inline code. */
INLINE_ROUTINE(alloc_store)
allsts:
 # First check that the length is acceptable
    testq   $TAG,%rax
    jz      alloc_in_rts            # Get the RTS to raise an exception
    movq    %rax,%rdi
    shrq    $TAGSHIFT,%rdi     # Remove tag
    movq    $Max_Length,%rdx   # Length field must fit in 56 bits
    cmpq    %rdx,%rdi
    ja      alloc_in_rts            # Get the RTS to raise an exception
    addq    $1,%rdi            # Add 1 word
    shlq    $3,%rdi            # Get length in bytes
    movq    %r15,%rdx
    subq    %rdi,%rdx               # Allocate the space
    movq    %rax,%rdi               # Clobber bad value in %rdi
    cmpq    Fr_LocalMbottom(%rbp),%rdx            # Check for free space
    jb      alloc_in_rts
/* Normally the above test is sufficient but if LocalMpointer is near the bottom of
   memory and the store requested is very large the value in %rdx can be negative
   which is greater, unsigned, than LocalMbottom.  We have to check it is less
   than, unsigned, the allocation pointer. */
    cmpq    %r15,%rdx
    jnb     alloc_in_rts
    movq    %rdx,%r15                 # Put back in the heap ptr
    shrq    $TAGSHIFT,%rax
    movq    %rax,(-8)(%rdx)         # Put in length
    shrq    $TAGSHIFT,%rbx     # remove tag from flag
    orq     $B_mutable,%rbx    # set mutable bit
    movb    %bl,(-1)(%rdx)         # and put it in.
 # Initialise the store.
    movq    %rax,%rcx               # Get back the no. of words.
    movq    %r8,%rax                 # Get initial value.
    andb    $B_typeBits,%bl          # Mask off mutable, weak etc.
    cmpb    $B_bytes,%bl
    jne     allst2

 # If this is a byte seg
    shrq    $TAGSHIFT,%rax # untag the initialiser
    shlq    $3,%rcx        # Convert to bytes
    movq    %rdx,%rdi
    rep; stosb
    jmp     allst3

 # If this is a word segment
allst2:
    movq    %rdx,%rdi
    rep; stosq

allst3:
    movq    %rdx,%rax

    movq    %rax,%rcx       # Clobber these
    movq    %rax,%rdx
    movq    %rax,%rbx
    movq    %rax,%rdi
    ret


# This is used if we have reached the store limit and need to garbage-collect.
alloc_in_rts:
    movq    %rax,%rdx       # Clobber these first
    movq    %rax,%rdi
    CALL_IO(POLY_SYS_alloc_store)

INLINE_ROUTINE(touch_final)
# This is really a pseudo-op
    movq    $UNIT,%rax
    ret


INLINE_ROUTINE(add_long)
    movq    %rax,%rdi
    andq    %rbx,%rdi
    andq    $TAG,%rdi
    jz      add_really_long
    leaq    (-TAG)(%rax),%rdi
    addq    %rbx,%rdi
    jo      add_really_long
    movq    %rdi,%rax
    ret
add_really_long:
    movq    %rax,%rdi
    CALL_IO(POLY_SYS_aplus)


INLINE_ROUTINE(sub_long)
    movq    %rax,%rdi
    andq    %rbx,%rdi
    andq    $TAG,%rdi
    jz      sub_really_long
    movq    %rax,%rdi
    subq    %rbx,%rdi
    jo      sub_really_long
    leaq    TAG(%rdi),%rax      # Put back the tag
    movq    %rax,%rdi
    ret
sub_really_long:
    movq    %rax,%rdi
    CALL_IO(POLY_SYS_aminus)


INLINE_ROUTINE(mult_long)
    movq    %rax,%rdi
    andq    %rbx,%rdi
    andq    $TAG,%rdi
    jz      mul_really_long
    movq    %rbx,%rdi
    sarq    $TAGSHIFT,%rdi # Shift multiplicand
    movq    %rax,%rsi
    subq    $TAG,%rsi          # Just subtract off the tag off multiplier
    imulq   %rdi,%rsi
    jo      mul_really_long
    addq    $TAG,%rsi
    movq    %rsi,%rax
    movq    %rax,%rdi
    ret
mul_really_long:
    movq    %rax,%rsi       # Clobber this
    movq    %rax,%rdi
    CALL_IO(POLY_SYS_amul)


INLINE_ROUTINE(div_long)
    movq    %rax,%rdi
    andq    %rbx,%rdi
    andq    $TAG,%rdi          # %rdi now contains $0 or $1 (both legal!)
    jz      div_really_long
    cmpq    $TAGGED(0),%rbx    # Check that it's non-zero
    jz      div_really_long         # We don't want a trap.
 # The only case of overflow is dividing the smallest negative number by -1
    cmpq    $TAGGED((-1)),%rbx
    jz      div_really_long
    sarq    $TAGSHIFT,%rax
    movq    %rbx,%rdi
    sarq    $TAGSHIFT,%rdi
    cqo
    idiv    %rdi
    MAKETAGGED(%rax,%rax)
    movq    %rax,%rdx
    movq    %rax,%rdi
    ret
div_really_long:
    movq    %rax,%rdi
    CALL_IO(POLY_SYS_adiv)


INLINE_ROUTINE(rem_long)
    movq    %rax,%rdi
    andq    %rbx,%rdi
    andq    $TAG,%rdi      # %rdi now contains $0 or $1 (both legal!
    jz      rem_really_long
    cmpq    $TAGGED(0),%rbx    # Check that it's non-zero
    jz      rem_really_long         # We don't want a trap.
 # The only case of overflow is dividing the smallest negative number by -1
    cmpq    $TAGGED((-1)),%rbx
    jz      rem_really_long
    sarq    $TAGSHIFT,%rax
    movq    %rbx,%rdi
    sarq    $TAGSHIFT,%rdi
    cqo
    idiv    %rdi
    MAKETAGGED(%rdx,%rax)
    movq    %rax,%rdx
    movq    %rax,%rdi
    ret
rem_really_long:
    movq    %rax,%rdi
    CALL_IO(POLY_SYS_amod)


/* Combined quotient and remainder.  We have to use the long form
   if the arguments are long or there's an overflow.  The first two
   arguments are the values to be divided.  The third argument is the
   address where the results should be placed. */
INLINE_ROUTINE(quotrem_long)
    movq    %rax,%rdi
    andq    %rbx,%rdi
    andq    $TAG,%rdi
    jz      quotrem_really_long
    cmpq    $TAGGED(0),%rbx
    jz      quotrem_really_long
 # The only case of overflow is dividing the smallest negative number by -1
    cmpq    $TAGGED((-1)),%rbx
    jz      quotrem_really_long

 # Get the address for the result.
    movq    %r8,%rcx
# Do the division
    sarq    $TAGSHIFT,%rax
    movq    %rbx,%rdi
    sarq    $TAGSHIFT,%rdi
    cqo
    idiv    %rdi
    MAKETAGGED(%rax,%rax)
    MAKETAGGED(%rdx,%rdx)
    movq    %rax,%rdi
    movq    %rax,(%rcx)
    movq    %rdx,POLYWORDSIZE(%rcx)
    movq    %rcx,%rax
    ret

mem_for_remquot1:  # Not enough store: clobber bad value in ecx.
    movq   $1,%rcx

quotrem_really_long:
    movq    %rax,%rdi
    CALL_IO(POLY_SYS_quotrem)


# TODO: Isn't this byte equality?
INLINE_ROUTINE(equal_long)
    cmpq    %rax,%rbx
    je      RetTrue
    movq    %rax,%rcx   # If either is short
    orq     %rbx,%rax   # the result is false
    andq    $TAG,%rax
    jnz     RetFalse
    movq    %rcx,%rax
    CALL_IO(POLY_SYS_equala)



INLINE_ROUTINE(or_long)
#ifdef NOTATTHEMOMENT
    movq    %rax,%rdi
    andq    %rbx,%rdi
    andq    $TAG,%rdi
    jz      or_really_long
    orq     %rbx,%rax
    movq    %rax,%rdi
    ret
or_really_long:
#endif
    CALL_IO(POLY_SYS_ora)


INLINE_ROUTINE(xor_long)
#ifdef NOTATTHEMOMENT
    movq    %rax,%rdi
    andq    %rbx,%rdi
    andq    $TAG,%rdi
    jz      xor_really_long
    xorq    %rbx,%rax
    orq     $TAG,%rax  # restore the tag
    movq    %rax,%rdi
    ret
xor_really_long:
#endif
    CALL_IO(POLY_SYS_xora)


INLINE_ROUTINE(and_long)
#ifdef NOTATTHEMOMENT
    movq    %rax,%rdi
    andq    %rbx,%rdi
    andq    $TAG,%rdi
    jz      and_really_long
    andq    %rbx,%rax
    movq    %rax,%rdi
    ret
and_really_long:
#endif
    CALL_IO(POLY_SYS_anda)


INLINE_ROUTINE(neg_long)
    testq   $TAG,%rax
    jz      neg_really_long
    movq    $(TAGGED(0)+TAG),%rdi
    subq    %rax,%rdi
    jo      neg_really_long
    movq    %rdi,%rax
    ret
neg_really_long:
    movq    %rax,%rdi
    CALL_IO(POLY_SYS_aneg)


INLINE_ROUTINE(int_geq)
    testq   $TAG,%rax # Is first arg short?
    jz      igeq2
    testq   $TAG,%rbx # Is second arg short?
    jz      igeq1
    cmpq    %rbx,%rax
    jge     RetTrue
    jmp     RetFalse
igeq1:
 # First arg is short, second isn't
    testb   $16,(-1)(%rbx)     # 16 is the "negative" bit
    jnz     RetTrue     # Negative - always less
    jmp     RetFalse

igeq2:
 # First arg is long
    testq   $TAG,%rbx # Is second arg short?
    jz      igeq3
 # First arg is long, second is short
    testb   $16,(-1)(%rax)     # 16 is the "negative" bit
    jz      RetTrue    # Positive - always greater
    jmp     RetFalse

igeq3:
 # Both long
    CALL_IO(POLY_SYS_int_geq)



INLINE_ROUTINE(int_leq)
    testq   $TAG,%rax # Is first arg short?
    jz      ileq2
    testq   $TAG,%rbx # Is second arg short?
    jz      ileq1
    cmpq    %rbx,%rax
    jle     RetTrue
    jmp     RetFalse
ileq1:
 # First arg is short, second isn't
    testb   $16,(-1)(%rbx)     # 16 is the "negative" bit
    jz      RetTrue     # Negative - always less
    jmp     RetFalse

ileq2:
 # First arg is long
    testq   $TAG,%rbx # Is second arg short?
    jz      ileq3
 # First arg is long, second is short
    testb   $16,(-1)(%rax)     # 16 is the "negative" bit
    jnz     RetTrue    # Positive - always greater
    jmp     RetFalse

ileq3:
    CALL_IO(POLY_SYS_int_leq)



INLINE_ROUTINE(int_gtr)
    testq   $TAG,%rax # Is first arg short?
    jz      igtr2
    testq   $TAG,%rbx # Is second arg short?
    jz      igtr1
    cmpq    %rbx,%rax
    jg      RetTrue
    jmp     RetFalse
igtr1:
 # First arg is short, second isn't
    testb   $16,(-1)(%rbx)     # 16 is the "negative" bit
    jnz     RetTrue     # Negative - always less
    jmp     RetFalse

igtr2:
 # First arg is long
    testq   $TAG,%rbx # Is second arg short?
    jz      igtr3
 # First arg is long, second is short
    testb   $16,(-1)(%rax)     # 16 is the "negative" bit
    jz      RetTrue    # Positive - always greater
    jmp     RetFalse

igtr3:
    CALL_IO(POLY_SYS_int_gtr)


INLINE_ROUTINE(int_lss)
    testq   $TAG,%rax # Is first arg short?
    jz      ilss2
    testq   $TAG,%rbx # Is second arg short?
    jz      ilss1
    cmpq    %rbx,%rax
    jl      RetTrue
    jmp     RetFalse
ilss1:
 # First arg is short, second isn't
    testb   $16,(-1)(%rbx)     # 16 is the "negative" bit
    jz      RetTrue     # Negative - always less
    jmp     RetFalse

ilss2:
 # First arg is long
    testq   $TAG,%rbx # Is second arg short?
    jz      ilss3
 # First arg is long, second is short
    testb   $16,(-1)(%rax)     # 16 is the "negative" bit
    jnz     RetTrue    # Positive - always greater
    jmp     RetFalse

ilss3:
     CALL_IO(POLY_SYS_int_lss)


INLINE_ROUTINE(offset_address)
 # This is needed in the code generator, but is a very risky thing to do.
    shrq    $TAGSHIFT,%rbx     # Untag
    addq    %rbx,%rax       # and add in
    movq    %rax,%rbx
    ret


# General test routine.  Returns with the condition codes set
# appropriately.

teststr:
    testq   $TAG,%rax     # Is arg1 short
    jz      tststr1
    testq   $TAG,%rbx     # Yes: is arg2 also short?
    jz      tststr0a
    # Both are short - just compare the characters
    cmpq    %rbx,%rax
    ret

tststr0a:
    movq    $1,%rdi        # Is arg2 the null string ?
    cmpq    (%rbx),%rdi
    jg      tststr4            # Return with "gtr" set if it is
    shrq    $TAGSHIFT,%rax
    cmpb    POLYWORDSIZE(%rbx),%al
    jne     tststr4            # If they're not equal that's the result
    cmpq    $256,%rax     # But if they're equal set "less" because A is less than B
    jmp     tststr4

tststr1: # arg2 is not short.  Is arg1 ?
    testq   $TAG,%rbx
    jz      tststr2
    movq    (%rax),%rdi        # Is arg1 the null string
    cmpq    $1,%rdi
    jl      tststr4            # Return with "less" set if it is
    shrq    $TAGSHIFT,%rbx
    movb    POLYWORDSIZE(%rax),%cl
    cmpb    %bl,%cl
    jne     tststr4            # If they're not equal that's the result
    cmpq    $0,%rdi      # But if they're equal set "greater" because A is greater than B
    jmp     tststr4

tststr2:
    movq    (%rax),%rdi     # Get length.
    movq    (%rbx),%rcx     # 
    cmpq    %rcx,%rdi       # Find shorter length
    jge     tststr3
    movq    %rdi,%rcx
tststr3:
    leaq    POLYWORDSIZE(%rax),%rsi    # Load ptrs for cmpsb
    leaq    POLYWORDSIZE(%rbx),%rdi
    cld                 # Make sure we increment
    cmpq    %rax,%rax       # Set the Zero bit
    repe; cmpsb           # Compare while equal and %rcx > 0
    jnz     tststr4
 # Strings are equal as far as the shorter of the two.  Have to compare
 # the lengths.
    movq    (%rax),%rdi
    cmpq    (%rbx),%rdi
tststr4:
    movq    $1,%rax      # Clobber these
    movq    %rax,%rbx       
    movq    %rax,%rcx       
    movq    %rax,%rsi
    movq    %rax,%rdi
    ret

 # These functions compare strings for lexical ordering.  This version, at
 # any rate, assumes that they are UNSIGNED bytes.

INLINE_ROUTINE(str_compare)
    call    teststr
    ja      RetTrue         # Return TAGGED(1) if it's greater
    je      RetFalse        # Return TAGGED(0) if it's equal
    movq    $MINUS1,%rax   # Return TAGGED(-1) if it's less.
    ret



INLINE_ROUTINE(teststrgeq)
    call    teststr
    jnb     RetTrue
    jmp     RetFalse


INLINE_ROUTINE(teststrleq)
    call    teststr
    jna     RetTrue
    jmp     RetFalse


INLINE_ROUTINE(teststrlss)
    call    teststr
    jb      RetTrue
    jmp     RetFalse


INLINE_ROUTINE(teststrgtr)
    call    teststr
    ja      RetTrue
    jmp     RetFalse



INLINE_ROUTINE(bytevec_eq)
/* Compare arrays of bytes.  The arguments are the same as move_bytes.
   (source, sourc_offset, destination, dest_offset, length) */

/* Assume that the offsets and length are all short integers. */
    movq    %r8,%rdi                     # Destination address
    movq    %r9,%rcx                     # Destination offset, untagged
    shrq    $TAGSHIFT,%rcx
    addq    %rcx,%rdi
    movq    %rax,%rsi                   # Source address
    shrq    $TAGSHIFT,%rbx
    addq    %rbx,%rsi
    movq    %r10,%rcx                    # Get the length to move
    shrq    $TAGSHIFT,%rcx
    cld                     # Make sure we increment
    cmpq    %rax,%rax       # Set the Zero bit
    repe; cmpsb
    movq    %rax,%rsi       # Make these valid
    movq    %rax,%rcx
    movq    %rax,%rdi
    jz      bvTrue
    movq    $FALSE,%rax
    jmp     bvRet
bvTrue:
    movq    $TRUE,%rax
bvRet:
    ret




INLINE_ROUTINE(is_big_endian)
    jmp     RetFalse    # I386/486 is little-endian


INLINE_ROUTINE(bytes_per_word)
    movq    $TAGGED(POLYWORDSIZE),%rax  # 4/8 bytes per word
    ret


 # Word functions.  These are all unsigned and do not raise Overflow
 
INLINE_ROUTINE(mul_word)
    shrq    $TAGSHIFT,%rbx # Untag the multiplier
    subq    $TAG,%rax      # Remove the tag from the multiplicand
    mulq    %rbx                # unsigned multiplication
    addq    $TAG,%rax      # Add back the tag, but don`t shift
    movq    %rax,%rdx           # clobber this which has the high-end result
    movq    %rax,%rbx           # and the other bad result.
    ret


INLINE_ROUTINE(plus_word)
    leaq    (-TAG)(%rax,%rbx),%rax  # Add the values and subtract a tag
    ret


INLINE_ROUTINE(minus_word)
    subq    %rbx,%rax
    addq    $TAG,%rax          # Put back the tag
    ret


INLINE_ROUTINE(div_word)
    shrq    $TAGSHIFT,%rbx     # Check for division by zero is done in ML
    shrq    $TAGSHIFT,%rax
    movq    $0,%rdx
    div     %rbx
    MAKETAGGED(%rax,%rax)
    movq    %rax,%rdx
    movq    %rax,%rbx
    ret


INLINE_ROUTINE(mod_word)
    shrq    $TAGSHIFT,%rbx # Check for division by zero is done in ML
    shrq    $TAGSHIFT,%rax
    movq    $0,%rdx
    div     %rbx
    MAKETAGGED(%rdx,%rax)
    movq    %rax,%rdx
    movq    %rax,%rbx
    ret


INLINE_ROUTINE(word_eq)
    cmpq    %rbx,%rax
    jz      RetTrue         # True if they are equal.
    jmp     RetFalse


INLINE_ROUTINE(word_geq)
    cmpq    %rbx,%rax
    jnb     RetTrue
    jmp     RetFalse


INLINE_ROUTINE(word_leq)
    cmpq    %rbx,%rax
    jna     RetTrue
    jmp     RetFalse


INLINE_ROUTINE(word_gtr)
    cmpq    %rbx,%rax
    ja      RetTrue
    jmp     RetFalse


 INLINE_ROUTINE(word_lss)
    cmpq    %rbx,%rax
    jb      RetTrue
    jmp     RetFalse


INLINE_ROUTINE(fixed_geq)
    cmpq    %rbx,%rax
    jge     RetTrue
    jmp     RetFalse


INLINE_ROUTINE(fixed_leq)
    cmpq    %rbx,%rax
    jle     RetTrue
    jmp     RetFalse


INLINE_ROUTINE(fixed_gtr)
    cmpq    %rbx,%rax
    jg      RetTrue
    jmp     RetFalse


INLINE_ROUTINE(fixed_lss)
    cmpq    %rbx,%rax
    jl      RetTrue
    jmp     RetFalse


INLINE_ROUTINE(fixed_add)
    leaq   (-TAG)(%rax),%rax
    addq    %rbx,%rax
    jo      raiseOverflowEx
    ret


INLINE_ROUTINE(fixed_sub)
    subq   %rbx,%rax
    jo      raiseOverflowEx
    addq    $TAG,%rax      # Put back the tag
    ret


INLINE_ROUTINE(fixed_mul)
    sarq    $TAGSHIFT,%rbx # Untag the multiplier
    subq    $TAG,%rax      # Remove the tag from the multiplicand
    imulq   %rbx,%rax           # signed multiplication
    jo      raiseOverflowEx
    addq    $TAG,%rax      # Add back the tag, but don`t shift
    movq    %rax,%rdx           # clobber this which has the high-end result
    movq    %rax,%rbx           # and the other bad result.
    ret


INLINE_ROUTINE(fixed_quot)
# Checking for overflow and zero is done in ML
    sarq    $TAGSHIFT,%rbx
    sarq    $TAGSHIFT,%rax
    cqo
    idiv    %rbx
    MAKETAGGED(%rax,%rax)
    movq    %rax,%rdx
    movq    %rax,%rbx
    ret


INLINE_ROUTINE(fixed_rem)
    sarq    $TAGSHIFT,%rbx
    sarq    $TAGSHIFT,%rax
    cqo
    idiv     %rbx
    MAKETAGGED(%rdx,%rax)
    movq    %rax,%rdx
    movq    %rax,%rbx
    ret


INLINE_ROUTINE(fixed_div)
# Checking for overflow and zero is done in ML
    sarq    $TAGSHIFT,%rbx
    sarq    $TAGSHIFT,%rax
    cqo
    idiv    %rbx
    cmpq    $0,%rdx
    jz      fixed_div1          # If the remainder if non-zero ...
    xorq    %rdx,%rbx        # and has a different sign from the divisor ...
    jns     fixed_div1
    subq    $1,%rax       # subtract one to round to -infinity rather than zero.
fixed_div1:
    MAKETAGGED(%rax,%rax)
    movq    %rax,%rdx
    movq    %rax,%rbx
    ret


INLINE_ROUTINE(fixed_mod)
    sarq    $TAGSHIFT,%rbx
    sarq    $TAGSHIFT,%rax
    cqo
    idiv     %rbx
# Result is in %rdx.  We have to change the result so that it has the sign as the divisor.
    cmpq    $0,%rdx
    jz      fixed_mod1  # Result is zero - no change
    xorq    %rdx,%rbx
    jns     fixed_mod1  # Skip if they had the same signs
    xorq    %rdx,%rbx   # Restore the original divisor
    addq    %rbx,%rdx   # And add it in
fixed_mod1:
    MAKETAGGED(%rdx,%rax)
    movq    %rax,%rdx
    movq    %rax,%rbx
    ret


raiseOverflowEx:
# Build the exception packet.
# Allocate four word item.
# Set first word to TAGGED(5)
# Set second word to the string "Overflow" - That's going to need allocation.
# Set third and fourth words to TAGGED(0)
# Raise the exception.
    jmp     *Fr_RaiseOverflow(%rbp)

# Atomically increment the value at the address of the arg and return the
# updated value.  Since the xadd instruction returns the original value
# we have to increment it.
INLINE_ROUTINE(atomic_increment)
atomic_incr:                    # Internal name in case "atomic_increment" is munged.
    movq    $2,%rbx
    lock xaddq %rbx,(%rax)
    addq    $2,%rbx
    movq    %rbx,%rax
    ret



# Atomically decrement the value at the address of the arg and return the
# updated value.  Since the xadd instruction returns the original value
# we have to decrement it.
INLINE_ROUTINE(atomic_decrement)
atomic_decr:
    movq    $-2,%rbx
    lock xaddq %rbx,(%rax)
    movq    %rbx,%rax
    subq    $2,%rax
    ret



/* Reset a mutex to (tagged) one.  Because the increment and decrements
   are atomic this doesn't have to do anything special. */
INLINE_ROUTINE(atomic_reset)
    movq    $3,(%rax)
    movq    $UNIT,%rax  # The function returns unit
    ret



# Return the thread id object for the current thread
INLINE_ROUTINE(thread_self)
    movq    Fr_Argument(%rbp),%rax
    movq    Arg_ThreadId(%rax),%rax
    ret




/* Memory for LargeWord.word values.  This is the same as mem_for_real on
  64-bits but only a single word on 32-bits.
  ********************************
  Some of this code is temporary.  The final version should compute the result and
  simply jump here to box it.  That requires the heap-overflow code to save the
  registers across the trap but not to examine them for pointers.  Temporarily we
  don't do that but instead clear all the registers across a trap. */
mem_for_largeword:
/* 64-bits: just drop through */

# FLOATING POINT

mem_for_real:
# Allocate memory for the result.
    movq    %r15,%rcx
    subq    $16,%rcx        # Length word (8 bytes) + 8 bytes
    cmpq    Fr_LocalMbottom(%rbp),%rcx
    jnb     mem_for_real1
/* ********************************
Temporarily: push these registers to the stack
and pop them afterwards.  This isn't the final version of this code
but is useful as a test. */
    pushq   %rax
    pushq   %rbx
    call    callExHeap
    popq    %rbx
    popq    %rax
mem_for_real1:
    movq    %rcx,%r15                        # Updated allocation pointer
    movq    $1,(-8)(%rcx)      # One word
    movb    $B_bytes,(-1)(%rcx)    # Set the byte flag.
    ret


INLINE_ROUTINE(real_add)
    call    mem_for_real
/* Do the operation and put the result in the allocated space. */
    movsd   (%rax),%xmm0
    addsd   (%rbx),%xmm0
    movsd   %xmm0,(%rcx)
    movq    %rcx,%rax
    ret

INLINE_ROUTINE(real_sub)
    call    mem_for_real
    movsd   (%rax),%xmm0
    subsd   (%rbx),%xmm0
    movsd   %xmm0,(%rcx)
    movq    %rcx,%rax
    ret

INLINE_ROUTINE(real_mul)
    call    mem_for_real
    movsd   (%rax),%xmm0
    mulsd   (%rbx),%xmm0
    movsd   %xmm0,(%rcx)
    movq    %rcx,%rax
    ret

INLINE_ROUTINE(real_div)
    call    mem_for_real
    movsd   (%rax),%xmm0
    divsd   (%rbx),%xmm0
    movsd   %xmm0,(%rcx)
    movq    %rcx,%rax
    ret

INLINE_ROUTINE(real_abs)
    movq    %rax,%rbx                # Put a valid value in %rbx
    call    mem_for_real
# N.B. Real.~ X is not the same as 0.0 - X.  Real.~ 0.0 is ~0.0
    movsd   (%rax),%xmm0
    movsd   real_abs_mask(%rip),%xmm1
    andpd   %xmm1,%xmm0
    movsd   %xmm0,(%rcx)
    movq    %rcx,%rax
    ret

INLINE_ROUTINE(real_neg)
    movq    %rax,%rbx                # Put a valid value in %rbx
    call    mem_for_real
# Do the operation and put the result in the allocated
# space.
# N.B. Real.~ X is not the same as 0.0 - X.  Real.~ 0.0 is ~0.0;
    movsd   (%rax),%xmm0
    movsd   real_neg_sign(%rip),%xmm1
    xorpd   %xmm1,%xmm0
    movsd   %xmm0,(%rcx)
    movq    %rcx,%rax
    ret

    .align 8
real_abs_mask:  .quad  0x7fffffffffffffff
real_neg_sign:  .quad  0x8000000000000000


INLINE_ROUTINE(real_eq)
    movsd   (%rax),%xmm0
    ucomisd (%rbx),%xmm0
    jp      RetFalse                # If either was a NaN we return false
    jnz     RetFalse
    jmp     RetTrue


INLINE_ROUTINE(real_neq)
    movsd   (%rax),%xmm0
    ucomisd (%rbx),%xmm0
    jp      RetTrue                # If either was a NaN we return true
    jnz     RetTrue
    jmp     RetFalse

INLINE_ROUTINE(real_lss)
    movsd   (%rbx),%xmm0
    ucomisd (%rax),%xmm0            # Sets CF if less than or unordered, ZF if equal or unordered
    jbe     RetFalse                # Return false if <= or unordered
    jmp     RetTrue

INLINE_ROUTINE(real_gtr)
    movsd   (%rax),%xmm0
    ucomisd (%rbx),%xmm0
    jbe     RetFalse
    jmp     RetTrue

INLINE_ROUTINE(real_leq)
    movsd   (%rbx),%xmm0
    ucomisd (%rax),%xmm0
    jb      RetFalse                # Return false if < or unordered
    jmp     RetTrue

INLINE_ROUTINE(real_geq)
    movsd   (%rax),%xmm0
    ucomisd (%rbx),%xmm0
    jb      RetFalse
    jmp     RetTrue


INLINE_ROUTINE(real_from_int)
    testq   $TAG,%rax   # Is it long ?
    jnz      fixed_to_real_internal

real_float_1:
    CALL_IO(POLY_SYS_int_to_real)

INLINE_ROUTINE(fixed_to_real)
fixed_to_real_internal:
    movq    %rax,%rbx                # Put a valid value in %rbx
    call    mem_for_real
    sarq    $TAGSHIFT,%rax # Untag the value
    cvtsi2sd %rax,%xmm0
    movsd   %xmm0,(%rcx)
    movq    %rcx,%rax
    ret



INLINE_ROUTINE(set_exception_trace)
    movq    %rax,%rdx                   # Target closure
    movq    $UNIT,%rax             # The function takes a unit arg.
    jmp     *(%rdx)

# Additional assembly code routines

# RTS call to kill the current thread. 
INLINE_ROUTINE(X86AsmKillSelf)
    CALL_IO(POLY_SYS_kill_self)

INLINE_ROUTINE(X86AsmCallbackReturn)
    CALL_EXTRA(RETURN_CALLBACK_RETURN)

INLINE_ROUTINE(X86AsmCallbackException)
    CALL_EXTRA(RETURN_CALLBACK_EXCEPTION)

# This implements atomic addition in the same way as atomic_increment
INLINE_ROUTINE(X86AsmAtomicIncrement)
#ifdef _WIN32
    movq    %rcx,%rax       # On Windows the argument is passed in %rcx
#else
    movq    %rdi,%rax   # On X86_64 the argument is passed in %rdi
#endif
# Use %rcx and %rax because they are volatile (unlike %rbx on X86/64/Unix)
    movq    $2,%rcx
    lock xaddq %rcx,(%rax)
    addq    $2,%rcx
    movq    %rcx,%rax
    ret

# LargeWord.word operations.  These are 32 or 64-bit values in a single-word byte
# memory cell.
INLINE_ROUTINE(eq_longword)
    movq    (%rax),%rax
    cmpq    (%rbx),%rax
    jz      RetTrue         # True if they are equal.
    jmp     RetFalse


INLINE_ROUTINE(geq_longword)

    movq    (%rax),%rax
    cmpq    (%rbx),%rax
    jnb     RetTrue
    jmp     RetFalse


INLINE_ROUTINE(leq_longword)
    movq    (%rax),%rax
    cmpq    (%rbx),%rax
    jna     RetTrue
    jmp     RetFalse


INLINE_ROUTINE(gt_longword)
    movq    (%rax),%rax
    cmpq    (%rbx),%rax
    ja      RetTrue
    jmp     RetFalse


INLINE_ROUTINE(lt_longword)
    movq    (%rax),%rax
    cmpq    (%rbx),%rax
    jb      RetTrue
    jmp     RetFalse


INLINE_ROUTINE(longword_to_tagged)
# Load the value and tag it, discarding the top bit
    movq    (%rax),%rax
    MAKETAGGED(%rax,%rax)
    ret


INLINE_ROUTINE(signed_to_longword)
# Shift the value to remove the tag and store it.
    movq    %rax,%rbx                   # mem_for_largeword may push rebx
    call    mem_for_largeword
    sarq    $TAGSHIFT,%rax         # Arithmetic shift, preserve sign
    movq    %rax,(%rcx)
    movq    %rcx,%rax
    ret


INLINE_ROUTINE(unsigned_to_longword)
# Shift the value to remove the tag and store it.
    movq    %rax,%rbx                   # mem_for_largeword may push rebx
    call    mem_for_largeword
    shrq    $TAGSHIFT,%rax         # Logical shift, zero top bit
    movq    %rax,(%rcx)
    movq    %rcx,%rax
    ret


INLINE_ROUTINE(plus_longword)
    call    mem_for_largeword
    movq    (%rax),%rax
    addq    (%rbx),%rax
    movq    %rax,(%rcx)
    movq    %rcx,%rax
    ret


INLINE_ROUTINE(minus_longword)
    call    mem_for_largeword
    movq    (%rax),%rax
    subq    (%rbx),%rax
    movq    %rax,(%rcx)
    movq    %rcx,%rax
    ret


INLINE_ROUTINE(mul_longword)
    call    mem_for_largeword
    movq    (%rax),%rax
    mulq    (%rbx)
    movq    %rax,(%rcx)
    movq    %rcx,%rax
    movq    %rax,%rdx           # clobber this which has the high-end result
    ret


INLINE_ROUTINE(div_longword)
# Check for division by zero is done in ML
    call    mem_for_largeword
    movq    (%rax),%rax
    movq    $0,%rdx
    divq    (%rbx)
    movq    %rax,(%rcx)         # Store the quotient
    movq    %rcx,%rax
    movq    %rax,%rdx           # clobber this which has the remainder
    ret



INLINE_ROUTINE(mod_longword)
# Check for division by zero is done in ML
    call    mem_for_largeword
    movq    (%rax),%rax
    movq    $0,%rdx
    divq    (%rbx)
    movq    %rdx,(%rcx)         # Store the remainder
    movq    %rcx,%rax
    movq    %rax,%rdx           # clobber this which has the remainder
    ret



INLINE_ROUTINE(andb_longword)
    call    mem_for_largeword
    movq    (%rax),%rax
    andq    (%rbx),%rax
    movq    %rax,(%rcx)
    movq    %rcx,%rax
    ret



INLINE_ROUTINE(orb_longword)
    call    mem_for_largeword
    movq    (%rax),%rax
    orq     (%rbx),%rax
    movq    %rax,(%rcx)
    movq    %rcx,%rax
    ret


INLINE_ROUTINE(xorb_longword)
    call    mem_for_largeword
    movq    (%rax),%rax
    xorq    (%rbx),%rax
    movq    %rax,(%rcx)
    movq    %rcx,%rax
    ret



INLINE_ROUTINE(shift_left_longword)
    call    mem_for_largeword
    movq    %rcx,%rdx           # We need %rcx for the shift
 # The shift value is always a Word.word value i.e. tagged
 # LargeWord.<<(a,b) is defined to return 0 if b > LargeWord.wordSize
    cmpq    $TAGGED(64),%rbx
    jb      sllw1
    movq    $0,%rax
    jmp     sllw2
sllw1:
    movq    %rbx,%rcx
    shrq    $TAGSHIFT,%rcx # remove tag
    movq    (%rax),%rax
    shlq    %cl,%rax
sllw2:
    movq    %rax,(%rdx)
    movq    %rdx,%rax
    movq    %rax,%rcx           # Clobber %rcx
    ret


INLINE_ROUTINE(shift_right_longword)
    call    mem_for_largeword
    movq    %rcx,%rdx           # We need %rcx for the shift
/* The shift value is always a Word.word value i.e. tagged
   LargeWord.>>(a,b) is defined to return 0 if b > LargeWord.wordSize */
    cmpq    $TAGGED(64),%rbx
    jb      srlw1
    movq    $0,%rax
    jmp     srlw2
srlw1:
    movq    %rbx,%rcx
    shrq    $TAGSHIFT,%rcx # remove tag
    movq    (%rax),%rax
    shrq    %cl,%rax
srlw2:
    movq    %rax,(%rdx)
    movq    %rdx,%rax
    movq    %rax,%rcx           # Clobber %rcx
    ret


INLINE_ROUTINE(shift_right_arith_longword)
    call    mem_for_largeword
    movq    %rcx,%rdx           # We need %rcx for the shift
/* The shift value is always a Word.word value i.e. tagged
   LargeWord.~>>(a,b) is defined to return 0 or ~1 if b > LargeWord.wordSize */
    cmpq    $TAGGED(64),%rbx
    jb      sralw1
    # Setting the shift to 31/63 propagates the sign bit
    movq    $TAGGED(63),%rbx
sralw1:
    movq    %rbx,%rcx
    shrq    $TAGSHIFT,%rcx # remove tag
    movq    (%rax),%rax
    sarq    %cl,%rax
    movq    %rax,(%rdx)
    movq    %rdx,%rax
    movq    %rax,%rcx           # Clobber %rcx
    ret


# C-memory operations.
INLINE_ROUTINE(cmem_load_asm_8)
    movq    (%rax),%rax             # The address is boxed.
    sarq    $TAGSHIFT,%rbx     # The offset is a signed tagged value
    addq    %rbx,%rax               # Add it in
    movq    %r8,%rbx                 # Get the index.
    sarq    $TAGSHIFT,%rbx     # That's also tagged
    movzbq  (%rax,%rbx,1),%rax
    MAKETAGGED(%rax,%rax)
    movq    %rax,%rbx       # Clobber bad value in %rbx
    ret


INLINE_ROUTINE(cmem_load_asm_16)
    movq    (%rax),%rax             # The address is boxed.
    sarq    $TAGSHIFT,%rbx     # The offset is a signed tagged value
    addq    %rbx,%rax               # Add it in
    movq    %r8,%rbx                 # Get the index.
    # The index is tagged but since we want to multiply by two we don't need anything here.
    movzwq  -1(%rax,%rbx,1),%rax
    MAKETAGGED(%rax,%rax)
    ret


INLINE_ROUTINE(cmem_load_asm_32)
# 64-bit mode - the result is tagged
    movq    (%rax),%rax             # The address is boxed.
    sarq    $TAGSHIFT,%rbx     # The offset is a signed tagged value
    addq    %rbx,%rax               # Add it in
    movl    -2(%rax,%r8,2),%eax
    MAKETAGGED(%rax,%rax)
    movq    %rax,%rbx       # Clobber bad value in %rbx
    ret



INLINE_ROUTINE(cmem_load_asm_64)
    pushq   %r8                     # Save %r8 which isn't saved in mem_for_largeword
    call    mem_for_largeword
    popq    %r8
    movq    (%rax),%rax             # The address is boxed.
    sarq    $TAGSHIFT,%rbx     # The offset is a signed tagged value
    addq    %rbx,%rax               # Add it in
    movq    (-4)(%rax,%r8,4),%rax
    movq    %rax,(%rcx)             # Save in the new memory
    movq    %rcx,%rax               # Copy the result address
    movq    %rax,%rbx               # Clobber bad value
    ret



INLINE_ROUTINE(cmem_load_asm_float)
    pushq   %r8                     # Save %r8 which isn't saved in mem_for_largeword
    call    mem_for_real
    popq    %r8                     # Save %r8 which isn't saved in mem_for_largeword
    movq    (%rax),%rax             # The address is boxed.
    sarq    $TAGSHIFT,%rbx     # The offset is a signed tagged value
    addq    %rbx,%rax               # Add it in
    movq    %r8,%rbx                 # Get the index.
    FLDS    -2(%rax,%rbx,2)
    FSTPL   (%rcx)
    movq    %rcx,%rax
    ret



INLINE_ROUTINE(cmem_load_asm_double)
    pushq   %r8                     # Save %r8 which isn't saved in mem_for_largeword
    call    mem_for_real
    popq    %r8                     # Save %r8 which isn't saved in mem_for_largeword
    movq    (%rax),%rax             # The address is boxed.
    sarq    $TAGSHIFT,%rbx     # The offset is a signed tagged value
    addq    %rbx,%rax               # Add it in
    movq    %r8,%rbx                 # Get the index.
    FLDL    -4(%rax,%rbx,4)
    FSTPL   (%rcx)
    movq    %rcx,%rax
    ret


   
INLINE_ROUTINE(cmem_store_asm_8)
    movq    (%rax),%rax             # The address is boxed.
    sarq    $TAGSHIFT,%rbx     # The offset is a signed tagged value
    addq    %rbx,%rax               # Add it in
    movq    %r8,%rbx                 # Get the index.
    movq    %r9,%rcx
    sarq    $TAGSHIFT,%rbx     # That's also tagged
    sarq    $TAGSHIFT,%rcx
    movb    %cl,(%rax,%rbx)
    movq    $UNIT,%rax             # The function returns unit
    movq    %rax,%rbx                   # Clobber bad value in %rbx
    movq    %rax,%rcx                   # and %rcx
    ret


INLINE_ROUTINE(cmem_store_asm_16)
    movq    (%rax),%rax             # The address is boxed.
    sarq    $TAGSHIFT,%rbx     # The offset is a signed tagged value
    addq    %rbx,%rax               # Add it in
    movq    %r8,%rbx                 # Get the index.
    movq    %r9,%rcx
    sarq    $TAGSHIFT,%rcx     # Untag the value to store
    movw    %cx,-1(%rax,%rbx,1)
    movq    $UNIT,%rax             # The function returns unit
    movq    %rax,%rcx                   # Bad value in %rcx
    ret


INLINE_ROUTINE(cmem_store_asm_32)
    movq    (%rax),%rax             # The address is boxed.
    sarq    $TAGSHIFT,%rbx     # The offset is a signed tagged value
    addq    %rbx,%rax               # Add it in
    movq    %r8,%rbx                 # Get the index.
    movq    %r9,%rcx
    sarq    $TAGSHIFT,%rcx     # Untag the value to store
    movl    %ecx,-2(%rax,%rbx,2)
    movq    $UNIT,%rax             # The function returns unit
    movq    %rax,%rcx                   # Bad value in %rcx
    ret


INLINE_ROUTINE(cmem_store_asm_64)
    movq    (%rax),%rax             # The address is boxed.
    sarq    $TAGSHIFT,%rbx     # The offset is a signed tagged value
    addq    %rbx,%rax               # Add it in
    movq    (%r9),%rbx               # Value to store
    movq    %rbx,(-4)(%rax,%r8,4)    # Store it
    movq    $UNIT,%rax         # The function returns unit
    movq    %rax,%rbx               # Bad value in %rbx
    ret



INLINE_ROUTINE(cmem_store_asm_float)
    movq    (%rax),%rax             # The address is boxed.
    sarq    $TAGSHIFT,%rbx     # The offset is a signed tagged value
    addq    %rbx,%rax               # Add it in
    movq    %r8,%rbx                 # Get the index.
    movq    %r9,%rcx
    FLDL    (%rcx)
    FSTPS    -2(%rax,%rbx,2)
    movq    $UNIT,%rax         # The function returns unit
    movq    %rax,%rbx               # Bad value in %rbx
    ret


INLINE_ROUTINE(cmem_store_asm_double)
    movq    (%rax),%rax             # The address is boxed.
    sarq    $TAGSHIFT,%rbx     # The offset is a signed tagged value
    addq    %rbx,%rax               # Add it in
    movq    %r8,%rbx                 # Get the index.
    movq    %r9,%rcx
    FLDL    (%rcx)
    FSTPL    -4(%rax,%rbx,4)
    movq    $UNIT,%rax         # The function returns unit
    movq    %rax,%rbx               # Bad value in %rbx
    ret

INLINE_ROUTINE(get_flags)
    movzbq  -1(%rax),%rax
    MAKETAGGED(%rax,%rax)
    ret

#define CREATE_IO_CALL(index) \
    INLINE_ROUTINE(Call##index##) \
    CALL_IO(index)

    CREATE_IO_CALL(POLY_SYS_exit)
    CREATE_IO_CALL(POLY_SYS_get_entry_point)
    CREATE_IO_CALL(POLY_SYS_profiler)
    CREATE_IO_CALL(POLY_SYS_Real_str)
    CREATE_IO_CALL(POLY_SYS_Real_Dispatch)
    CREATE_IO_CALL(POLY_SYS_conv_real)
    CREATE_IO_CALL(POLY_SYS_real_to_int)
    CREATE_IO_CALL(POLY_SYS_sqrt_real)
    CREATE_IO_CALL(POLY_SYS_sin_real)
    CREATE_IO_CALL(POLY_SYS_signal_handler)
    CREATE_IO_CALL(POLY_SYS_os_specific)
    CREATE_IO_CALL(POLY_SYS_network)
    CREATE_IO_CALL(POLY_SYS_io_dispatch)
    CREATE_IO_CALL(POLY_SYS_poly_specific)
    CREATE_IO_CALL(POLY_SYS_set_code_constant)
    CREATE_IO_CALL(POLY_SYS_process_env)
    CREATE_IO_CALL(POLY_SYS_foreign_dispatch)
    CREATE_IO_CALL(POLY_SYS_ffi)
    CREATE_IO_CALL(POLY_SYS_full_gc)
    CREATE_IO_CALL(POLY_SYS_XWindows)
    CREATE_IO_CALL(POLY_SYS_timing_dispatch)
    CREATE_IO_CALL(POLY_SYS_kill_self)
    CREATE_IO_CALL(POLY_SYS_thread_dispatch)
    CREATE_IO_CALL(POLY_SYS_io_operation)
    CREATE_IO_CALL(POLY_SYS_ln_real)
    CREATE_IO_CALL(POLY_SYS_exp_real)
    CREATE_IO_CALL(POLY_SYS_arctan_real)
    CREATE_IO_CALL(POLY_SYS_cos_real)
