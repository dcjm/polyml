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
   This is the 32-bit Unix version of the assembly code file.
   There are separate versions of 32/64 and Windows (Intel syntax)
   and Unix (gas syntax).
*/

/*
 Registers used :-

  %%eax: First argument to function.  Result of function call.
  %%ebx: Second argument to function.
  %%ecx: General register
  %%edx: Closure pointer in call.
  %%ebp: Points to memory used for extra registers
  %%esi: General register.
  %%edi: General register.
  %%esp: Stack pointer.
*/


#include "config.h"
#ifdef SYMBOLS_REQUIRE_UNDERSCORE
#define EXTNAME(x)  _##x
#else
#define EXTNAME(x)  x
#endif

#
# Macro to begin the hand-coded functions
#

#ifdef MACOSX
#define GLOBAL .globl
#else
#define GLOBAL .global
#endif

#define INLINE_ROUTINE(id) \
GLOBAL EXTNAME(id); \
EXTNAME(id):

#define TAG         1
#define TAGSHIFT    1
#define TAGMULT     (1 << TAGSHIFT)

#define TAGGED(i) ((i << TAGSHIFT) | TAG)
#define MAKETAGGED(from,to)     leal    TAG(,from,2),to

#define POLYWORDSIZE    4

#define NIL         TAGGED(0)
#define UNIT        TAGGED(0)
#define ZERO        TAGGED(0)
#define FALSE       TAGGED(0)
#define TRUE        TAGGED(1)
#define MINUS1      TAGGED(0-1)

#define B_bytes     0x01
#define B_typeBits  0x03
#define B_mutable   0x40

#define Max_Length  0x00ffffff


/* The "stack frame" pointed to by ebp acts as the "memory registers". */
#define Fr_RaiseOverflow            -48
#define Fr_HeapOverflow             -40
#define Fr_StackOverflow            -36
#define Fr_StackOverflowEx          -32
#define Fr_LocalMpointer            -28
#define Fr_HandlerRegister          -24
#define Fr_LocalMbottom             -20
#define Fr_StackLimit               -16
#define Fr_SavedEsi                 -12
#define Fr_SavedEdi                 -8
#define Fr_SavedEbx                 -4
#define Fr_SavedEBP                 0
#define Fr_Argument                 8
/* Extra entries on the C stack */
#define Fr_SavedMLSP                -52

#define Fr_Size                     56

/* This is the argument vector passed in to X86AsmSwitchToPoly
   It is used to initialise the frame.  A few values are updated
   when ML returns. */
#define Arg_LocalMpointer       0x0
#define Arg_HandlerRegister     0x4
#define Arg_LocalMbottom        0x8
#define Arg_StackLimit          0xc
#define Arg_ExceptionPacket     0x10  /* Address of packet to raise */
#define Arg_RequestCode         0x14 /* Byte: Io function to call. */
#define Arg_ReturnReason        0x16  /* Byte: Reason for returning from ML. */
#define Arg_FullRestore         0x17  /* Byte: Full/partial restore */
#define Arg_PolyStack           0x18  /* Current stack base */
#define Arg_ThreadId            0x1c  /* My thread id */
#define Arg_StackPtr            0x20  /* Stack Pointer */
#define Arg_ProgramCtr          0x24
#define Arg_SaveRAX             0x28
#define Arg_SaveRBX             0x2c
#define Arg_SaveRCX             0x30
#define Arg_SaveRDX             0x34
#define Arg_SaveRSI             0x38
#define Arg_SaveRDI             0x3c
#define Arg_SaveFP              0x40

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
        movl  Fr_Argument(%ebp),%ecx; \
        movb  $index,Arg_RequestCode(%ecx); \
        jmp   SaveStateAndReturnLocal;

#define CALL_EXTRA(index) \
        pushl %ecx; \
        movl  Fr_Argument(%ebp),%ecx; \
        movb  $index,Arg_ReturnReason(%ecx); \
        popl  %ecx; \
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
    pushl   %ebp                            # Standard entry sequence
    movl    %esp,%ebp
    pushl   %ebx
    pushl   %edi
    pushl   %esi                            # Push callee-save registers
    subl    $(Fr_Size-12),%esp              # Allocate frame
    movl    Fr_Argument(%ebp),%ecx          # Address of argument vector
    movl    Arg_StackLimit(%ecx),%eax
    movl    %eax,Fr_StackLimit(%ebp)
    movl    Arg_LocalMbottom(%ecx),%eax
    movl    %eax,Fr_LocalMbottom(%ebp)
    movl    Arg_HandlerRegister(%ecx),%eax
    movl    %eax,Fr_HandlerRegister(%ebp)
    movl    Arg_LocalMpointer(%ecx),%eax
    movl    %eax,Fr_LocalMpointer(%ebp)
    movl    $callExStackEx,%eax
    movl    %eax,Fr_StackOverflowEx(%ebp)
    movl    $callExStack,%eax
    movl    %eax,Fr_StackOverflow(%ebp)
    movl    $callExHeap,%eax
    movl    %eax,Fr_HeapOverflow(%ebp)
    movl    $callExRaiseOverflow,%eax
    movl    %eax,Fr_RaiseOverflow(%ebp)
    movl    Arg_StackPtr(%ecx),%esp               # Set the new stack ptr
    movl    Arg_ExceptionPacket(%ecx),%eax
    cmpl    $1,%eax                             # Did we raise an exception?
    jnz     raisex
    testb   $1,Arg_FullRestore(%ecx)           # Should we restore or clear the regs?
    jnz     sw2polyfull
/* We're returning from an RTS call. */
    movl    Arg_SaveRAX(%ecx),%eax              # The return value
    cld                                     # Clear this just in case
    jmp     *Arg_ProgramCtr(%ecx)            # Jump to code address

sw2polyfull:
    FRSTOR  Arg_SaveFP(%ecx)
    movl    Arg_SaveRAX(%ecx),%eax              # Load the registers
    movl    Arg_SaveRBX(%ecx),%ebx              # Load the registers
    movl    Arg_SaveRDX(%ecx),%edx
    movl    Arg_SaveRSI(%ecx),%esi
    movl    Arg_SaveRDI(%ecx),%edi
    cld                                     # Clear this just in case
    push    Arg_ProgramCtr(%ecx)
    movl    Arg_SaveRCX(%ecx),%ecx
    ret                                     # Jump to code address


/* Code to save the state and switch to C
   This saves the full register state. */
SaveFullState:
    pushl   %eax                            # Save eax
    movl    Fr_Argument(%ebp),%eax
    movl    %ebx,Arg_SaveRBX(%eax)
    movl    %ecx,Arg_SaveRCX(%eax)
    movl    %edx,Arg_SaveRDX(%eax)
    movl    %esi,Arg_SaveRSI(%eax)
    movl    %edi,Arg_SaveRDI(%eax)
    fnsave  Arg_SaveFP(%eax)                # Save FP state.  Also resets the state so...
    fldcw   Arg_SaveFP(%eax)                # ...load because we need the same rounding mode in the RTS
    popl    %ebx                            # Get old eax value
    movl    %ebx,Arg_SaveRAX(%eax)
    movl    %esp,Arg_StackPtr(%eax)               # Save ML stack pointer
    movl    Fr_Argument(%ebp),%eax
    movl    Fr_LocalMpointer(%ebp),%ebx     # Copy back heap pointer
    movl    %ebx,Arg_LocalMpointer(%eax)
    movl    Fr_HandlerRegister(%ebp),%ebx   # and handler pointer
    movl    %ebx,Arg_HandlerRegister(%eax)
    movl    %ebp,%esp                       # Restore C stack pointer
    subl    $12,%esp                        # Sp is just before the registers
    popl    %esi
    popl    %edi
    popl    %ebx
    popl    %ebp
    ret

callExHeap:
callheapOverflowLocal:
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
    
#if (0)
    pushl   %eax                # Save eax
    movl    PolyStack(%ebp),%eax
    movl    %ebx,EBX_OFF(%eax)
    movl    %edx,EDX_OFF(%eax)
    fstcw   FPREGS_OFF(%eax)
    fninit                     # Reset the FP state.
    fldcw   FPREGS_OFF(%eax)   # But reload the rounding mode
    popl    %ebx                # Get old eax value
    movl    %ebx,EAX_OFF(%eax)
    movl    %esp,SP_OFF(%eax)
    movb    $1,InRTS(%ebp)             # inRTS:=0 (stack now kosher)
    movl    SavedSp(%ebp),%esp
    popal
    ret
#endif

/* Used when entering new code.  The argument and closure are on the stack
   in case there is a GC before we enter the code. */
INLINE_ROUTINE(X86AsmPopArgAndClosure)
    popl    %edx
    popl    %eax
    jmp     *(%edx)

/* CallcodeTupled.  This is currently only used to "fold" RTS functions that are applied
   to constant arguments.  Oddly, compared with other RTS functions, it takes a single
   argument that is a pair containing the function closure and an argument vector. */
INLINE_ROUTINE(callcodeTupled)
    movl    (%eax),%edx                         # closure
    movl    POLYWORDSIZE(%eax),%esi             # address of arg vector
    cmpl    $NIL,%esi                           # If calling a function without args this could be nil
    je      cct2
    movl    $Max_Length,%ecx                    # Mask for length removing any flags
    andl    (-POLYWORDSIZE)(%esi),%ecx          # Load and mask length
    jz      cct2
    movl    (%esi),%eax                         # First argument
    addl    $POLYWORDSIZE,%esi
    subl    $1,%ecx
    jz      cct2
    movl    (%esi),%ebx                         # Second argument
    addl    $POLYWORDSIZE,%esi
    subl    $1,%ecx
    jz      cct2
    popl    %edi                                # Get the return address
cct1:                                           # Push the remaining args to the stack
    pushl   (%esi)
    addl    $POLYWORDSIZE,%esi
    LOOP    cct1
    pushl   %edi                                # Push the return address
cct2:                                           # Finished - enter function
    movl    $UNIT,%esi                          # Clobber, for the moment
    movl    $UNIT,%ecx
    jmp     *(%edx)



/*
A number of functions implemented in Assembly for efficiency reasons
*/

INLINE_ROUTINE(int_to_word)
 # Extract the low order bits from a word.
    testl   $TAG,%eax
    jz      get_first_long_word_a1
    ret                 # Return the argument

 # This is now used in conjunction with isShort in Word.fromInt.
INLINE_ROUTINE(get_first_long_word_a)
get_first_long_word_a1:
    testb   $16,(-1)(%eax)     # 16 is the "negative" bit
    movl    (%eax),%eax     # Extract the word which is already little-endian
    jz      gfw1
    negl    %eax            # We can ignore overflow
gfw1:
    MAKETAGGED(%eax,%eax)
    ret



INLINE_ROUTINE(move_bytes)
/* Move a segment of memory from one location to another.
   Must deal with the case of overlapping segments correctly.
   (source, sourc_offset, destination, dest_offset, length) */

 # Assume that the offsets and length are all short integers.
    movl    12(%esp),%edi               # Destination address
    movl    8(%esp),%ecx                # Destination offset, untagged
    shrl    $TAGSHIFT,%ecx
    addl    %ecx,%edi
    movl    %eax,%esi                   # Source address
    shrl    $TAGSHIFT,%ebx
    addl    %ebx,%esi
    movl    4(%esp),%ecx                # Get the length to move
    shrl    $TAGSHIFT,%ecx
    cld                             # Default to increment %edi,%esi
    cmpl    %edi,%esi                   # Check for potential overlap
 # If dest > src then use decrementing moves else
 # use incrementing moves.
    ja      mvb1
    std                             # Decrement %edi,%esi
    leal    (-1)(%esi,%ecx),%esi
    leal    (-1)(%edi,%ecx),%edi
mvb1:
    rep; movsb                      # Copy the bytes
    movl    $UNIT,%eax              # The function returns unit
    movl    %eax,%ebx               # Clobber bad value in %rbx
    movl    %eax,%ecx               # and %%ecx
    movl    %eax,%edi
    movl    %eax,%esi
 # Visual Studio 5 C++ seems to assume that the direction flag
 # is cleared.  I think that`s a bug but we have to go along with it.
    cld
    ret     $12



INLINE_ROUTINE(move_words)
/* Move a segment of memory from one location to another.
   Must deal with the case of overlapping segments correctly.
   (source, source_offset, destination, dest_offset, length)
   Assume that the offsets and length are all short integers. */
    movl    12(%esp),%edi               # Destination address
    movl    8(%esp),%ecx                # Destination offset
    leal    (-2)(%edi,%ecx,2),%edi      # Destination address plus offset
    leal    (-2)(%eax,%ebx,2),%esi      # Source address plus offset
    movl    4(%esp),%ecx                # Get the length to move (words)
    shrl    $TAGSHIFT,%ecx
    cld                             # Default to increment %edi,%esi
    cmpl    %edi,%esi                   # Check for potential overlap
 # If dest > src then use decrementing moves else
 # use incrementing moves.
    ja      mvw1
    std                             # Decrement %edi,%esi
    leal    (-POLYWORDSIZE)(%esi,%ecx,POLYWORDSIZE),%esi
    leal    (-POLYWORDSIZE)(%edi,%ecx,POLYWORDSIZE),%edi
mvw1:
    rep; movsl                           # Copy the words
    movl    $UNIT,%eax             # The function returns unit
    movl    %eax,%ecx               # Clobber bad values
    movl    %eax,%edi
    movl    %eax,%esi
 # Visual Studio 5 C++ seems to assume that the direction flag
 # is cleared.  I think that`s a bug but we have to go along with it.
    cld
    ret     $12

#

RetFalse:
    movl    $FALSE,%eax
    ret

RetTrue:
    movl    $TRUE,%eax
    ret

INLINE_ROUTINE(not_bool)
    xorl    $(TRUE-TAG),%eax   # Change the value but leave the tag
    ret


 # or, and, xor shift etc. assume the values are tagged integers
INLINE_ROUTINE(or_word)
    orl     %ebx,%eax
    ret


INLINE_ROUTINE(and_word)
    andl    %ebx,%eax
    ret


INLINE_ROUTINE(xor_word)
    xorl    %ebx,%eax
    orl     $TAG,%eax  # restore the tag
    ret


INLINE_ROUTINE(shift_left_word)
 # Assume that both args are tagged integers
 # Word.<<(a,b) is defined to return 0 if b > Word.wordSize
    cmpl    $TAGGED(31),%ebx
    jb      slw1
    movl    $ZERO,%eax
    ret
slw1:
    movl    %ebx,%ecx
    shrl    $TAGSHIFT,%ecx # remove tag
    subl    $TAG,%eax
    shll    %cl,%eax
    orl     $TAG,%eax  # restore the tag
    movl    %eax,%ecx       # clobber %%ecx
    ret


INLINE_ROUTINE(shift_right_word)
 # Word.>>(a,b) is defined to return 0 if b > Word.wordSize
    cmpl    $TAGGED(31),%ebx
    jb      srw1
    movl    $ZERO,%eax
    ret
srw1:
    movl    %ebx,%ecx
    shrl    $TAGSHIFT,%ecx # remove tag
    shrl    %cl,%eax
    orl     $TAG,%eax  # restore the tag
    movl    %eax,%ecx       # clobber %%ecx
    ret


INLINE_ROUTINE(shift_right_arith_word)
 # Word.~>>(a,b) is defined to return 0 or ~1 if b > Word.wordSize
 # The easiest way to do that is to set the shift to 31.
    cmpl    $TAGGED(31),%ebx
    jb      sra1
    movl    $TAGGED(31),%ebx
sra1:
    movl    %ebx,%ecx
    shrl    $TAGSHIFT,%ecx # remove tag
    sarl    %cl,%eax
    orl     $TAG,%eax  # restore the tag
    movl    %eax,%ecx       # clobber %%ecx
    ret


# Clears the "mutable" bit on a segment
INLINE_ROUTINE(locksega)
    andb    $(0xff-B_mutable),-1(%eax)
    movl     $TAGGED(0),%eax   # Return Unit,
    ret


INLINE_ROUTINE(get_length_a)
    movl    (-POLYWORDSIZE)(%eax),%eax
    shll    $8,%eax            # Clear top byte
    shrl    $(8-TAGSHIFT),%eax  # Make it a tagged integer
    orl $TAG,%eax
    ret



INLINE_ROUTINE(is_shorta)
# Returns true if the argument is tagged
    andl    $TAG,%eax
    jz      RetFalse
    jmp     RetTrue


INLINE_ROUTINE(string_length)
    testl   $TAG,%eax  # Single char strings are represented by the
    jnz     RetOne      # character.
    movl    (%eax),%eax # Get length field
    MAKETAGGED(%eax,%eax)
    ret
RetOne: movl    $TAGGED(1),%eax
    ret


 # Store the length of a string in the first word.
INLINE_ROUTINE(set_string_length_a)
    shrl    $TAGSHIFT,%ebx # Untag the length
    movl    %ebx,(%eax)
    movl    $UNIT,%eax     # Return unit
    movl    %eax,%ebx           # Clobber untagged value
    ret


# raisex (formerly raisexn) is used by compiled code.
INLINE_ROUTINE(raisex)
raisexlocal:
    movl    Fr_HandlerRegister(%ebp),%ecx    # Get next handler into %rcx
    jmp     *(%ecx)

INLINE_ROUTINE(load_byte)
    movl    %ebx,%edi
    shrl    $TAGSHIFT,%edi
    movzbl  (%eax,%edi,1),%edi
    MAKETAGGED(%edi,%eax)
    movl    %eax,%edi       # Clobber bad value in %%edi
    ret


INLINE_ROUTINE(load_word)
    movl    (-2)(%eax,%ebx,2),%eax
    movl    %eax,%ebx
    ret


INLINE_ROUTINE(assign_byte)
# We can assume that the data value will not overflow 30 bits (it is only 1 byte!)
    movl    4(%esp),%ecx
    shrl    $TAGSHIFT,%ecx       # Remove tags from data value

# We can assume that the index will not overflow 30 bits i.e. it is a tagged short
    shrl    $TAGSHIFT,%ebx     # Remove tags from offset
    movb    %cl,(%eax,%ebx)

    movl    $UNIT,%eax             # The function returns unit
    movl    %eax,%ebx                   # Clobber bad value in %%ebx
    movl    %eax,%ecx                   # and %%ecx
    ret     $4



INLINE_ROUTINE(assign_word)
    movl    4(%esp),%ecx
    movl    %ecx,(-2)(%eax,%ebx,2)
    movl    $UNIT,%eax           # The function returns unit
    ret     $4


/* Allocate a piece of memory that does not need to be initialised.
   We can't actually risk leaving word objects uninitialised so for the
   moment we always initialise. */
INLINE_ROUTINE(alloc_uninit)
    popl    %ecx                # Get the return address
    pushl   $ZERO               # Push the initial value - zero
    pushl   %ecx                # Restore the return address
# Drop through into alloc_store




/* alloc(size, flags, initial).  Allocates a segment of a given size and
  initialises it.
 
  This is primarily used for arrays and for strings.  Refs are
  allocated using inline code. */
INLINE_ROUTINE(alloc_store)
allsts:
 # First check that the length is acceptable
    testl   $TAG,%eax
    jz      alloc_in_rts                # Get the RTS to raise an exception
    movl    %eax,%edi
    shrl    $TAGSHIFT,%edi              # Remove tag
    cmpl    $Max_Length,%edi            # Length field must fit in 24 bits
    ja      alloc_in_rts                # Get the RTS to raise an exception
    incl    %edi                        # Add 1 word
    shll    $2,%edi                     # Get length in bytes
    movl    Fr_LocalMpointer(%ebp),%edx
    subl    %edi,%edx                   # Allocate the space
    movl    %eax,%edi                   # Clobber bad value in %edi
    cmpl    Fr_LocalMbottom(%ebp),%edx     # Check for free space
    jb      alloc_in_rts
/* Normally the above test is sufficient but if LocalMpointer is near the bottom of
   memory and the store requested is very large the value in %edx can be negative
   which is greater, unsigned, than LocalMbottom.  We have to check it is less
   than, unsigned, the allocation pointer. */
    cmpl    Fr_LocalMpointer(%ebp),%edx
    jnb     alloc_in_rts
    movl    %edx,Fr_LocalMpointer(%ebp)             # Put back in the heap ptr
    shrl    $TAGSHIFT,%eax
    movl    %eax,(-4)(%edx)         # Put in length
    shrl    $TAGSHIFT,%ebx     # remove tag from flag
    orl     $B_mutable,%ebx    # set mutable bit
    movb    %bl,(-1)(%edx)         # and put it in.
 # Initialise the store.
    movl    %eax,%ecx               # Get back the no. of words.
    movl    4(%esp),%eax            # Get initial value.
    andb    $B_typeBits,%bl         # Mask off mutable, weak etc.
    cmpb    $B_bytes,%bl
    jne     allst2

 # If this is a byte seg
    shrl    $TAGSHIFT,%eax # untag the initialiser
    shll    $2,%ecx        # Convert to bytes
    movl    %edx,%edi
    rep; stosb
    jmp     allst3

 # If this is a word segment
allst2:
    movl    %edx,%edi
    rep; stosl

allst3:
    movl    %edx,%eax

    movl    %eax,%ecx       # Clobber these
    movl    %eax,%edx
    movl    %eax,%ebx
    movl    %eax,%edi
    ret     $4



# This is used if we have reached the store limit and need to garbage-collect.
alloc_in_rts:
    movl    %eax,%edx       # Clobber these first
    movl    %eax,%edi
    CALL_IO(POLY_SYS_alloc_store)

INLINE_ROUTINE(touch_final)
# This is really a pseudo-op
    movl    $UNIT,%eax
    ret


INLINE_ROUTINE(add_long)
    movl    %eax,%edi
    andl    %ebx,%edi
    andl    $TAG,%edi
    jz      add_really_long
    leal    (-TAG)(%eax),%edi
    addl    %ebx,%edi
    jo      add_really_long
    movl    %edi,%eax
    ret
add_really_long:
    movl    %eax,%edi
    CALL_IO(POLY_SYS_aplus)


INLINE_ROUTINE(sub_long)
    movl    %eax,%edi
    andl    %ebx,%edi
    andl    $TAG,%edi
    jz      sub_really_long
    movl    %eax,%edi
    subl    %ebx,%edi
    jo      sub_really_long
    leal    TAG(%edi),%eax      # Put back the tag
    movl    %eax,%edi
    ret
sub_really_long:
    movl    %eax,%edi
    CALL_IO(POLY_SYS_aminus)


INLINE_ROUTINE(mult_long)
    movl    %eax,%edi
    andl    %ebx,%edi
    andl    $TAG,%edi
    jz      mul_really_long
    movl    %ebx,%edi
    sarl    $TAGSHIFT,%edi # Shift multiplicand
    movl    %eax,%esi
    subl    $TAG,%esi          # Just subtract off the tag off multiplier
    imull   %edi,%esi
    jo      mul_really_long
    addl    $TAG,%esi
    movl    %esi,%eax
    movl    %eax,%edi
    ret
mul_really_long:
    movl    %eax,%esi       # Clobber this
    movl    %eax,%edi
    CALL_IO(POLY_SYS_amul)


INLINE_ROUTINE(div_long)
    movl    %eax,%edi
    andl    %ebx,%edi
    andl    $TAG,%edi          # %%edi now contains $0 or $1 (both legal!)
    jz      div_really_long
    cmpl    $TAGGED(0),%ebx    # Check that it's non-zero
    jz      div_really_long         # We don't want a trap.
 # The only case of overflow is dividing the smallest negative number by -1
    cmpl    $TAGGED((-1)),%ebx
    jz      div_really_long
    sarl    $TAGSHIFT,%eax
    movl    %ebx,%edi
    sarl    $TAGSHIFT,%edi
    cdq
    idiv    %edi
    MAKETAGGED(%eax,%eax)
    movl    %eax,%edx
    movl    %eax,%edi
    ret
div_really_long:
    movl    %eax,%edi
    CALL_IO(POLY_SYS_adiv)


INLINE_ROUTINE(rem_long)
    movl    %eax,%edi
    andl    %ebx,%edi
    andl    $TAG,%edi      # %%edi now contains $0 or $1 (both legal!
    jz      rem_really_long
    cmpl    $TAGGED(0),%ebx    # Check that it's non-zero
    jz      rem_really_long         # We don't want a trap.
 # The only case of overflow is dividing the smallest negative number by -1
    cmpl    $TAGGED((-1)),%ebx
    jz      rem_really_long
    sarl    $TAGSHIFT,%eax
    movl    %ebx,%edi
    sarl    $TAGSHIFT,%edi
    cdq
    idiv    %edi
    MAKETAGGED(%edx,%eax)
    movl    %eax,%edx
    movl    %eax,%edi
    ret
rem_really_long:
    movl    %eax,%edi
    CALL_IO(POLY_SYS_amod)


/* Combined quotient and remainder.  We have to use the long form
   if the arguments are long or there's an overflow.  The first two
   arguments are the values to be divided.  The third argument is the
   address where the results should be placed. */
INLINE_ROUTINE(quotrem_long)
    movl    %eax,%edi
    andl    %ebx,%edi
    andl    $TAG,%edi
    jz      quotrem_really_long
    cmpl    $TAGGED(0),%ebx
    jz      quotrem_really_long
 # The only case of overflow is dividing the smallest negative number by -1
    cmpl    $TAGGED((-1)),%ebx
    jz      quotrem_really_long

 # Get the address for the result.
    movl    4(%esp),%ecx
# Do the division
    sarl    $TAGSHIFT,%eax
    movl    %ebx,%edi
    sarl    $TAGSHIFT,%edi
    cdq
    idiv    %edi
    MAKETAGGED(%eax,%eax)
    MAKETAGGED(%edx,%edx)
    movl    %eax,%edi
    movl    %eax,(%ecx)
    movl    %edx,POLYWORDSIZE(%ecx)
    movl    %ecx,%eax
    ret     $4

mem_for_remquot1:  # Not enough store: clobber bad value in ecx.
    movl   $1,%ecx

quotrem_really_long:
    movl    %eax,%edi
    CALL_IO(POLY_SYS_quotrem)


# TODO: Isn't this byte equality?
INLINE_ROUTINE(equal_long)
    cmpl    %eax,%ebx
    je      RetTrue
    movl    %eax,%ecx   # If either is short
    orl     %ebx,%eax   # the result is false
    andl    $TAG,%eax
    jnz     RetFalse
    movl    %ecx,%eax
    CALL_IO(POLY_SYS_equala)



INLINE_ROUTINE(or_long)
#ifdef NOTATTHEMOMENT
    movl    %eax,%edi
    andl    %ebx,%edi
    andl    $TAG,%edi
    jz      or_really_long
    orl     %ebx,%eax
    movl    %eax,%edi
    ret
or_really_long:
#endif
    CALL_IO(POLY_SYS_ora)


INLINE_ROUTINE(xor_long)
#ifdef NOTATTHEMOMENT
    movl    %eax,%edi
    andl    %ebx,%edi
    andl    $TAG,%edi
    jz      xor_really_long
    xorl    %ebx,%eax
    orl     $TAG,%eax  # restore the tag
    movl    %eax,%edi
    ret
xor_really_long:
#endif
    CALL_IO(POLY_SYS_xora)


INLINE_ROUTINE(and_long)
#ifdef NOTATTHEMOMENT
    movl    %eax,%edi
    andl    %ebx,%edi
    andl    $TAG,%edi
    jz      and_really_long
    andl    %ebx,%eax
    movl    %eax,%edi
    ret
and_really_long:
#endif
    CALL_IO(POLY_SYS_anda)


INLINE_ROUTINE(neg_long)
    testl   $TAG,%eax
    jz      neg_really_long
    movl    $(TAGGED(0)+TAG),%edi
    subl    %eax,%edi
    jo      neg_really_long
    movl    %edi,%eax
    ret
neg_really_long:
    movl    %eax,%edi
    CALL_IO(POLY_SYS_aneg)


INLINE_ROUTINE(int_geq)
    testl   $TAG,%eax # Is first arg short?
    jz      igeq2
    testl   $TAG,%ebx # Is second arg short?
    jz      igeq1
    cmpl    %ebx,%eax
    jge     RetTrue
    jmp     RetFalse
igeq1:
 # First arg is short, second isn't
    testb   $16,(-1)(%ebx)     # 16 is the "negative" bit
    jnz     RetTrue     # Negative - always less
    jmp     RetFalse

igeq2:
 # First arg is long
    testl   $TAG,%ebx # Is second arg short?
    jz      igeq3
 # First arg is long, second is short
    testb   $16,(-1)(%eax)     # 16 is the "negative" bit
    jz      RetTrue    # Positive - always greater
    jmp     RetFalse

igeq3:
 # Both long
    CALL_IO(POLY_SYS_int_geq)



INLINE_ROUTINE(int_leq)
    testl   $TAG,%eax # Is first arg short?
    jz      ileq2
    testl   $TAG,%ebx # Is second arg short?
    jz      ileq1
    cmpl    %ebx,%eax
    jle     RetTrue
    jmp     RetFalse
ileq1:
 # First arg is short, second isn't
    testb   $16,(-1)(%ebx)     # 16 is the "negative" bit
    jz      RetTrue     # Negative - always less
    jmp     RetFalse

ileq2:
 # First arg is long
    testl   $TAG,%ebx # Is second arg short?
    jz      ileq3
 # First arg is long, second is short
    testb   $16,(-1)(%eax)     # 16 is the "negative" bit
    jnz     RetTrue    # Positive - always greater
    jmp     RetFalse

ileq3:
    CALL_IO(POLY_SYS_int_leq)



INLINE_ROUTINE(int_gtr)
    testl   $TAG,%eax # Is first arg short?
    jz      igtr2
    testl   $TAG,%ebx # Is second arg short?
    jz      igtr1
    cmpl    %ebx,%eax
    jg      RetTrue
    jmp     RetFalse
igtr1:
 # First arg is short, second isn't
    testb   $16,(-1)(%ebx)     # 16 is the "negative" bit
    jnz     RetTrue     # Negative - always less
    jmp     RetFalse

igtr2:
 # First arg is long
    testl   $TAG,%ebx # Is second arg short?
    jz      igtr3
 # First arg is long, second is short
    testb   $16,(-1)(%eax)     # 16 is the "negative" bit
    jz      RetTrue    # Positive - always greater
    jmp     RetFalse

igtr3:
    CALL_IO(POLY_SYS_int_gtr)



INLINE_ROUTINE(int_lss)
    testl   $TAG,%eax # Is first arg short?
    jz      ilss2
    testl   $TAG,%ebx # Is second arg short?
    jz      ilss1
    cmpl    %ebx,%eax
    jl      RetTrue
    jmp     RetFalse
ilss1:
 # First arg is short, second isn't
    testb   $16,(-1)(%ebx)     # 16 is the "negative" bit
    jz      RetTrue     # Negative - always less
    jmp     RetFalse

ilss2:
 # First arg is long
    testl   $TAG,%ebx # Is second arg short?
    jz      ilss3
 # First arg is long, second is short
    testb   $16,(-1)(%eax)     # 16 is the "negative" bit
    jnz     RetTrue    # Positive - always greater
    jmp     RetFalse

ilss3:
    CALL_IO(POLY_SYS_int_lss)


INLINE_ROUTINE(offset_address)
 # This is needed in the code generator, but is a very risky thing to do.
    shrl    $TAGSHIFT,%ebx     # Untag
    addl    %ebx,%eax       # and add in
    movl    %eax,%ebx
    ret


# General test routine.  Returns with the condition codes set
# appropriately.

teststr:
    testl   $TAG,%eax     # Is arg1 short
    jz      tststr1
    testl   $TAG,%ebx     # Yes: is arg2 also short?
    jz      tststr0a
    # Both are short - just compare the characters
    cmpl    %ebx,%eax
    ret

tststr0a:
    movl    $1,%edi        # Is arg2 the null string ?
    cmpl    (%ebx),%edi
    jg      tststr4            # Return with "gtr" set if it is
    shrl    $TAGSHIFT,%eax
    cmpb    POLYWORDSIZE(%ebx),%al
    jne     tststr4            # If they're not equal that's the result
    cmpl    $256,%eax     # But if they're equal set "less" because A is less than B
    jmp     tststr4

tststr1: # arg2 is not short.  Is arg1 ?
    testl   $TAG,%ebx
    jz      tststr2
    movl    (%eax),%edi        # Is arg1 the null string
    cmpl    $1,%edi
    jl      tststr4            # Return with "less" set if it is
    shrl    $TAGSHIFT,%ebx
    movb    POLYWORDSIZE(%eax),%cl
    cmpb    %bl,%cl
    jne     tststr4            # If they're not equal that's the result
    cmpl    $0,%edi      # But if they're equal set "greater" because A is greater than B
    jmp     tststr4

tststr2:
    movl    (%eax),%edi     # Get length.
    movl    (%ebx),%ecx     # 
    cmpl    %ecx,%edi       # Find shorter length
    jge     tststr3
    movl    %edi,%ecx
tststr3:
    leal    POLYWORDSIZE(%eax),%esi    # Load ptrs for cmpsb
    leal    POLYWORDSIZE(%ebx),%edi
    cld                 # Make sure we increment
    cmpl    %eax,%eax       # Set the Zero bit
    repe; cmpsb           # Compare while equal and %rcx > 0
    jnz     tststr4
 # Strings are equal as far as the shorter of the two.  Have to compare
 # the lengths.
    movl    (%eax),%edi
    cmpl    (%ebx),%edi
tststr4:
    movl    $1,%eax      # Clobber these
    movl    %eax,%ebx       
    movl    %eax,%ecx       
    movl    %eax,%esi
    movl    %eax,%edi
    ret

 # These functions compare strings for lexical ordering.  This version, at
 # any rate, assumes that they are UNSIGNED bytes.

INLINE_ROUTINE(str_compare)
    call    teststr
    ja      RetTrue         # Return TAGGED(1) if it's greater
    je      RetFalse        # Return TAGGED(0) if it's equal
    movl    $MINUS1,%eax   # Return TAGGED(-1) if it's less.
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
    movl    12(%esp),%edi               # Destination address
    movl    8(%esp),%ecx                # Destination offset, untagged
    shrl    $TAGSHIFT,%ecx
    addl    %ecx,%edi
    movl    %eax,%esi                   # Source address
    shrl    $TAGSHIFT,%ebx
    addl    %ebx,%esi
    movl    4(%esp),%ecx                # Get the length to move
    shrl    $TAGSHIFT,%ecx
    cld                     # Make sure we increment
    cmpl    %eax,%eax       # Set the Zero bit
    repe; cmpsb
    movl    %eax,%esi       # Make these valid
    movl    %eax,%ecx
    movl    %eax,%edi
    jz      bvTrue
    movl    $FALSE,%eax
    jmp     bvRet
bvTrue:
    movl    $TRUE,%eax
bvRet:
    ret     $12




INLINE_ROUTINE(is_big_endian)
    jmp     RetFalse    # I386/486 is little-endian


INLINE_ROUTINE(bytes_per_word)
    movl    $TAGGED(POLYWORDSIZE),%eax  # 4/8 bytes per word
    ret


 # Word functions.  These are all unsigned and do not raise Overflow
 
INLINE_ROUTINE(mul_word)
    shrl    $TAGSHIFT,%ebx # Untag the multiplier
    subl    $TAG,%eax      # Remove the tag from the multiplicand
    mull    %ebx                # unsigned multiplication
    addl    $TAG,%eax      # Add back the tag, but don`t shift
    movl    %eax,%edx           # clobber this which has the high-end result
    movl    %eax,%ebx           # and the other bad result.
    ret


INLINE_ROUTINE(plus_word)
    leal    (-TAG)(%eax,%ebx),%eax  # Add the values and subtract a tag
    ret


INLINE_ROUTINE(minus_word)
    subl    %ebx,%eax
    addl    $TAG,%eax          # Put back the tag
    ret


INLINE_ROUTINE(div_word)
    shrl    $TAGSHIFT,%ebx     # Check for division by zero is done in ML
    shrl    $TAGSHIFT,%eax
    movl    $0,%edx
    div     %ebx
    MAKETAGGED(%eax,%eax)
    movl    %eax,%edx
    movl    %eax,%ebx
    ret


INLINE_ROUTINE(mod_word)
    shrl    $TAGSHIFT,%ebx # Check for division by zero is done in ML
    shrl    $TAGSHIFT,%eax
    movl    $0,%edx
    div     %ebx
    MAKETAGGED(%edx,%eax)
    movl    %eax,%edx
    movl    %eax,%ebx
    ret


INLINE_ROUTINE(word_eq)
    cmpl    %ebx,%eax
    jz      RetTrue         # True if they are equal.
    jmp     RetFalse


INLINE_ROUTINE(word_geq)
    cmpl    %ebx,%eax
    jnb     RetTrue
    jmp     RetFalse


INLINE_ROUTINE(word_leq)
    cmpl    %ebx,%eax
    jna     RetTrue
    jmp     RetFalse


INLINE_ROUTINE(word_gtr)
    cmpl    %ebx,%eax
    ja      RetTrue
    jmp     RetFalse


 INLINE_ROUTINE(word_lss)
    cmpl    %ebx,%eax
    jb      RetTrue
    jmp     RetFalse


INLINE_ROUTINE(fixed_geq)
    cmpl    %ebx,%eax
    jge     RetTrue
    jmp     RetFalse


INLINE_ROUTINE(fixed_leq)
    cmpl    %ebx,%eax
    jle     RetTrue
    jmp     RetFalse


INLINE_ROUTINE(fixed_gtr)
    cmpl    %ebx,%eax
    jg      RetTrue
    jmp     RetFalse


INLINE_ROUTINE(fixed_lss)
    cmpl    %ebx,%eax
    jl      RetTrue
    jmp     RetFalse


INLINE_ROUTINE(fixed_add)
    leal    (-TAG)(%eax),%eax
    addl    %ebx,%eax
    jo      raiseOverflowEx
    ret


INLINE_ROUTINE(fixed_sub)
    subl    %ebx,%eax
    jo      raiseOverflowEx
    addl    $TAG,%eax      # Put back the tag
    ret


INLINE_ROUTINE(fixed_mul)
    sarl    $TAGSHIFT,%ebx # Untag the multiplier
    subl    $TAG,%eax      # Remove the tag from the multiplicand
    imull   %ebx,%eax           # signed multiplication
    jo      raiseOverflowEx
    addl    $TAG,%eax      # Add back the tag, but don`t shift
    movl    %eax,%edx           # clobber this which has the high-end result
    movl    %eax,%ebx           # and the other bad result.
    ret


INLINE_ROUTINE(fixed_quot)
# Checking for overflow and zero is done in ML
    sarl    $TAGSHIFT,%ebx
    sarl    $TAGSHIFT,%eax
    cdq
    idiv    %ebx
    MAKETAGGED(%eax,%eax)
    movl    %eax,%edx
    movl    %eax,%ebx
    ret


INLINE_ROUTINE(fixed_rem)
    sarl    $TAGSHIFT,%ebx
    sarl    $TAGSHIFT,%eax
    cdq
    idiv     %ebx
    MAKETAGGED(%edx,%eax)
    movl    %eax,%edx
    movl    %eax,%ebx
    ret


INLINE_ROUTINE(fixed_div)
# Checking for overflow and zero is done in ML
    sarl    $TAGSHIFT,%ebx
    sarl    $TAGSHIFT,%eax
    cdq
    idiv    %ebx
    cmpl    $0,%edx
    jz      fixed_div1              # If the remainder if non-zero ...
    xorl    %edx,%ebx               # and has a different sign from the divisor ...
    jns     fixed_div1
    subl    $1,%eax                 # subtract one to round to -infinity rather than zero.
fixed_div1:
    MAKETAGGED(%eax,%eax)
    movl    %eax,%edx
    movl    %eax,%ebx
    ret


INLINE_ROUTINE(fixed_mod)
    sarl    $TAGSHIFT,%ebx
    sarl    $TAGSHIFT,%eax
    cdq
    idiv     %ebx
# Result is in %edx.  We have to change the result so that it has the sign as the divisor.
    cmpl    $0,%edx
    jz      fixed_mod1  # Result is zero - no change
    xorl    %edx,%ebx
    jns     fixed_mod1  # Skip if they had the same signs
    xorl    %edx,%ebx   # Restore the original divisor
    addl    %ebx,%edx   # And add it in
fixed_mod1:
    MAKETAGGED(%edx,%eax)
    movl    %eax,%edx
    movl    %eax,%ebx
    ret


raiseOverflowEx:
# Build the exception packet.
# Allocate four word item.
# Set first word to TAGGED(5)
# Set second word to the string "Overflow" - That's going to need allocation.
# Set third and fourth words to TAGGED(0)
# Raise the exception.
    jmp     *Fr_RaiseOverflow(%ebp)

# Atomically increment the value at the address of the arg and return the
# updated value.  Since the xadd instruction returns the original value
# we have to increment it.
INLINE_ROUTINE(atomic_increment)
atomic_incr:                    # Internal name in case "atomic_increment" is munged.
    movl    $2,%ebx
    lock; xaddl %ebx,(%eax)
    addl    $2,%ebx
    movl    %ebx,%eax
    ret



# Atomically decrement the value at the address of the arg and return the
# updated value.  Since the xadd instruction returns the original value
# we have to decrement it.
INLINE_ROUTINE(atomic_decrement)
atomic_decr:
    movl    $-2,%ebx
    lock; xaddl %ebx,(%eax)
    movl    %ebx,%eax
    subl    $2,%eax
    ret



/* Reset a mutex to (tagged) one.  Because the increment and decrements
   are atomic this doesn't have to do anything special. */
INLINE_ROUTINE(atomic_reset)
    movl    $3,(%eax)
    movl    $UNIT,%eax  # The function returns unit
    ret



# Return the thread id object for the current thread
INLINE_ROUTINE(thread_self)
    movl    Fr_Argument(%ebp),%eax
    movl    Arg_ThreadId(%eax),%eax
    ret




/* Memory for LargeWord.word values.  This is the same as mem_for_real on
  64-bits but only a single word on 32-bits.
  ********************************
  Some of this code is temporary.  The final version should compute the result and
  simply jump here to box it.  That requires the heap-overflow code to save the
  registers across the trap but not to examine them for pointers.  Temporarily we
  don't do that but instead clear all the registers across a trap. */
mem_for_largeword:
        movl    Fr_LocalMpointer(%ebp),%ecx
        subl    $8,%ecx        # Length word (4 bytes) + 4 bytes
#ifdef TEST_ALLOC
# Test case - this will always force a call into RTS.
        cmpl    Fr_LocalMpointer(%ebp),%ecx
#else
        cmpl    Fr_LocalMbottom(%ebp),%ecx
#endif
        jnb      mem_for_largeword1
/* ********************************
  Temporarily: push these registers to the stack
  and pop them afterwards.  This isn't the final version of this code
  but is useful as a test. */
        pushl   %eax
        pushl    %ebx
        call     callheapOverflowLocal
        popl     %ebx
        popl     %eax
mem_for_largeword1:
        movl    %ecx,Fr_LocalMpointer(%ebp) # Updated allocation pointer
        movl    $0x01000001,(-4)(%ecx)     # Length word
        ret

# FLOATING POINT

mem_for_real:
# Allocate memory for the result.
        movl    Fr_LocalMpointer(%ebp),%ecx
        subl    $12,%ecx        # Length word (4 bytes) + 8 bytes
#ifdef TEST_ALLOC
# Test case - this will always force a call into RTS.
        cmpl    Fr_LocalMpointer(%ebp),%ecx
#else
        cmpl    Fr_LocalMbottom(%ebp),%ecx
#endif
        jnb      mem_for_real1
/* ********************************
  Temporarily: push these registers to the stack
  and pop them afterwards.  This isn't the final version of this code
  but is useful as a test. */
        pushl   %eax
        pushl    %ebx
        call     callheapOverflowLocal
        popl     %ebx
        popl     %eax
mem_for_real1:
        movl    %ecx,Fr_LocalMpointer(%ebp) # Updated allocation pointer
        movl    $0x01000002,(-4)(%ecx)     # Two words plus tag
        ret


INLINE_ROUTINE(real_add)
    call    mem_for_real
/* Do the operation and put the result in the allocated space. */
    fldl    (%eax)
    faddl   (%ebx)
    fstpl   (%ecx)
    movl    %ecx,%eax
    ret

/* The mask includes FP7 rather than FP0 because this pushes a value which
   overwrites the bottom of the stack. */




INLINE_ROUTINE(real_sub)
    call    mem_for_real
    fldl    (%eax)
    fsubl   (%ebx)
    fstpl   (%ecx)
    movl    %ecx,%eax
    ret




INLINE_ROUTINE(real_mul)
    call    mem_for_real
    fldl    (%eax)
    fmull   (%ebx)
    fstpl   (%ecx)
    movl    %ecx,%eax
    ret




INLINE_ROUTINE(real_div)
    call    mem_for_real
    fldl    (%eax)
    fdivl   (%ebx)
    fstpl   (%ecx)
    movl    %ecx,%eax
    ret




# For all values except NaN it's possible to do this by a test such as
# "if x < 0.0 then ~ x else x" but the test always fails for NaNs

INLINE_ROUTINE(real_abs)
    movl    %eax,%ebx                # Put a valid value in %ebx
    call    mem_for_real
# N.B. Real.~ X is not the same as 0.0 - X.  Real.~ 0.0 is ~0.0;
    fldl    (%eax)
    fabs
    fstpl   (%ecx)
    movl    %ecx,%eax
    ret




INLINE_ROUTINE(real_neg)
    movl    %eax,%ebx                # Put a valid value in %ebx
    call    mem_for_real
# Do the operation and put the result in the allocated
# space.
# N.B. Real.~ X is not the same as 0.0 - X.  Real.~ 0.0 is ~0.0;
    fldl    (%eax)
    fchs
    fstpl   (%ecx)
    movl    %ecx,%eax
    ret





INLINE_ROUTINE(real_eq)
    fldl    (%eax)
    fcompl  (%ebx)
    fnstsw  %ax
# Not all 64-bit processors support SAHF.
# The result is true if the zero flag is set and parity flag clear.  
    andl    $17408,%eax        # 0x4400
    cmpl    $16384,%eax        # 0x4000
    je      RetTrue
    jmp     RetFalse



INLINE_ROUTINE(real_neq)
    fldl    (%eax)
    fcompl  (%ebx)
    fnstsw  %ax
    andl    $17408,%eax        # 0x4400
    cmpl    $16384,%eax        # 0x4000
    jne     RetTrue
    jmp     RetFalse




INLINE_ROUTINE(real_lss)
# Compare %ebx > %eax
    fldl    (%ebx)
    fcompl  (%eax)
    fnstsw  %ax

# True if the carry flag (C0), zero flag (C3) and parity (C2) are all clear
    andl    $17664,%eax # 0x4500
    je      RetTrue
    jmp     RetFalse




INLINE_ROUTINE(real_gtr)
    fldl    (%eax)
    fcompl  (%ebx)
    fnstsw  %ax

# True if the carry flag (C0), zero flag (C3) and parity (C2) are all clear
    andl    $17664,%eax    # 0x4500
    je      RetTrue
    jmp     RetFalse




INLINE_ROUTINE(real_leq)
# Compare %ebx > %eax
    fldl    (%ebx)
    fcompl  (%eax)
    fnstsw  %ax
# True if the carry flag (C0) and parity (C2) are both clear
    andl    $1280,%eax # 0x500
    je      RetTrue
    jmp     RetFalse




INLINE_ROUTINE(real_geq)
    fldl    (%eax)
    fcompl  (%ebx)
    fnstsw  %ax
# True if the carry flag (C0) and parity (C2) are both clear
    andl    $1280,%eax # 0x500
    je      RetTrue
    jmp     RetFalse



INLINE_ROUTINE(real_from_int)
    testl   $TAG,%eax   # Is it long ?
    jnz      fixed_to_real_internal

real_float_1:
    CALL_IO(POLY_SYS_int_to_real)



INLINE_ROUTINE(fixed_to_real)
fixed_to_real_internal:
    movl    %eax,%ebx                # Put a valid value in %ebx
    call    mem_for_real
    sarl    $TAGSHIFT,%eax # Untag the value
    pushl   %eax                     # Push it to the stack.  We can only load it from memory
    fildl   (%esp)
    popl    %eax
    fstpl   (%ecx)
    movl    %ecx,%eax
    ret



INLINE_ROUTINE(set_exception_trace)
    movl    %eax,%edx                   # Target closure
    movl    $UNIT,%eax             # The function takes a unit arg.
    jmp     *(%edx)

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
#ifndef HOSTARCHITECTURE_X86_64
    movl    4(%esp),%eax
#else
    movl    %edi,%eax   # On X86_64 the argument is passed in %edi
#endif
# Use %ecx and %eax because they are volatile (unlike %ebx on X86/64/Unix)
    movl    $2,%ecx
    lock; xaddl %ecx,(%eax)
    addl    $2,%ecx
    movl    %ecx,%eax
    ret

# LargeWord.word operations.  These are 32 or 64-bit values in a single-word byte
# memory cell.
INLINE_ROUTINE(eq_longword)
    movl    (%eax),%eax
    cmpl    (%ebx),%eax
    jz      RetTrue         # True if they are equal.
    jmp     RetFalse


INLINE_ROUTINE(geq_longword)

    movl    (%eax),%eax
    cmpl    (%ebx),%eax
    jnb     RetTrue
    jmp     RetFalse


INLINE_ROUTINE(leq_longword)
    movl    (%eax),%eax
    cmpl    (%ebx),%eax
    jna     RetTrue
    jmp     RetFalse


INLINE_ROUTINE(gt_longword)
    movl    (%eax),%eax
    cmpl    (%ebx),%eax
    ja      RetTrue
    jmp     RetFalse


INLINE_ROUTINE(lt_longword)
    movl    (%eax),%eax
    cmpl    (%ebx),%eax
    jb      RetTrue
    jmp     RetFalse


INLINE_ROUTINE(longword_to_tagged)
# Load the value and tag it, discarding the top bit
    movl    (%eax),%eax
        MAKETAGGED(%eax,%eax)
    ret


INLINE_ROUTINE(signed_to_longword)
# Shift the value to remove the tag and store it.
    movl    %eax,%ebx                   # mem_for_largeword may push rebx
    call    mem_for_largeword
    sarl    $TAGSHIFT,%eax         # Arithmetic shift, preserve sign
    movl    %eax,(%ecx)
    movl    %ecx,%eax
    ret


INLINE_ROUTINE(unsigned_to_longword)
# Shift the value to remove the tag and store it.
    movl    %eax,%ebx                   # mem_for_largeword may push rebx
    call    mem_for_largeword
    shrl    $TAGSHIFT,%eax         # Logical shift, zero top bit
    movl    %eax,(%ecx)
    movl    %ecx,%eax
    ret


INLINE_ROUTINE(plus_longword)
    call    mem_for_largeword
    movl    (%eax),%eax
    addl    (%ebx),%eax
    movl    %eax,(%ecx)
    movl    %ecx,%eax
    ret


INLINE_ROUTINE(minus_longword)
    call    mem_for_largeword
    movl    (%eax),%eax
    subl    (%ebx),%eax
    movl    %eax,(%ecx)
    movl    %ecx,%eax
    ret


INLINE_ROUTINE(mul_longword)
    call    mem_for_largeword
    movl    (%eax),%eax
    mull    (%ebx)
    movl    %eax,(%ecx)
    movl    %ecx,%eax
    movl    %eax,%edx           # clobber this which has the high-end result
    ret


INLINE_ROUTINE(div_longword)
# Check for division by zero is done in ML
    call    mem_for_largeword
    movl    (%eax),%eax
    movl    $0,%edx
    divl    (%ebx)
    movl    %eax,(%ecx)         # Store the quotient
    movl    %ecx,%eax
    movl    %eax,%edx           # clobber this which has the remainder
    ret



INLINE_ROUTINE(mod_longword)
# Check for division by zero is done in ML
    call    mem_for_largeword
    movl    (%eax),%eax
    movl    $0,%edx
    divl    (%ebx)
    movl    %edx,(%ecx)         # Store the remainder
    movl    %ecx,%eax
    movl    %eax,%edx           # clobber this which has the remainder
    ret



INLINE_ROUTINE(andb_longword)
    call    mem_for_largeword
    movl    (%eax),%eax
    andl    (%ebx),%eax
    movl    %eax,(%ecx)
    movl    %ecx,%eax
    ret



INLINE_ROUTINE(orb_longword)
    call    mem_for_largeword
    movl    (%eax),%eax
    orl     (%ebx),%eax
    movl    %eax,(%ecx)
    movl    %ecx,%eax
    ret



INLINE_ROUTINE(xorb_longword)
    call    mem_for_largeword
    movl    (%eax),%eax
    xorl    (%ebx),%eax
    movl    %eax,(%ecx)
    movl    %ecx,%eax
    ret



INLINE_ROUTINE(shift_left_longword)
    call    mem_for_largeword
    movl    %ecx,%edx           # We need %ecx for the shift
 # The shift value is always a Word.word value i.e. tagged
 # LargeWord.<<(a,b) is defined to return 0 if b > LargeWord.wordSize
#ifndef HOSTARCHITECTURE_X86_64
    cmpl    $TAGGED(32),%ebx
#else
    cmpl    $TAGGED(64),%ebx
#endif
    jb      sllw1
    movl    $0,%eax
    jmp     sllw2
sllw1:
    movl    %ebx,%ecx
    shrl    $TAGSHIFT,%ecx # remove tag
    movl    (%eax),%eax
    shll    %cl,%eax
sllw2:
    movl    %eax,(%edx)
    movl    %edx,%eax
    movl    %eax,%ecx           # Clobber %ecx
    ret


INLINE_ROUTINE(shift_right_longword)
    call    mem_for_largeword
    movl    %ecx,%edx           # We need %ecx for the shift
/* The shift value is always a Word.word value i.e. tagged
   LargeWord.>>(a,b) is defined to return 0 if b > LargeWord.wordSize */
#ifndef HOSTARCHITECTURE_X86_64
    cmpl    $TAGGED(32),%ebx
#else
    cmpl    $TAGGED(64),%ebx
#endif
    jb      srlw1
    movl    $0,%eax
    jmp     srlw2
srlw1:
    movl    %ebx,%ecx
    shrl    $TAGSHIFT,%ecx # remove tag
    movl    (%eax),%eax
    shrl    %cl,%eax
srlw2:
    movl    %eax,(%edx)
    movl    %edx,%eax
    movl    %eax,%ecx           # Clobber %ecx
    ret


INLINE_ROUTINE(shift_right_arith_longword)
    call    mem_for_largeword
    movl    %ecx,%edx           # We need %ecx for the shift
/* The shift value is always a Word.word value i.e. tagged
   LargeWord.~>>(a,b) is defined to return 0 or ~1 if b > LargeWord.wordSize */
    cmpl    $TAGGED(32),%ebx
    jb      sralw1
    # Setting the shift to 31/63 propagates the sign bit
    movl    $TAGGED(31),%ebx
sralw1:
    movl    %ebx,%ecx
    shrl    $TAGSHIFT,%ecx # remove tag
    movl    (%eax),%eax
    sarl    %cl,%eax
    movl    %eax,(%edx)
    movl    %edx,%eax
    movl    %eax,%ecx           # Clobber %ecx
    ret


# C-memory operations.
INLINE_ROUTINE(cmem_load_asm_8)
    movl    (%eax),%eax             # The address is boxed.
    sarl    $TAGSHIFT,%ebx     # The offset is a signed tagged value
    addl    %ebx,%eax               # Add it in
    movl    4(%esp),%ebx            # Get the index.
    sarl    $TAGSHIFT,%ebx     # That's also tagged
    movzbl  (%eax,%ebx,1),%eax
    MAKETAGGED(%eax,%eax)
    movl    %eax,%ebx       # Clobber bad value in %%ebx
    ret $4


INLINE_ROUTINE(cmem_load_asm_16)
    movl    (%eax),%eax             # The address is boxed.
    sarl    $TAGSHIFT,%ebx     # The offset is a signed tagged value
    addl    %ebx,%eax               # Add it in
    movl    4(%esp),%ebx            # Get the index.
    # The index is tagged but since we want to multiply by two we don't need anything here.
    movzwl  -1(%eax,%ebx,1),%eax
    MAKETAGGED(%eax,%eax)
    ret $4


INLINE_ROUTINE(cmem_load_asm_32)
# 32-bit mode - the result is boxed
    call    mem_for_largeword
    movl    (%eax),%eax             # The address is boxed.
    sarl    $TAGSHIFT,%ebx     # The offset is a signed tagged value
    addl    %ebx,%eax               # Add it in
    movl    4(%esp),%ebx            # Get the index.
    movl    (-2)(%eax,%ebx,2),%eax
    movl    %eax,(%ecx)             # Save in the new memory
    movl    %ecx,%eax               # Copy the result address
    ret $4



INLINE_ROUTINE(cmem_load_asm_float)
    call    mem_for_real
    movl    (%eax),%eax             # The address is boxed.
    sarl    $TAGSHIFT,%ebx     # The offset is a signed tagged value
    addl    %ebx,%eax               # Add it in
    movl    4(%esp),%ebx            # Get the index.
    flds    -2(%eax,%ebx,2)
    fstpl   (%ecx)
    movl    %ecx,%eax
    ret $4



INLINE_ROUTINE(cmem_load_asm_double)
    call    mem_for_real
    movl    (%eax),%eax             # The address is boxed.
    sarl    $TAGSHIFT,%ebx     # The offset is a signed tagged value
    addl    %ebx,%eax               # Add it in
    movl    4(%esp),%ebx            # Get the index.
    fldl    -4(%eax,%ebx,4)
    fstpl   (%ecx)
    movl    %ecx,%eax
    ret $4


   
INLINE_ROUTINE(cmem_store_asm_8)
    movl    (%eax),%eax             # The address is boxed.
    sarl    $TAGSHIFT,%ebx     # The offset is a signed tagged value
    addl    %ebx,%eax               # Add it in
    movl    8(%esp),%ebx            # Get the index.
    movl    4(%esp),%ecx            # Get the value to store
    sarl    $TAGSHIFT,%ebx     # That's also tagged
    sarl    $TAGSHIFT,%ecx
    movb    %cl,(%eax,%ebx)
    movl    $UNIT,%eax             # The function returns unit
    movl    %eax,%ebx                   # Clobber bad value in %%ebx
    movl    %eax,%ecx                   # and %%ecx
    ret $8


INLINE_ROUTINE(cmem_store_asm_16)
    movl    (%eax),%eax             # The address is boxed.
    sarl    $TAGSHIFT,%ebx     # The offset is a signed tagged value
    addl    %ebx,%eax               # Add it in
    movl    8(%esp),%ebx            # Get the index.
    movl    4(%esp),%ecx            # Get the value to store
    sarl    $TAGSHIFT,%ecx     # Untag the value to store
    movw    %cx,-1(%eax,%ebx,1)
    movl    $UNIT,%eax             # The function returns unit
    movl    %eax,%ecx                   # Bad value in %%ecx
    ret $8


INLINE_ROUTINE(cmem_store_asm_32)
    movl    (%eax),%eax             # The address is boxed.
    sarl    $TAGSHIFT,%ebx     # The offset is a signed tagged value
    addl    %ebx,%eax               # Add it in
    movl    8(%esp),%ebx            # Get the index.
    movl    4(%esp),%ecx            # Get the value to store
    movl    (%ecx),%ecx
    movl    %ecx,-2(%eax,%ebx,2)
    movl    $UNIT,%eax             # The function returns unit
    movl    %eax,%ecx                   # Bad value in %%ecx
    ret $8


INLINE_ROUTINE(cmem_store_asm_float)
    movl    (%eax),%eax             # The address is boxed.
    sarl    $TAGSHIFT,%ebx     # The offset is a signed tagged value
    addl    %ebx,%eax               # Add it in
    movl    8(%esp),%ebx            # Get the index.
    movl    4(%esp),%ecx            # Get the address of the real
    fldl    (%ecx)
    fstps   -2(%eax,%ebx,2)
    movl    $UNIT,%eax         # The function returns unit
    movl    %eax,%ebx               # Bad value in %%ebx
    ret $8


INLINE_ROUTINE(cmem_store_asm_double)
    movl    (%eax),%eax             # The address is boxed.
    sarl    $TAGSHIFT,%ebx     # The offset is a signed tagged value
    addl    %ebx,%eax               # Add it in
    movl    8(%esp),%ebx            # Get the index.
    movl    4(%esp),%ecx            # Get the address of the real
    fldl    (%ecx)
    fstpl   -4(%eax,%ebx,4)
    movl    $UNIT,%eax         # The function returns unit
    movl    %eax,%ebx               # Bad value in %%ebx
    ret $8

INLINE_ROUTINE(get_flags)
    movzbl  -1(%eax),%eax
    MAKETAGGED(%eax,%eax)
    ret

#define CREATE_IO_CALL(index) \
    INLINE_ROUTINE(Call##index##) \
    CALL_IO(index)

#define CREATE_EXTRA_CALL(index) \
    INLINE_ROUTINE(X86AsmCallExtra##index##) \
    CALL_EXTRA(index)

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

