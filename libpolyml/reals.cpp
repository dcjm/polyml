/*
    Title:  Real number package.
    Author:     Dave Matthews, Cambridge University Computer Laboratory

    Copyright (c) 2000
        Cambridge University Technical Services Limited

    Further work copyright David C.J. Matthews 2011, 2016

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

#ifdef HAVE_IEEEFP_H
/* Other operating systems include "finite" in math.h, but Solaris doesn't? */
#include <ieeefp.h>
#endif

#ifdef HAVE_FPU_CONTROL_H
#include <fpu_control.h>
#endif

#ifdef HAVE_FENV_H
#include <fenv.h>
#endif

#if (defined(_MSC_VER))
#define finite  _finite
#define isnan   _isnan
#define copysign _copysign
#endif

#ifdef HAVE_FLOAT_H
#include <float.h>
#endif

#ifdef HAVE_MATH_H
#include <math.h>
#endif

#ifdef HAVE_STDIO_H
#include <stdio.h>
#endif

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#ifdef HAVE_ERRNO_H
#include <errno.h>
#endif

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#ifdef HAVE_STDINT_H
#include <stdint.h>
#endif

#ifdef HAVE_ASSERT_H 
#include <assert.h>
#define ASSERT(x)   assert(x)
#else
#define ASSERT(x)
#endif

#include "globals.h"
#include "run_time.h"
#include "reals.h"
#include "arb.h"
#include "sys.h"
#include "realconv.h"
#include "polystring.h"
#include "save_vec.h"
#include "rts_module.h"
#include "machine_dep.h"
#include "processes.h"
#include "rtsentry.h"

/*
    The Standard Basis Library assumes IEEE representation for reals.  Among other
    things it does not permit equality on reals.  That simplifies things
    considerably since we don't have to worry about there being two different
    representations of zero as 0 and ~0.  We also don't need to check that the
    result is finite since NaN is allowed as a result.
    This code could do with being checked by someone who really understands
    IEEE floating point arithmetic.

    The original real number functions all had separate entries in the interface
    vector.  Newly added functions all go through a single dispatch function.
    DCJM: March 2000.
*/

extern "C" {
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyRealBoxedToString(PolyObject *threadId, PolyWord arg, PolyWord mode, PolyWord digits);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyRealGeneral(PolyObject *threadId, PolyWord code, PolyWord arg);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyRealBoxedFromString(PolyObject *threadId, PolyWord str);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyRealBoxedToLongInt(PolyObject *threadId, PolyWord arg);
    POLYEXTERNALSYMBOL double PolyRealSqrt(double arg);
    POLYEXTERNALSYMBOL double PolyRealSin(double arg);
    POLYEXTERNALSYMBOL double PolyRealCos(double arg);
    POLYEXTERNALSYMBOL double PolyRealArctan(double arg);
    POLYEXTERNALSYMBOL double PolyRealExp(double arg);
    POLYEXTERNALSYMBOL double PolyRealLog(double arg);
    POLYEXTERNALSYMBOL double PolyRealTan(double arg);
    POLYEXTERNALSYMBOL double PolyRealArcSin(double arg);
    POLYEXTERNALSYMBOL double PolyRealArcCos(double arg);
    POLYEXTERNALSYMBOL double PolyRealLog10(double arg);
    POLYEXTERNALSYMBOL double PolyRealSinh(double arg);
    POLYEXTERNALSYMBOL double PolyRealCosh(double arg);
    POLYEXTERNALSYMBOL double PolyRealTanh(double arg);
    POLYEXTERNALSYMBOL double PolyRealFloor(double arg);
    POLYEXTERNALSYMBOL double PolyRealCeil(double arg);
    POLYEXTERNALSYMBOL double PolyRealTrunc(double arg);
    POLYEXTERNALSYMBOL double PolyRealRound(double arg);
    POLYEXTERNALSYMBOL double PolyFloatArbitraryPrecision(PolyWord arg);
    POLYEXTERNALSYMBOL POLYSIGNED PolyGetRoundingMode(PolyWord);
    POLYEXTERNALSYMBOL POLYSIGNED PolySetRoundingMode(PolyWord);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyRealSize(PolyWord);
}

static Handle Real_strc(TaskData *mdTaskData, Handle hDigits, Handle hMode, Handle arg);
static Handle Real_convc(TaskData *mdTaskData, Handle str);


// Positive and negative infinities and (positive) NaN.
double posInf, negInf, notANumber;

/* Real numbers are represented by the address of the value. */
#define DBLE sizeof(double)

union db { double dble; byte bytes[DBLE]; };

/* Assumes that there will be a separate handler for floating point overflow
   and underflow. */

/* tidied up 27/10/93 SPF */
/*  can't assume (double *) has same alignment as (PolyWord *), so all the */
/* parameters should be passed in as a Handle, not (double **).        */

double real_arg(Handle x)
{
    union db r_arg_x;
    for (unsigned i = 0; i < DBLE; i++)
    {
        r_arg_x.bytes[i] = DEREFBYTEHANDLE(x)[i];
    }
    return r_arg_x.dble;
}

Handle real_result(TaskData *mdTaskData, double x)
{
    union db argx;
    
    argx.dble = x;
    
    PolyObject *v = alloc(mdTaskData, DBLE/sizeof(PolyWord), F_BYTE_OBJ);
    /* Copy as words in case the alignment is wrong. */
    for(unsigned i = 0; i < DBLE; i++)
    {
        v->AsBytePtr()[i] = argx.bytes[i];
    }
    return mdTaskData->saveVec.push(v);
}

POLYEXTERNALSYMBOL double PolyFloatArbitraryPrecision(PolyWord arg)
{
    return get_arbitrary_precision_as_real(arg);
}

// Convert a boxed real to a long precision int.
POLYUNSIGNED PolyRealBoxedToLongInt(PolyObject *threadId, PolyWord arg)
{
    TaskData *taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    Handle reset = taskData->saveVec.mark();
    Handle pushedArg = taskData->saveVec.push(arg);

    double dx = real_arg(pushedArg);
    int64_t i = (int64_t)dx;
    Handle result = Make_arbitrary_precision(taskData, i);

    taskData->saveVec.reset(reset);
    taskData->PostRTSCall();
    if (result == 0) return TAGGED(0).AsUnsigned();
    else return result->Word().AsUnsigned();
}

// RTS call for square-root.
double PolyRealSqrt(double arg)
{
    return sqrt(arg);
}

// RTS call for sine.
double PolyRealSin(double arg)
{
    return sin(arg);
}

// RTS call for cosine.
double PolyRealCos(double arg)
{
    return cos(arg);
}

// RTS call for arctan.
double PolyRealArctan(double arg)
{
    return atan(arg);
}

// RTS call for exp.
double PolyRealExp(double arg)
{
    return exp(arg);
}

// RTS call for ln.
double PolyRealLog(double arg)
{
    // Make sure the result conforms to the definition.
    // If the argument is a Nan each of the first two tests will fail.
    if (arg > 0.0)
        return log(arg);
    else if (arg == 0.0) // x may be +0.0 or -0.0
        return negInf; // -infinity.
    else return notANumber;
}

// These were handled by the general dispatch function
double PolyRealTan(double arg)
{
    return tan(arg);
}

double PolyRealArcSin(double arg)
{
    if (arg >= -1.0 && arg <= 1.0)
        return asin(arg);
    else return notANumber;
}

double PolyRealArcCos(double arg)
{
    if (arg >= -1.0 && arg <= 1.0)
        return acos(arg);
    else return notANumber;
}

double PolyRealLog10(double arg)
{
    // Make sure the result conforms to the definition.
    // If the argument is a Nan each of the first two tests will fail.
    if (arg > 0.0)
        return log10(arg);
    else if (arg == 0.0) // x may be +0.0 or -0.0
        return negInf; // -infinity.
    else return notANumber;
}

double PolyRealSinh(double arg)
{
    return sinh(arg);
}

double PolyRealCosh(double arg)
{
    return cosh(arg);
}

double PolyRealTanh(double arg)
{
    return tanh(arg);
}

double PolyRealFloor(double arg)
{
    return floor(arg);
}

double PolyRealCeil(double arg)
{
    return ceil(arg);
}

double PolyRealTrunc(double arg)
{
    // Truncate towards zero
    if (arg >= 0.0) return floor(arg);
    else return ceil(arg);
}

double PolyRealRound(double arg)
{
    // Round to nearest integral value.
    double drem = fmod(arg, 2.0);
    if (drem == 0.5 || drem == -1.5)
        // If the value was exactly positive even + 0.5 or
        // negative odd -0.5 round it down, otherwise round it up. 
        return ceil(arg-0.5);
    else return floor(arg+0.5);

}

/* CALL_IO1(Real_conv, REF, NOIND) */
Handle Real_convc(TaskData *mdTaskData, Handle str) /* string to real */
{
    double result;
    int i;
    char *finish;
    TempCString string_buffer(Poly_string_to_C_alloc(DEREFHANDLE(str)));
    
    /* Scan the string turning '~' into '-' */
    for(i = 0; string_buffer[i] != '\0'; i ++)
    {
        if (string_buffer[i] == '~') string_buffer[i] = '-';
    }
        
    /* Now convert it */
#ifdef HAVE_STRTOD
    result = strtod(string_buffer, &finish);
#else
    result = poly_strtod(string_buffer, &finish);
#endif
    // We no longer detect overflow and underflow and instead return
    // (signed) zeros for underflow and (signed) infinities for overflow.
    if (*finish != '\0') raise_exception_string(mdTaskData, EXC_conversion, "");

    return real_result(mdTaskData, result);
}/* Real_conv */

// Convert a string to a boxed real.  This should really return an unboxed real.
POLYUNSIGNED PolyRealBoxedFromString(PolyObject *threadId, PolyWord str)
{
    TaskData *taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    Handle reset = taskData->saveVec.mark();
    Handle pushedString = taskData->saveVec.push(str);
    Handle result = 0;

    try {
        result = Real_convc(taskData, pushedString);
    } catch (...) { } // If an ML exception is raised

    taskData->saveVec.reset(reset);
    taskData->PostRTSCall();
    if (result == 0) return TAGGED(0).AsUnsigned();
    else return result->Word().AsUnsigned();
}

static double real_arg1(Handle x)
{
    union db r_arg_x;
    for(unsigned i = 0; i < DBLE; i++)
    {
        r_arg_x.bytes[i] = DEREFHANDLE(x)->Get(0).AsObjPtr()->AsBytePtr()[i];
    }
    return r_arg_x.dble;
}

static double real_arg2(Handle x)
{
    union db r_arg_x;
    for(unsigned i = 0; i < DBLE; i++)
    {
        r_arg_x.bytes[i] = DEREFHANDLE(x)->Get(1).AsObjPtr()->AsBytePtr()[i];
    }
    return r_arg_x.dble;
}

static Handle powerOf(TaskData *mdTaskData, Handle args)
{
    double x = real_arg1(args), y = real_arg2(args);
    /* Some of the special cases are defined and don't seem to match
       the C pow function (at least as implemented in MS C). */
    /* Maybe handle all this in ML? */
    if (isnan(x))
    {
        if (y == 0.0) return real_result(mdTaskData, 1.0);
        else return real_result(mdTaskData, notANumber);
    }
    else if (isnan(y)) return real_result(mdTaskData, y); /* i.e. nan. */
    else if (x == 0.0 && y < 0.0)
    {
        /* This case is not handled correctly in Solaris. It always
           returns -infinity. */
        int iy = (int)floor(y);
        /* If x is -0.0 and y is an odd integer the result is -infinity. */
        if (copysign(1.0, x) < 0.0 && (double)iy == y && (iy & 1))
            return real_result(mdTaskData, negInf); /* -infinity. */
        else return real_result(mdTaskData, posInf); /* +infinity. */
    }
    return real_result(mdTaskData, pow(x, y));
}

#define POLY_ROUND_TONEAREST    0
#define POLY_ROUND_DOWNWARD     1
#define POLY_ROUND_UPWARD       2
#define POLY_ROUND_TOZERO       3
#define POLY_ROUND_ERROR        -1

#if defined(__SOFTFP__)
// soft-float lacks proper rounding mode support
// While some systems will support fegetround/fesetround, it will have no
// effect on the actual rounding performed, as the software implementation only
// ever rounds to nearest.
static int getrounding()
{
    return POLY_ROUND_TONEAREST;
}

static int setrounding(int rounding)
{
    switch (rounding)
    {
    case POLY_ROUND_TONEAREST: return 0; // The only mode supported
    }
    return -1; // Error - unsupported
}

// It would be nice to be able to use autoconf to test for these as functions
// but they are frequently inlined 
#elif defined(HAVE_FENV_H)
// C99 version.  This is becoming the most common.
static int getrounding()
{
    switch (fegetround())
    {
    case FE_TONEAREST: return POLY_ROUND_TONEAREST;
#ifndef HOSTARCHITECTURE_SH
    case FE_DOWNWARD: return POLY_ROUND_DOWNWARD;
    case FE_UPWARD: return POLY_ROUND_UPWARD;
#endif
    case FE_TOWARDZERO: return POLY_ROUND_TOZERO;
    }
    return POLY_ROUND_ERROR;
}

static int setrounding(int rounding)
{
    switch (rounding)
    {
    case POLY_ROUND_TONEAREST: fesetround(FE_TONEAREST); return 0; // Choose nearest
#ifndef HOSTARCHITECTURE_SH
    case POLY_ROUND_DOWNWARD: fesetround(FE_DOWNWARD); return 0; // Towards negative infinity
    case POLY_ROUND_UPWARD: fesetround(FE_UPWARD); return 0; // Towards positive infinity
#endif
    case POLY_ROUND_TOZERO: fesetround(FE_TOWARDZERO); return 0; // Truncate towards zero
    default: return -1;
    }
}

#elif (defined(HAVE_IEEEFP_H) && ! defined(__CYGWIN__))
// Older FreeBSD.  Cygwin has the ieeefp.h header but not the functions!
static int getrounding()
{
    switch (fpgetround())
    {
    case FP_RN: return POLY_ROUND_TONEAREST;
    case FP_RM: return POLY_ROUND_DOWNWARD;
    case FP_RP: return POLY_ROUND_UPWARD;
    case FP_RZ: return POLY_ROUND_TOZERO;
    default: return POLY_ROUND_ERROR; /* Shouldn't happen. */ 
    }
}

static int setrounding(int rounding)
{
    switch (rounding)
    {
    case POLY_ROUND_TONEAREST: fpsetround(FP_RN); break; /* Choose nearest */
    case POLY_ROUND_DOWNWARD: fpsetround(FP_RM); break; /* Towards negative infinity */
    case POLY_ROUND_UPWARD: fpsetround(FP_RP); break; /* Towards positive infinity */
    case POLY_ROUND_TOZERO: fpsetround(FP_RZ); break; /* Truncate towards zero */
    }
    return 0
}

#elif defined(_WIN32)
// Windows version
static int getrounding()
{
    switch (_controlfp(0,0) & _MCW_RC)
    {
    case _RC_NEAR: return POLY_ROUND_TONEAREST;
    case _RC_DOWN: return POLY_ROUND_DOWNWARD;
    case _RC_UP: return POLY_ROUND_UPWARD;
    case _RC_CHOP: return POLY_ROUND_TOZERO;
    }
    return POLY_ROUND_ERROR;
}

static int setrounding(int rounding)
{
    switch (rounding)
    {
    case POLY_ROUND_TONEAREST: _controlfp(_RC_NEAR, _MCW_RC); return 0; // Choose nearest
    case POLY_ROUND_DOWNWARD: _controlfp(_RC_DOWN, _MCW_RC); return 0; // Towards negative infinity
    case POLY_ROUND_UPWARD: _controlfp(_RC_UP, _MCW_RC); return 0; // Towards positive infinity
    case POLY_ROUND_TOZERO: _controlfp(_RC_CHOP, _MCW_RC); return 0; // Truncate towards zero
    }
    return -1;
}

#elif defined(_FPU_GETCW) && defined(_FPU_SETCW)
// Older Linux version
static int getrounding()
{
    fpu_control_t ctrl;
    _FPU_GETCW(ctrl);
    switch (ctrl & _FPU_RC_ZERO)
    {
    case _FPU_RC_NEAREST: return POLY_ROUND_TONEAREST;
    case _FPU_RC_DOWN: return POLY_ROUND_DOWNWARD;
    case _FPU_RC_UP: return POLY_ROUND_UPWARD;
    case _FPU_RC_ZERO: return POLY_ROUND_TOZERO;
    }
    return POLY_ROUND_ERROR; /* Never reached but this avoids warning message. */
}

static int setrounding(int rounding)
{
    fpu_control_t ctrl;
    _FPU_GETCW(ctrl);
    ctrl &= ~_FPU_RC_ZERO; /* Mask off any existing rounding. */
    switch (rounding)
    {
    case POLY_ROUND_TONEAREST: ctrl |= _FPU_RC_NEAREST;
    case POLY_ROUND_DOWNWARD: ctrl |= _FPU_RC_DOWN;
    case POLY_ROUND_UPWARD: ctrl |= _FPU_RC_UP;
    case POLY_ROUND_TOZERO: ctrl |= _FPU_RC_ZERO;
    }
    _FPU_SETCW(ctrl);
    return 0;
}
#else
// Give up.
static int getrounding()
{
    return POLY_ROUND_ERROR;
}

static int setrounding()
{
    return -1;
}
#endif

POLYSIGNED PolyGetRoundingMode(PolyWord)
{
    // Get the rounding and turn the result into a tagged integer.
    return TAGGED(getrounding()).AsSigned();
}

POLYSIGNED PolySetRoundingMode(PolyWord arg)
{
    return TAGGED(setrounding((int)arg.UnTagged())).AsSigned();
}

Handle Real_strc(TaskData *mdTaskData, Handle hDigits, Handle hMode, Handle arg)
{
    double  dx = real_arg(arg);
    int     decpt, sign;
    int     mode = get_C_int(mdTaskData, DEREFWORDHANDLE(hMode));
    int     digits = get_C_int(mdTaskData, DEREFWORDHANDLE(hDigits));
    /* Compute the shortest string which gives the required value. */
    /*  */
    char *chars = poly_dtoa(dx, mode, digits, &decpt, &sign, NULL);
    /* We have to be careful in case an allocation causes a
       garbage collection. */
    PolyWord pStr = C_string_to_Poly(mdTaskData, chars);
    poly_freedtoa(chars);
    Handle ppStr = mdTaskData->saveVec.push(pStr);
    /* Allocate a triple for the results. */
    PolyObject *result = alloc(mdTaskData, 3);
    result->Set(0, DEREFWORDHANDLE(ppStr));
    result->Set(1, TAGGED(decpt));
    result->Set(2, TAGGED(sign));
    return mdTaskData->saveVec.push(result);
}

// Convert boxed real to string.  This should be changed to use an unboxed real argument.
POLYUNSIGNED PolyRealBoxedToString(PolyObject *threadId, PolyWord arg, PolyWord mode, PolyWord digits)
{
    TaskData *taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    Handle reset = taskData->saveVec.mark();
    Handle pushedArg = taskData->saveVec.push(arg);
    Handle pushedMode = taskData->saveVec.push(mode);
    Handle pushedDigits = taskData->saveVec.push(digits);
    Handle result = 0;

    try {
        result = Real_strc(taskData, pushedDigits, pushedMode, pushedArg);
    } catch (...) { } // Can this raise an exception?

    taskData->saveVec.reset(reset);
    taskData->PostRTSCall();
    if (result == 0) return TAGGED(0).AsUnsigned();
    else return result->Word().AsUnsigned();
}

/* Functions added for Standard Basis Library are all indirected through here. */
static Handle Real_dispatchc(TaskData *mdTaskData, Handle args, Handle code)
{
    unsigned c = get_C_unsigned(mdTaskData, DEREFWORDHANDLE(code));
    switch (c)
    {
    case 3: /* atan2 */ return real_result(mdTaskData, atan2(real_arg1(args), real_arg2(args)));
    case 4: /* pow */ return powerOf(mdTaskData, args);
    /* Floating point representation queries. */
#ifdef _DBL_RADIX
    case 11: /* Value of radix */ return mdTaskData->saveVec.push(TAGGED(_DBL_RADIX));
#else
    case 11: /* Value of radix */ return mdTaskData->saveVec.push(TAGGED(FLT_RADIX));
#endif
    case 12: /* Value of precision */ return mdTaskData->saveVec.push(TAGGED(DBL_MANT_DIG));
    case 13: /* Maximum number */ return real_result(mdTaskData, DBL_MAX);
    /* float.h describes DBL_MIN as the minimum positive number.
       In fact this is the minimum NORMALISED number.  The smallest
       number which can be represented is DBL_MIN*2**(-DBL_MANT_DIG) */
    case 14: /* Minimum normalised number. */
        return real_result(mdTaskData, DBL_MIN);
    case 17: /* Get sign bit.  There may be better ways to find this. */
        return mdTaskData->saveVec.push(copysign(1.0, real_arg(args)) < 0.0 ? TAGGED(1) : TAGGED(0));
    case 18: /* Copy sign. */
        return real_result(mdTaskData, copysign(real_arg1(args), real_arg2(args)));
    case 23: /* Compute ldexp */
        {
            int exp = get_C_int(mdTaskData, DEREFHANDLE(args)->Get(1));
            return real_result(mdTaskData, ldexp(real_arg1(args), exp));
        }
    case 24: /* Get mantissa. */
        {
            int exp;
            return real_result(mdTaskData, frexp(real_arg(args), &exp));
        }
    case 25: /* Get exponent. */
        {
            int exp;
            (void)frexp(real_arg(args), &exp);
            return mdTaskData->saveVec.push(TAGGED(exp));
        }

    default:
        {
            char msg[100];
            sprintf(msg, "Unknown real arithmetic function: %d", c);
            raise_exception_string(mdTaskData, EXC_Fail, msg);
            return 0;
        }
    }
}

POLYUNSIGNED PolyRealSize(PolyWord)
{
    // Return the number of bytes for a real.  This is used in PackRealBig/Little.
    return TAGGED(sizeof(double)).AsUnsigned();
}

POLYUNSIGNED PolyRealGeneral(PolyObject *threadId, PolyWord code, PolyWord arg)
{
    TaskData *taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    Handle reset = taskData->saveVec.mark();
    Handle pushedCode = taskData->saveVec.push(code);
    Handle pushedArg = taskData->saveVec.push(arg);
    Handle result = 0;

    try {
        result = Real_dispatchc(taskData, pushedArg, pushedCode);
    } catch (...) { } // If an ML exception is raised

    taskData->saveVec.reset(reset);
    taskData->PostRTSCall();
    if (result == 0) return TAGGED(0).AsUnsigned();
    else return result->Word().AsUnsigned();
}

struct _entrypts realsEPT[] =
{
    { "PolyRealBoxedToString",          (polyRTSFunction)&PolyRealBoxedToString},
    { "PolyRealGeneral",                (polyRTSFunction)&PolyRealGeneral},
    { "PolyRealBoxedFromString",        (polyRTSFunction)&PolyRealBoxedFromString},
    { "PolyRealBoxedToLongInt",         (polyRTSFunction)&PolyRealBoxedToLongInt},
    { "PolyRealSqrt",                   (polyRTSFunction)&PolyRealSqrt},
    { "PolyRealSin",                    (polyRTSFunction)&PolyRealSin},
    { "PolyRealCos",                    (polyRTSFunction)&PolyRealCos},
    { "PolyRealArctan",                 (polyRTSFunction)&PolyRealArctan},
    { "PolyRealExp",                    (polyRTSFunction)&PolyRealExp},
    { "PolyRealLog",                    (polyRTSFunction)&PolyRealLog},
    { "PolyRealTan",                    (polyRTSFunction)&PolyRealTan},
    { "PolyRealArcSin",                 (polyRTSFunction)&PolyRealArcSin},
    { "PolyRealArcCos",                 (polyRTSFunction)&PolyRealArcCos},
    { "PolyRealLog10",                  (polyRTSFunction)&PolyRealLog10},
    { "PolyRealSinh",                   (polyRTSFunction)&PolyRealSinh},
    { "PolyRealCosh",                   (polyRTSFunction)&PolyRealCosh},
    { "PolyRealTanh",                   (polyRTSFunction)&PolyRealTanh},
    { "PolyRealFloor",                  (polyRTSFunction)&PolyRealFloor},
    { "PolyRealCeil",                   (polyRTSFunction)&PolyRealCeil},
    { "PolyRealTrunc",                  (polyRTSFunction)&PolyRealTrunc},
    { "PolyRealRound",                  (polyRTSFunction)&PolyRealRound},
    { "PolyFloatArbitraryPrecision",    (polyRTSFunction)&PolyFloatArbitraryPrecision},
    { "PolyGetRoundingMode",            (polyRTSFunction)&PolyGetRoundingMode},
    { "PolySetRoundingMode",            (polyRTSFunction)&PolySetRoundingMode},
    { "PolyRealSize",                   (polyRTSFunction)&PolyRealSize},

    { NULL, NULL} // End of list.
};

class RealArithmetic: public RtsModule
{
public:
    virtual void Init(void);
};

// Declare this.  It will be automatically added to the table.
static RealArithmetic realModule;

void RealArithmetic::Init(void)
{
    /* Some compilers object to overflow in constants so
       we compute the values here. */
#if (HAVE_DECL_FPSETMASK && ! defined(__CYGWIN__))
    /* In FreeBSD 3.4 at least, we sometimes get floating point
       exceptions if we don't clear the mask.  Maybe need to do
       this on other platforms as well just to be sure. */
    // N.B.  fpsetmask is defined in the headers on Cygwin but there's no function!
    fpsetmask(0);
#endif
    // NAN and INFINITY are defined in GCC but not in Visual C++.
#if (defined(INFINITY))
    posInf = INFINITY;
    negInf = -(INFINITY);
#else
    {
        double zero = 0.0;
        posInf = 1.0 / zero;
        negInf = -1.0 / zero;
    }
#endif
#if (defined(NAN))
    notANumber = NAN;
#else
    {
        double zero = 0.0;
        notANumber = zero / zero;
    }
#endif
    // Make sure this is a positive NaN since we return it from "abs".
    // "Positive" in this context is copysign(1.0, x) > 0.0 because that's
    // how we test the sign so we test it first and then try to change the
    // sign if it's wrong.
    if (copysign(1.0, notANumber) < 0)
        notANumber = copysign(notANumber, 1.0);
}
