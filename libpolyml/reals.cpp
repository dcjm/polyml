/*
    Title:  Real number package.
    Author:     Dave Matthews, Cambridge University Computer Laboratory

    Copyright (c) 2000
        Cambridge University Technical Services Limited

    Further work copyright David C.J. Matthews 2011, 2016-19, 2023

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

#include <cmath> // Currently just for isnan.

#include "globals.h"
#include "run_time.h"
#include "reals.h"
#include "arb.h"
#include "sys.h"
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
*/

extern "C" {
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyRealDoubleToString(POLYUNSIGNED threadId, POLYUNSIGNED arg, POLYUNSIGNED kind, POLYUNSIGNED prec);
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
    POLYEXTERNALSYMBOL double PolyRealRem(double arg1, double arg2);
    POLYEXTERNALSYMBOL double PolyFloatArbitraryPrecision(POLYUNSIGNED arg);
    POLYEXTERNALSYMBOL POLYSIGNED PolyGetRoundingMode(POLYUNSIGNED);
    POLYEXTERNALSYMBOL POLYSIGNED PolySetRoundingMode(POLYUNSIGNED);
    POLYEXTERNALSYMBOL double PolyRealAtan2(double arg1, double arg2);
    POLYEXTERNALSYMBOL double PolyRealPow(double arg1, double arg2);
    POLYEXTERNALSYMBOL double PolyRealCopySign(double arg1, double arg2);
    POLYEXTERNALSYMBOL double PolyRealNextAfter(double arg1, double arg2);
    POLYEXTERNALSYMBOL double PolyRealLdexp(double arg1, POLYUNSIGNED arg2);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyRealFrexp(POLYUNSIGNED threadId, POLYUNSIGNED arg);
    POLYEXTERNALSYMBOL float PolyRealFSqrt(float arg);
    POLYEXTERNALSYMBOL float PolyRealFSin(float arg);
    POLYEXTERNALSYMBOL float PolyRealFCos(float arg);
    POLYEXTERNALSYMBOL float PolyRealFArctan(float arg);
    POLYEXTERNALSYMBOL float PolyRealFExp(float arg);
    POLYEXTERNALSYMBOL float PolyRealFLog(float arg);
    POLYEXTERNALSYMBOL float PolyRealFTan(float arg);
    POLYEXTERNALSYMBOL float PolyRealFArcSin(float arg);
    POLYEXTERNALSYMBOL float PolyRealFArcCos(float arg);
    POLYEXTERNALSYMBOL float PolyRealFLog10(float arg);
    POLYEXTERNALSYMBOL float PolyRealFSinh(float arg);
    POLYEXTERNALSYMBOL float PolyRealFCosh(float arg);
    POLYEXTERNALSYMBOL float PolyRealFTanh(float arg);
    POLYEXTERNALSYMBOL float PolyRealFAtan2(float arg1, float arg2);
    POLYEXTERNALSYMBOL float PolyRealFPow(float arg1, float arg2);
    POLYEXTERNALSYMBOL float PolyRealFCopySign(float arg1, float arg2);
    POLYEXTERNALSYMBOL float PolyRealFFloor(float arg);
    POLYEXTERNALSYMBOL float PolyRealFCeil(float arg);
    POLYEXTERNALSYMBOL float PolyRealFTrunc(float arg);
    POLYEXTERNALSYMBOL float PolyRealFRound(float arg);
    POLYEXTERNALSYMBOL float PolyRealFRem(float arg1, float arg2);
    POLYEXTERNALSYMBOL float PolyRealFNextAfter(float arg1, float arg2);
}

// Positive and negative infinities and (positive) NaN.
double posInf, negInf, notANumber;
float posInfF, negInfF, notANumberF;

/* Real numbers are represented by the address of the value. */
#define DBLE sizeof(double)/sizeof(POLYUNSIGNED)

union db { double dble; POLYUNSIGNED words[DBLE]; };

double real_arg(Handle x)
{
    union db r_arg_x;
    for (unsigned i = 0; i < DBLE; i++)
    {
        r_arg_x.words[i] = x->WordP()->Get(i).AsUnsigned();
    }
    return r_arg_x.dble;
}

Handle real_result(TaskData *mdTaskData, double x)
{
    union db argx;
    
    argx.dble = x;
    
    PolyObject *v = alloc(mdTaskData, DBLE, F_BYTE_OBJ);
    /* Copy as words in case the alignment is wrong. */
    for(unsigned i = 0; i < DBLE; i++)
    {
        v->Set(i, PolyWord::FromUnsigned(argx.words[i]));
    }
    return mdTaskData->saveVec.push(v);
}

// We're using float for Real32 so it needs to be 32-bits.
// Assume that's true for the moment.
#if (SIZEOF_FLOAT != 4)
#error "Float is not 32-bits.  Please report this"
#endif

union flt { float fl; int32_t i; };

#if (SIZEOF_FLOAT < SIZEOF_POLYWORD)

// Typically for 64-bit mode.  Use a tagged representation.
// The code-generator on the X86/64 assumes the float is in the
// high order word.
#define FLT_SHIFT ((SIZEOF_POLYWORD-SIZEOF_FLOAT)*8)
float float_arg(Handle x)
{
    union flt argx;
    argx.i = x->Word().AsSigned() >> FLT_SHIFT;
    return argx.fl;
}

Handle float_result(TaskData *mdTaskData, float x)
{
    union flt argx;
    argx.fl = x;
    return mdTaskData->saveVec.push(PolyWord::FromSigned(((POLYSIGNED)argx.i << FLT_SHIFT) + 1));
}
#else
// Typically for 32-bit mode.  Use a boxed representation.
float float_arg(Handle x)
{
    union flt argx;
    argx.i = (int32_t)x->WordP()->Get(0).AsSigned();
    return argx.fl;
}

Handle float_result(TaskData *mdTaskData, float x)
{
    union flt argx;
    argx.fl = x;

    PolyObject *v = alloc(mdTaskData, 1, F_BYTE_OBJ);
    v->Set(0, PolyWord::FromSigned(argx.i));
    return mdTaskData->saveVec.push(v);
}

#endif

POLYEXTERNALSYMBOL double PolyFloatArbitraryPrecision(POLYUNSIGNED arg)
{
    return get_arbitrary_precision_as_real(PolyWord::FromUnsigned(arg));
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

double PolyRealRem(double arg1, double arg2)
{
    return fmod(arg1, arg2);
}

double PolyRealAtan2(double arg1, double arg2)
{
    return atan2(arg1, arg2);
}

double PolyRealPow(double x, double y)
{
    /* Some of the special cases are defined and don't seem to match
    the C pow function (at least as implemented in MS C). */
    /* Maybe handle all this in ML? */
    if (std::isnan(x))
    {
        if (y == 0.0) return 1.0;
        else return notANumber;
    }
    else if (std::isnan(y)) return y; /* i.e. nan. */
    else if (x == 0.0 && y < 0.0)
    {
        /* This case is not handled correctly in Solaris. It always
        returns -infinity. */
        int iy = (int)floor(y);
        /* If x is -0.0 and y is an odd integer the result is -infinity. */
        if (copysign(1.0, x) < 0.0 && (double)iy == y && (iy & 1))
            return negInf; /* -infinity. */
        else return posInf; /* +infinity. */
    }
    return pow(x, y);
}

double PolyRealCopySign(double arg1, double arg2)
{
    return copysign(arg1, arg2);
}

double PolyRealNextAfter(double arg1, double arg2)
{
    return nextafter(arg1, arg2);
}

double PolyRealLdexp(double arg1, POLYUNSIGNED arg2)
{
    POLYSIGNED exponent = PolyWord::FromUnsigned(arg2).UnTagged();
#if (SIZEOF_POLYWORD > SIZEOF_INT)
    // We've already checked for arbitrary precision values where necessary and
    // for zero and non-finite mantissa.  Check the exponent fits in an int.
    if (exponent > 2 * DBL_MAX_EXP) return copysign(INFINITY, arg1);
    if (exponent < -2 * DBL_MAX_EXP) return copysign(0.0, arg1);
#endif
    return ldexp(arg1, (int)exponent);
}

// Return the normalised fraction and the exponent.
POLYUNSIGNED PolyRealFrexp(POLYUNSIGNED threadId, POLYUNSIGNED arg)
{
    TaskData *taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    Handle reset = taskData->saveVec.mark();
    Handle pushedArg = taskData->saveVec.push(arg);
    Handle result = 0;

    try {
        int exp = 0; // The value of exp is not always defined.
        Handle mantH = real_result(taskData, frexp(real_arg(pushedArg), &exp));
        // Allocate a pair for the result
        result = alloc_and_save(taskData, 2);

        result->WordP()->Set(0, TAGGED(exp));
        result->WordP()->Set(1, mantH->Word());
    }
    catch (...) {} // If an ML exception is raised

    taskData->saveVec.reset(reset);
    taskData->PostRTSCall();
    if (result == 0) return TAGGED(0).AsUnsigned();
    else return result->Word().AsUnsigned();
}

// RTS call for square-root.
float PolyRealFSqrt(float arg)
{
    return sqrtf(arg);
}

// RTS call for sine.
float PolyRealFSin(float arg)
{
    return sinf(arg);
}

// RTS call for cosine.
float PolyRealFCos(float arg)
{
    return cosf(arg);
}

// RTS call for arctan.
float PolyRealFArctan(float arg)
{
    return atanf(arg);
}

// RTS call for exp.
float PolyRealFExp(float arg)
{
    return expf(arg);
}

// RTS call for ln.
float PolyRealFLog(float arg)
{
    // Make sure the result conforms to the definition.
    // If the argument is a Nan each of the first two tests will fail.
    if (arg > 0.0)
        return logf(arg);
    else if (arg == 0.0) // x may be +0.0 or -0.0
        return negInfF; // -infinity.
    else return notANumberF;
}

float PolyRealFTan(float arg)
{
    return tanf(arg);
}

float PolyRealFArcSin(float arg)
{
    if (arg >= -1.0 && arg <= 1.0)
        return asinf(arg);
    else return notANumberF;
}

float PolyRealFArcCos(float arg)
{
    if (arg >= -1.0 && arg <= 1.0)
        return acosf(arg);
    else return notANumberF;
}

float PolyRealFLog10(float arg)
{
    // Make sure the result conforms to the definition.
    // If the argument is a Nan each of the first two tests will fail.
    if (arg > 0.0)
        return log10f(arg);
    else if (arg == 0.0) // x may be +0.0 or -0.0
        return negInfF; // -infinity.
    else return notANumberF;
}

float PolyRealFSinh(float arg)
{
    return sinhf(arg);
}

float PolyRealFCosh(float arg)
{
    return coshf(arg);
}

float PolyRealFTanh(float arg)
{
    return tanhf(arg);
}

float PolyRealFAtan2(float arg1, float arg2)
{
    return atan2f(arg1, arg2);
}

float PolyRealFPow(float x, float y)
{
    /* Some of the special cases are defined and don't seem to match
    the C pow function (at least as implemented in MS C). */
    /* Maybe handle all this in ML? */
    if (std::isnan(x))
    {
        if (y == 0.0) return 1.0;
        else return notANumberF;
    }
    else if (std::isnan(y)) return y; /* i.e. nan. */
    else if (x == 0.0 && y < 0.0)
    {
        /* This case is not handled correctly in Solaris. It always
        returns -infinity. */
        int iy = (int)floorf(y);
        /* If x is -0.0 and y is an odd integer the result is -infinity. */
        if (copysign(1.0, x) < 0.0 && (float)iy == y && (iy & 1))
            return negInfF; /* -infinity. */
        else return posInfF; /* +infinity. */
    }
    return powf(x, y);
}

float PolyRealFFloor(float arg)
{
    return floorf(arg);
}

float PolyRealFCeil(float arg)
{
    return ceilf(arg);
}

float PolyRealFTrunc(float arg)
{
    // Truncate towards zero
    if (arg >= 0.0) return floorf(arg);
    else return ceilf(arg);
}

float PolyRealFRound(float arg)
{
    // Round to nearest integral value.
    float drem = fmodf(arg, 2.0);
    if (drem == 0.5 || drem == -1.5)
        // If the value was exactly positive even + 0.5 or
        // negative odd -0.5 round it down, otherwise round it up. 
        return ceilf(arg - 0.5f);
    else return floorf(arg + 0.5f);
}

float PolyRealFRem(float arg1, float arg2)
{
    return fmodf(arg1, arg2);
}

float PolyRealFCopySign(float arg1, float arg2)
{
    return copysignf(arg1, arg2);
}

float PolyRealFNextAfter(float arg1, float arg2)
{
    return nextafterf(arg1, arg2);
}

// Format a double-precision number using the format specifier.
POLYUNSIGNED PolyRealDoubleToString(POLYUNSIGNED threadId, POLYUNSIGNED arg, POLYUNSIGNED kind, POLYUNSIGNED prec)
{
    TaskData* taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    Handle reset = taskData->saveVec.mark();
    Handle pushedArg = taskData->saveVec.push(arg);
    Handle pushedPrec = taskData->saveVec.push(prec);
    Handle result = 0;
    const char* format;
    switch (UNTAGGED(PolyWord::FromUnsigned(kind)))
    {
    case 'e': case 'E': format = "%1.*E"; break;
    case 'f': case 'F': format = "%1.*F"; break;
    default: format = "%1.*G"; break;
    }

    try {
        int sSize = 100;
        do {
            TempCString buffer((char*)malloc(sSize));
            if ((char*)buffer == NULL)
                raise_fail(taskData, "Insufficient memory for string");
            int precision = get_C_int(taskData, pushedPrec->Word());
            int retSize = snprintf((char* const)buffer, sSize, format, precision, real_arg(pushedArg));
            if (retSize < 0)
                raise_fail(taskData, "Conversion error");
            if (retSize <= sSize)
            {
                char* p = buffer;
                char* q = buffer;
                // Replace - by ~ and remove any + signs. Suppress leading zeros after E.
                bool suppressZero = false;
                while (*p)
                {
                    char c = *p++;
                    switch (c)
                    {
                    case '-': *q++ = '~'; break;
                    case '+': break;
                    case 'e': case 'E': *q++ = 'E'; suppressZero = true; break;
                    case '0': if (!suppressZero) *q++ = '0'; break;
                    default: *q++ = c; suppressZero = false; break;
                    }
                }
                if (suppressZero) *q++ = '0'; // We need at least one digit
                *q = 0;
                result = taskData->saveVec.push(C_string_to_Poly(taskData, buffer));
                break;
            }
            sSize *= 2;
        } while (true);
        
    }
    catch (...) {} // If an ML exception is raised

    taskData->saveVec.reset(reset);
    taskData->PostRTSCall();
    if (result == 0) return TAGGED(0).AsUnsigned();
    else return result->Word().AsUnsigned();
}

#if defined(__SOFTFP__)
// soft-float lacks proper rounding mode support
// While some systems will support fegetround/fesetround, it will have no
// effect on the actual rounding performed, as the software implementation only
// ever rounds to nearest.
int getrounding()
{
    return POLY_ROUND_TONEAREST;
}

int setrounding(int rounding)
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
int getrounding()
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
    return POLY_ROUND_TONEAREST;
}

int setrounding(int rounding)
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
int getrounding()
{
    switch (fpgetround())
    {
    case FP_RN: return POLY_ROUND_TONEAREST;
    case FP_RM: return POLY_ROUND_DOWNWARD;
    case FP_RP: return POLY_ROUND_UPWARD;
    case FP_RZ: return POLY_ROUND_TOZERO;
    default: return POLY_ROUND_TONEAREST; /* Shouldn't happen. */ 
    }
}

int setrounding(int rounding)
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
int getrounding()
{
    switch (_controlfp(0,0) & _MCW_RC)
    {
    case _RC_NEAR: return POLY_ROUND_TONEAREST;
    case _RC_DOWN: return POLY_ROUND_DOWNWARD;
    case _RC_UP: return POLY_ROUND_UPWARD;
    case _RC_CHOP: return POLY_ROUND_TOZERO;
    }
    return POLY_ROUND_TONEAREST;
}

int setrounding(int rounding)
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
int getrounding()
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
    return POLY_ROUND_TONEAREST; /* Never reached but this avoids warning message. */
}

int setrounding(int rounding)
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
// Give up.  Assume that we only support TO_NEAREST
int getrounding()
{
    return POLY_ROUND_TONEAREST;
}

int setrounding(int rounding)
{
    if (rounding == POLY_ROUND_TONEAREST)
        return 0;
    else return -1;
}
#endif

POLYSIGNED PolyGetRoundingMode(POLYUNSIGNED)
{
    // Get the rounding and turn the result into a tagged integer.
    return TAGGED(getrounding()).AsSigned();
}

POLYSIGNED PolySetRoundingMode(POLYUNSIGNED arg)
{
    return TAGGED(setrounding((int)PolyWord::FromUnsigned(arg).UnTagged())).AsSigned();
}

struct _entrypts realsEPT[] =
{
    { "PolyRealDoubleToString",         (polyRTSFunction)&PolyRealDoubleToString},
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
    { "PolyRealRem",                    (polyRTSFunction)&PolyRealRem },
    { "PolyFloatArbitraryPrecision",    (polyRTSFunction)&PolyFloatArbitraryPrecision},
    { "PolyGetRoundingMode",            (polyRTSFunction)&PolyGetRoundingMode},
    { "PolySetRoundingMode",            (polyRTSFunction)&PolySetRoundingMode},
    { "PolyRealAtan2",                  (polyRTSFunction)&PolyRealAtan2 },
    { "PolyRealPow",                    (polyRTSFunction)&PolyRealPow },
    { "PolyRealCopySign",               (polyRTSFunction)&PolyRealCopySign },
    { "PolyRealNextAfter",              (polyRTSFunction)&PolyRealNextAfter },
    { "PolyRealLdexp",                  (polyRTSFunction)&PolyRealLdexp },
    { "PolyRealFrexp",                  (polyRTSFunction)&PolyRealFrexp },
    { "PolyRealFSqrt",                  (polyRTSFunction)&PolyRealFSqrt },
    { "PolyRealFSin",                   (polyRTSFunction)&PolyRealFSin },
    { "PolyRealFCos",                   (polyRTSFunction)&PolyRealFCos },
    { "PolyRealFArctan",                (polyRTSFunction)&PolyRealFArctan },
    { "PolyRealFExp",                   (polyRTSFunction)&PolyRealFExp },
    { "PolyRealFLog",                   (polyRTSFunction)&PolyRealFLog },
    { "PolyRealFTan",                   (polyRTSFunction)&PolyRealFTan },
    { "PolyRealFArcSin",                (polyRTSFunction)&PolyRealFArcSin },
    { "PolyRealFArcCos",                (polyRTSFunction)&PolyRealFArcCos },
    { "PolyRealFLog10",                 (polyRTSFunction)&PolyRealFLog10 },
    { "PolyRealFSinh",                  (polyRTSFunction)&PolyRealFSinh },
    { "PolyRealFCosh",                  (polyRTSFunction)&PolyRealFCosh },
    { "PolyRealFTanh",                  (polyRTSFunction)&PolyRealFTanh },
    { "PolyRealFAtan2",                 (polyRTSFunction)&PolyRealFAtan2 },
    { "PolyRealFPow",                   (polyRTSFunction)&PolyRealFPow },
    { "PolyRealFCopySign",              (polyRTSFunction)&PolyRealFCopySign },
    { "PolyRealFFloor",                 (polyRTSFunction)&PolyRealFFloor },
    { "PolyRealFCeil",                  (polyRTSFunction)&PolyRealFCeil },
    { "PolyRealFTrunc",                 (polyRTSFunction)&PolyRealFTrunc },
    { "PolyRealFRound",                 (polyRTSFunction)&PolyRealFRound },
    { "PolyRealFRem",                   (polyRTSFunction)&PolyRealFRem },
    { "PolyRealFNextAfter",             (polyRTSFunction)&PolyRealFNextAfter },

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
    posInfF = INFINITY;
    negInfF = -(INFINITY);
#else
    {
        double zero = 0.0;
        posInf = 1.0 / zero;
        negInf = -1.0 / zero;
        float zeroF = 0.0;
        posInfF = 1.0 / zeroF;
        negInfF = -1.0 / zeroF;
    }
#endif
#if (defined(NAN))
    notANumber = NAN;
#else
    {
        double zero = 0.0;
        notANumber = zero / zero;
        float zeroF = 0.0;
        notANumberF = zeroF / zeroF;
    }
#endif
    // Make sure this is a positive NaN since we return it from "abs".
    // "Positive" in this context is copysign(1.0, x) > 0.0 because that's
    // how we test the sign so we test it first and then try to change the
    // sign if it's wrong.
    if (copysign(1.0, notANumber) < 0)
        notANumber = copysign(notANumber, 1.0);
    if (copysignf(1.0, notANumberF) < 0)
        notANumberF = copysignf(notANumberF, 1.0);
}
