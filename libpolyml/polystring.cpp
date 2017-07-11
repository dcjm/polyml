/*
    Title:  polystring.cpp - String functions and types

    Copyright (c) 2006, 2015 David C.J. Matthews

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

#ifdef HAVE_STDIO_H
#include <stdio.h>
#endif

#ifdef HAVE_MALLOC_H
#include <malloc.h>
#endif

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include "globals.h"
#include "polystring.h"
#include "run_time.h"
#include "mpoly.h"
#include "sys.h"
#include "arb.h"
#include "save_vec.h"
#include "processes.h"

#define SAVE(x) mdTaskData->saveVec.push(x)
#define SIZEOF(x) (sizeof(x)/sizeof(PolyWord))

// Return empty string.
PolyWord EmptyString(void)
{
    return (PolyObject*)IoEntry(POLY_SYS_emptystring);
}

PolyWord C_string_to_Poly(TaskData *mdTaskData, const char *buffer, size_t buffLen)
/* Returns a C string as a Poly string. */
{
    if (buffer == NULL) return EmptyString();

    if (buffLen == (size_t)-1) buffLen = strlen(buffer);

    /* Get the number of words required, plus 1 for length word,
       plus flag bit. */
    PolyStringObject *result = (PolyStringObject *)(alloc(mdTaskData, WORDS(buffLen) + 1, F_BYTE_OBJ));
    
    /* Set length of string, then copy the characters. */
    result->length = buffLen;
    memcpy(result->chars,buffer,buffLen);
    return result;
} /* C_string_to_Poly */

POLYUNSIGNED Poly_string_to_C(PolyWord ps, char *buff, POLYUNSIGNED bufflen)
/* Copies the characters from the string into the destination buffer.
   Returns original length of string. */
{
    if (IS_INT(ps))
    {
        buff[0] = (char)(UNTAGGED(ps));
        buff[1] = '\0';
        return(1);
    }
    PolyStringObject *str = (PolyStringObject *)ps.AsObjPtr();
    POLYUNSIGNED chars = str->length >= bufflen ? bufflen-1 : str->length;
    if (chars != 0) strncpy(buff, str->chars, chars);
    buff[chars] = '\0';
    return chars;
} /* Poly_string_to_C */

char *Poly_string_to_C_alloc(PolyWord ps, size_t extraChars)
/* Similar to Poly_string_to_C except that the string is allocated using
   malloc and must be freed by the caller. */
{
    char    *res;

    if (IS_INT(ps))
    {
        res = (char*)malloc(2 + extraChars);
        if (res == 0) return 0;
        res[0] = (char)(UNTAGGED(ps));
        res[1] = '\0';
    }
    else
    {
        PolyStringObject * str = (PolyStringObject *)ps.AsObjPtr();
        POLYUNSIGNED chars = str->length;
        res = (char*)malloc(chars + extraChars + 1);
        if (res == 0) return 0;
        if (chars != 0) strncpy(res, str->chars, chars);
        res[chars] = '\0';
    }
    return res;
} /* Poly_string_to_C_alloc */

TempCString::~TempCString()
{
    free(m_value); 
}

#if (defined(_WIN32) && defined(UNICODE))

unsigned int codePage = CP_ACP;

bool setWindowsCodePage(const TCHAR *codePageArg)
{
    if (_tcscmp(codePageArg, _T("CP_ACP")) == 0) { codePage = CP_ACP; return true; }
    if (_tcscmp(codePageArg, _T("CP_UTF7")) == 0 || lstrcmpi(codePageArg, _T("utf7")) == 0)
         { codePage = CP_UTF7; return true; }
    if (_tcscmp(codePageArg, _T("CP_UTF8")) == 0 || lstrcmpi(codePageArg, _T("utf8")) == 0)
         { codePage = CP_UTF8; return true; }
    if (*codePageArg >= '0' && *codePageArg <= '9')
    {
        TCHAR *endp;
        codePage = _tcstol(codePageArg, &endp, 10);
        return true;
    }
    return false;
}

/* We need Unicode versions of these. */
PolyWord C_string_to_Poly(TaskData *mdTaskData, const WCHAR *buffer, size_t buffLen)
/* Returns a Unicode string as a Poly string. */
{
    if (buffer == NULL) return EmptyString();

    // Get the length of the string, without the terminating null.
    if (buffLen == -1) buffLen = wcslen(buffer);
    if (buffLen == 0) return EmptyString(); // If it's zero return empty string.

    // Find the length when converted.
    int outputLen = WideCharToMultiByte(codePage, 0, buffer, (int)buffLen, NULL, 0, NULL, NULL);

    // Return the null string if there's an error 
    if (outputLen <= 0) return EmptyString();
    
    // Return the character itself if the length is 1 */
    if (outputLen == 1)
    {
        char obuff[1];
        int check = WideCharToMultiByte(codePage, 0, buffer, (int)buffLen, obuff, 1, NULL, NULL);
        if (check <= 0) return EmptyString();
        return TAGGED(obuff[0]);
    }
    
    // Get the number of words required, plus 1 for length word, plus flag bit.
    PolyStringObject *result = (PolyStringObject *)(alloc(mdTaskData, WORDS(outputLen) + 1, F_BYTE_OBJ));
    
    // Set length of string, then copy the characters.
    result->length = outputLen;
    int check = WideCharToMultiByte(codePage, 0, buffer, (int)buffLen, result->chars, outputLen, NULL, NULL);
    if (check <= 0) return EmptyString();

    return result;
}

POLYUNSIGNED Poly_string_to_C(PolyWord ps, WCHAR *buff, POLYUNSIGNED bufflen)
{
    char iBuff[1];
    int iLength = 0;
    const char *iPtr;

    if (IS_INT(ps))
    {
        iLength = 1;
        iBuff[0] = (char)UNTAGGED(ps);
        iPtr = iBuff;
    }
    else
    {
        PolyStringObject *str = (PolyStringObject *)ps.AsObjPtr();
        iLength = (int)str->length;
        if (iLength == 0)
        {
            // Null string.
            if (bufflen != 0) buff[0] = 0;
            return 0;
        }
        iPtr = str->chars;
    }
    // We can convert it directly using the maximum string length.
    int space = MultiByteToWideChar(codePage, 0, iPtr, iLength, buff, (int)bufflen-1);

    if (space <= 0)
    {
        if (bufflen != 0) buff[0] = 0;
        return 0; // Error
    }

    buff[space] = 0; // Null terminate
    return space;
}

WCHAR *Poly_string_to_U_alloc(PolyWord ps, size_t extraChars)
{
    char iBuff[1];
    int iLength = 0;
    const char *iPtr;

    if (IS_INT(ps))
    {
        iLength = 1;
        iBuff[0] = (char)UNTAGGED(ps);
        iPtr = iBuff;
    }
    else
    {
        PolyStringObject *str = (PolyStringObject *)ps.AsObjPtr();
        iLength = (int)str->length;
        if (iLength == 0 && extraChars == 0) return _wcsdup(L"");
        iPtr = str->chars;
    }

    // Find the space required.
    int chars = MultiByteToWideChar(codePage, 0, iPtr, iLength, NULL, 0);
    if (chars <= 0) return _wcsdup(L"");
    WCHAR *res = (WCHAR*)malloc((chars+1+extraChars) * sizeof(WCHAR));
    if (res == 0) return 0;
    chars = MultiByteToWideChar(codePage, 0, iPtr, iLength, res, chars);
    res[chars] = 0;
    return res;
}

// convert_string_list return a list of strings.
// This converts Unicode strings.
Handle convert_string_list(TaskData *mdTaskData, int count, WCHAR **strings)
{
    Handle saved = mdTaskData->saveVec.mark();
    Handle list  = SAVE(ListNull);
    
    // It's simplest to process the strings in reverse order */
    for (int i = count - 1; 0 <= i; i--)
    {
        Handle value = SAVE(C_string_to_Poly(mdTaskData, strings[i]));
        Handle next  = alloc_and_save(mdTaskData, SIZEOF(ML_Cons_Cell));
        
        DEREFLISTHANDLE(next)->h = DEREFWORDHANDLE(value); 
        DEREFLISTHANDLE(next)->t = DEREFLISTHANDLE(list);
        
        // reset save vector to stop it overflowing    
        mdTaskData->saveVec.reset(saved);
        list = SAVE(DEREFHANDLE(next));
    }
    
    return list;
}

TempString::~TempString()
{
    free(m_value); 
}

#endif

/* convert_string_list return a list of strings. */
Handle convert_string_list(TaskData *mdTaskData, int count, char **strings)
{
    Handle saved = mdTaskData->saveVec.mark();
    Handle list  = SAVE(ListNull);
    
    // It's simplest to process the strings in reverse order */
    for (int i = count - 1; 0 <= i; i--)
    {
        /* 
        The order of these declarations is important, becaue we don't
        want to have make to make the cons cell mutable. This is only
        safe if we promise to initialise it fully before the next
        ML heap allocation. SPF 29/11/96
        */
        Handle value = SAVE(C_string_to_Poly(mdTaskData, strings[i]));
        Handle next  = alloc_and_save(mdTaskData, SIZEOF(ML_Cons_Cell));
        
        DEREFLISTHANDLE(next)->h = DEREFWORDHANDLE(value); 
        DEREFLISTHANDLE(next)->t = DEREFLISTHANDLE(list);
        
        // reset save vector to stop it overflowing    
        mdTaskData->saveVec.reset(saved);
        list = SAVE(DEREFHANDLE(next));
    }
    
    return list;
}

/* Convert a string list to a vector of C strings. */
char **stringListToVector(Handle list)
{
    int len = 0;
    /* Find the length of the list */
    for (PolyWord p = DEREFHANDLE(list); p != ListNull; p = ((ML_Cons_Cell*)p.AsObjPtr())->t) len++;
    /* Allocate vector. */
    char **vec = (char**)calloc(len+1, sizeof(char*));
    /* Copy the strings and put them into the vector. */
    len = 0;

    PolyWord q = DEREFHANDLE(list);
    while (q != ListNull) {
        ML_Cons_Cell *cell = (ML_Cons_Cell*)q.AsObjPtr();
        vec[len++] = Poly_string_to_C_alloc(cell->h);
        q = cell->t;
    }
    return vec;
}

/* Free the memory used by the vector. */
void freeStringVector(char **vec)
{
    char **p;
    if (vec == 0) return;
    for (p = vec; *p != 0; p++) free(*p);
    free(vec);
}

// Concatenate two strings.  Now used only internally in the RTS.
Handle strconcatc(TaskData *mdTaskData, Handle y, Handle x)
/* Note: arguments are in the reverse order from Poly */
{
    Handle result;
    POLYUNSIGNED len, xlen, ylen;
    char *from_ptr, *to_ptr;
    
    if (IS_INT(DEREFWORD(x)))
        xlen = 1;
    else 
        xlen = DEREFSTRINGHANDLE(x)->length;
    
    /* Don't concatenate with null strings */
    if (xlen == 0) return y;
    
    if (IS_INT(DEREFWORD(y)))
        ylen = 1;
    else
        ylen = DEREFSTRINGHANDLE(y)->length;
    
    if (ylen == 0) return x;
    
    len = xlen + ylen;
    
    /* Get store for combined string. Include rounding up to next word and
    room for the length word and add in the flag. */
    result = alloc_and_save(mdTaskData, (len + sizeof(PolyWord)-1)/sizeof(PolyWord) + 1, F_BYTE_OBJ);
    
    DEREFSTRINGHANDLE(result)->length = len;
    
    /* Copy first string */
    to_ptr = DEREFSTRINGHANDLE(result)->chars;
    if (xlen == 1)
    {
        *to_ptr++ = (char)UNTAGGED(DEREFSTRINGHANDLE(x));
    }
    else
    {
        from_ptr = DEREFSTRINGHANDLE(x)->chars;
        while (xlen-- > 0) (*to_ptr++ = *from_ptr++);
    }
    
    
    /* Add on second */
    if (ylen == 1)
    {
        *to_ptr = (char)UNTAGGED(DEREFSTRINGHANDLE(y));
    }
    else
    {
        from_ptr = DEREFSTRINGHANDLE(y)->chars;
        while (ylen-- > 0) (*to_ptr++ = *from_ptr++);
    }
    
    return(result);
} /* strconcat */

void print_string(PolyWord s)
{
    extern FILE *polyStdout;
    if (IS_INT(s))
        putc((char)UNTAGGED(s), polyStdout);
    else {
        PolyStringObject * str = (PolyStringObject *)s.AsObjPtr();
        fwrite(str->chars, 1, str->length, polyStdout);
    }
}

Handle string_length_c(TaskData *mdTaskData, Handle string)    /* Length of a string */
{
    PolyWord str = string->Word();
    if (str.IsTagged()) // Short form
        return Make_fixed_precision(mdTaskData, 1);
    
    POLYUNSIGNED length = ((PolyStringObject *)str.AsObjPtr())->length;
    return Make_fixed_precision(mdTaskData, length);
}

static PolyStringObject s_test_x, s_test_y;

// These functions are used in the interpreter.  They are generally replaced by
// hand-coded versions in the assembly code section.
static int string_test(PolyWord x, PolyWord y) 
/* Returns -1, 0, +1 if the first string is less, equal to or greater than the
   second. These are addresses of the strings because calling
   fix_persistent_address could result in a garbage-collection which could
   move the other string. */
{
    POLYUNSIGNED i;
    PolyStringObject *xs, *ys;
    /* Deal with single characters. */
    if (IS_INT(x))
    {
        s_test_x.length = 1;
        s_test_x.chars[0] = (char)UNTAGGED(x);
        xs = &s_test_x;
    }
    else xs = (PolyStringObject*)x.AsObjPtr();

    if (IS_INT(y))
    {
        s_test_y.length = 1;
        s_test_y.chars[0] = (char)UNTAGGED(y);
        ys = &s_test_y;
    }
    else ys = (PolyStringObject*)y.AsObjPtr();
    /* Now do the comparison. */

    for(i = 0; i < xs->length && i < ys->length; i++)
    {
        if (xs->chars[i] != ys->chars[i])
            return xs->chars[i] < ys->chars[i] ? -1 : 1;
    }
    /* They must be equal or one must be a leading substring of the other. */
    if (i < xs->length)
        return 1; /* y must be the substring. */
    else if (i < ys->length)
        return -1; /* x must be the substring */
    else
        return 0; /* They must be equal. */
}

Handle compareStrings(TaskData *mdTaskData, Handle y, Handle x)
{
    return mdTaskData->saveVec.push(TAGGED(string_test(DEREFWORD(x), DEREFWORD(y))));
}

Handle testStringEqual(TaskData *mdTaskData, Handle y, Handle x)
{
    return mdTaskData->saveVec.push(string_test(DEREFWORD(x), DEREFWORD(y)) == 0 ? TAGGED(1) : TAGGED(0));
}

Handle testStringNotEqual(TaskData *mdTaskData, Handle y, Handle x)
{
    return mdTaskData->saveVec.push(string_test(DEREFWORD(x), DEREFWORD(y)) != 0 ? TAGGED(1) : TAGGED(0));
}

Handle testStringGreater(TaskData *mdTaskData, Handle y, Handle x)
{
    return mdTaskData->saveVec.push(string_test(DEREFWORD(x), DEREFWORD(y)) > 0 ? TAGGED(1) : TAGGED(0));
}

Handle testStringLess(TaskData *mdTaskData, Handle y, Handle x)
{
    return mdTaskData->saveVec.push(string_test(DEREFWORD(x), DEREFWORD(y)) < 0 ? TAGGED(1) : TAGGED(0));
}

Handle testStringGreaterOrEqual(TaskData *mdTaskData, Handle y, Handle x)
{
    return mdTaskData->saveVec.push(string_test(DEREFWORD(x), DEREFWORD(y)) >= 0 ? TAGGED(1) : TAGGED(0));
}

Handle testStringLessOrEqual(TaskData *mdTaskData, Handle y, Handle x)
{
    return mdTaskData->saveVec.push(string_test(DEREFWORD(x), DEREFWORD(y)) <= 0 ? TAGGED(1) : TAGGED(0));
}
