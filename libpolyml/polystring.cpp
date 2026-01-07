/*
    Title:  polystring.cpp - String functions and types

    Copyright (c) 2006, 2015, 2025 David C.J. Matthews

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

#include <string>

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
#define DEREFSTRINGHANDLE(_x)    ((PolyStringObject *)(_x)->WordP())

// Return empty string.
PolyWord EmptyString(TaskData *mdTaskData)
{
    // It might be preferable to have a single empty string.
    PolyStringObject *result = (PolyStringObject *)(alloc(mdTaskData, 1, F_BYTE_OBJ));
    result->length = 0;
    return result;
}

PolyWord C_string_to_Poly(TaskData *mdTaskData, const char *buffer, size_t buffLen)
/* Returns a C string as a Poly string. */
{
    if (buffer == NULL) return EmptyString(mdTaskData);

    if (buffLen == (size_t)-1) buffLen = strlen(buffer);

    /* Get the number of words required, plus 1 for length word,
       plus flag bit. */
    PolyStringObject *result = (PolyStringObject *)(alloc(mdTaskData, WORDS(buffLen) + 1, F_BYTE_OBJ));
    
    /* Set length of string, then copy the characters. */
    result->length = (POLYUNSIGNED)buffLen;
    memcpy(result->chars,buffer,buffLen);
    return result;
} /* C_string_to_Poly */

POLYUNSIGNED Poly_string_to_C(PolyWord ps, char *buff, POLYUNSIGNED bufflen)
/* Copies the characters from the string into the destination buffer.
   Returns original length of string. */
{
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
    PolyStringObject * str = (PolyStringObject *)ps.AsObjPtr();
    POLYUNSIGNED chars = str->length;
    char    *res = (char*)malloc(chars + extraChars + 1);
    if (res == 0) return 0;
    if (chars != 0) strncpy(res, str->chars, chars);
    res[chars] = '\0';
    return res;
} /* Poly_string_to_C_alloc */

std::string PolyStringToCString(PolyWord ps)
{
    PolyStringObject* str = (PolyStringObject*)ps.AsObjPtr();
    return std::string(str->chars, str->length);
}

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
    if (buffer == NULL) return EmptyString(mdTaskData);

    // Get the length of the string, without the terminating null.
    if (buffLen == -1) buffLen = wcslen(buffer);
    if (buffLen == 0) return EmptyString(mdTaskData); // If it's zero return empty string.

    // Find the length when converted.
    int outputLen = WideCharToMultiByte(codePage, 0, buffer, (int)buffLen, NULL, 0, NULL, NULL);

    // Return the null string if there's an error 
    if (outputLen <= 0) return EmptyString(mdTaskData);
     
    // Get the number of words required, plus 1 for length word, plus flag bit.
    PolyStringObject *result = (PolyStringObject *)(alloc(mdTaskData, WORDS(outputLen) + 1, F_BYTE_OBJ));
    
    // Set length of string, then copy the characters.
    result->length = outputLen;
    int check = WideCharToMultiByte(codePage, 0, buffer, (int)buffLen, result->chars, outputLen, NULL, NULL);
    if (check <= 0) return EmptyString(mdTaskData);

    return result;
}

POLYUNSIGNED Poly_string_to_C(PolyWord ps, WCHAR *buff, POLYUNSIGNED bufflen)
{
    PolyStringObject *str = (PolyStringObject *)ps.AsObjPtr();
    int iLength = (int)str->length;
    if (iLength == 0)
    {
        // Null string.
        if (bufflen != 0) buff[0] = 0;
        return 0;
    }
    const char *iPtr = str->chars;
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
    PolyStringObject *str = (PolyStringObject *)ps.AsObjPtr();
    int iLength = (int)str->length;
    if (iLength == 0 && extraChars == 0) return _wcsdup(L"");
    const char *iPtr = str->chars;

    // Find the space required.
    int chars = MultiByteToWideChar(codePage, 0, iPtr, iLength, NULL, 0);
    if (chars <= 0) return _wcsdup(L"");
    WCHAR *res = (WCHAR*)malloc((chars+1+extraChars) * sizeof(WCHAR));
    if (res == 0) return 0;
    chars = MultiByteToWideChar(codePage, 0, iPtr, iLength, res, chars);
    res[chars] = 0;
    return res;
}

std::wstring PolyStringToUString(PolyWord ps)
{
    PolyStringObject* str = (PolyStringObject*)ps.AsObjPtr();
    int iLength = (int)str->length;
    if (iLength == 0) return std::wstring();
    const char* iPtr = str->chars;

    // Find the space required.
    int chars = MultiByteToWideChar(codePage, 0, iPtr, iLength, NULL, 0);
    if (chars <= 0) return std::wstring();
    std::wstring result;
    result.resize(chars);
    MultiByteToWideChar(codePage, 0, iPtr, iLength, &result[0], (int)result.size());
    return result;
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
        
        DEREFLISTHANDLE(next)->h = value->Word(); 
        DEREFLISTHANDLE(next)->t = list->Word();
        
        // reset save vector to stop it overflowing    
        mdTaskData->saveVec.reset(saved);
        list = SAVE(next->Word());
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
        
        DEREFLISTHANDLE(next)->h = value->Word();
        DEREFLISTHANDLE(next)->t = list->Word();
        
        // reset save vector to stop it overflowing    
        mdTaskData->saveVec.reset(saved);
        list = SAVE(next->Word());
    }
    
    return list;
}

/* Convert a string list to a vector of C strings. */
char **stringListToVector(Handle list)
{
    int len = 0;
    /* Find the length of the list */
    for (PolyWord p = list->Word(); p != ListNull; p = ((ML_Cons_Cell*)p.AsObjPtr())->t) len++;
    /* Allocate vector. */
    char **vec = (char**)calloc(len+1, sizeof(char*));
    /* Copy the strings and put them into the vector. */
    len = 0;

    PolyWord q = list->Word();
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


// Only used in Xwindows and then only for debugging.
void print_string(PolyWord s)
{
    extern FILE *polyStdout;
    PolyStringObject * str = (PolyStringObject *)s.AsObjPtr();
    fwrite(str->chars, 1, str->length, polyStdout);
}
