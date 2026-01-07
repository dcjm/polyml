/*
    Title:  polystring.h - String functions and types

    Copyright (c) 2006, 2015 David C.J. Matthews

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License  version 2.1 as published by the Free Software Foundation.
    
    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.
    
    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA

*/

#ifndef POLYSTRING_H
#define POLYSTRING_H

#include <string>

#include "globals.h" // For PolyObject

class SaveVecEntry;
typedef SaveVecEntry *Handle;
class TaskData;

// A string object.
class PolyStringObject: public PolyObject {

public:
    POLYUNSIGNED length;
    char chars[1];
};

inline POLYUNSIGNED PolyStringLength(PolyWord ps) { return ((PolyStringObject*)ps.AsObjPtr())->length; }

// We often want to be able to allocate a temporary C string from a Poly string
// and have it automatically deallocated when the context has been exited.

// Functions to return std::string and std::wstring have now been added and this should
// reduce the need for TempString and TempCString.

extern PolyWord EmptyString(TaskData *mdTaskData);

/* PolyStringObject functions */
extern PolyWord C_string_to_Poly(TaskData *mdTaskData, const char *buffer, size_t buffLen = -1);
extern POLYUNSIGNED Poly_string_to_C(PolyWord ps, char *buff, POLYUNSIGNED bufflen);
extern char *Poly_string_to_C_alloc(PolyWord ps, size_t extraChars = 0);
extern std::string PolyStringToCString(PolyWord ps);

extern Handle convert_string_list(TaskData *mdTaskData, int count, char **strings);

// Dynamically allocated strings with automatic freeing.
// These are mainly used for file-names.
class TempCString
{
public:
    TempCString(char *p = 0):  m_value(p) {}
    TempCString(PolyWord ps): m_value(Poly_string_to_C_alloc(ps)) {}
    ~TempCString();

    operator char*() { return m_value; }
    char* operator = (char* p)  { return (m_value = p); }

private:
    char *m_value;
};

#if (defined(_WIN32) && defined(UNICODE))

extern unsigned int codePage;

extern bool setWindowsCodePage(const TCHAR *codePageArg);

#ifdef HAVE_TCHAR_H
#include <tchar.h>
#else
#define WCHAR short
#define TCHAR char
#endif

#define std_tstring std::wstring

extern PolyWord C_string_to_Poly(TaskData *mdTaskData, const WCHAR *buffer, size_t buffLen = -1);
extern POLYUNSIGNED Poly_string_to_C(PolyWord ps, WCHAR *buff, POLYUNSIGNED bufflen);
extern WCHAR *Poly_string_to_U_alloc(PolyWord ps, size_t extraChars = 0);
extern std::wstring PolyStringToUString(PolyWord ps);

extern Handle convert_string_list(TaskData *mdTaskData, int count, WCHAR **strings);

// Poly_string_to_T_alloc returns a Unicode string in Unicode and char string otherwise.
#define Poly_string_to_T_alloc  Poly_string_to_U_alloc
#define PolyStringToTString PolyStringToUString

// Unicode on Windows, character strings elsewhere.
class TempString
{
public:
    TempString(TCHAR *p = 0): m_value(p) {}
    TempString(PolyWord ps): m_value(Poly_string_to_T_alloc(ps)) {}
    ~TempString();

    operator TCHAR*() { return m_value; }
    TCHAR* operator = (TCHAR* p)  { return (m_value = p); }

private:
    TCHAR *m_value;
};

#else
#define Poly_string_to_T_alloc  Poly_string_to_C_alloc
#define TempString TempCString
#define std_tstring std::string
#define PolyStringToTString PolyStringToCString

#endif

extern char **stringListToVector(Handle list);
extern void freeStringVector(char **vec);
extern void print_string(PolyWord s);

#endif /* POLYSTRING_H */
