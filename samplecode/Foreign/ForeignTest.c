/* Example code for a C-library accessible from ML
using the CInterface structure.

Copyright David C.J. Matthews 1999, 2009, 2015

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
Linux: cc -shared -o Foreign ForeignTest.c
Windows: cl /MT ForeignTest.c /link /dll /out:Foreign.dll /def:Foreign.def
Mac OS X : cc -dynamiclib -o Foreign.dylib ForeignTest.c
*/
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

/* Return a string duplicated n Times. */
char *DupNString(int n, char *str)
{
    int nSize = strlen(str);
    char *res = malloc(n*nSize + 1);
    int i;
    *res = 0;
    for (i = 0; i < n; i++) strcat(res, str);
    return res;
}
typedef struct _tree {
    struct _tree *left, *right;
    int	nValue;
} *tree;

int SumTree(tree t)
{
    if (t == NULL) return 0;
    else return t->nValue + SumTree(t->left) + SumTree(t->right);
}

/* Tests to see whether small structures are returned specially. */
struct r2 { int n, m; };

struct r2 ReturnR2(int n, int m)
{
    struct r2 ret;
    ret.n = n + 1;
    ret.m = m + 1;
    return ret;
}

// The SysV ABI for X64 Unix puts these in RAX/XMM0.
struct r4 {
    int a, b;
    float c, d;
};

struct r4 ReturnR4(int n, int m)
{
    struct r4 ret;
    ret.a = n + 1;
    ret.b = m + 1;
    ret.c = n + 2;
    ret.d = m + 2;
    return ret;
}

struct r6 {
    int a, b, c, d, e, f;
};

struct r6 ReturnR6(int n, int m)
{
    struct r6 ret;
    ret.a = n + 1;
    ret.b = m + 1;
    ret.c = n + 2;
    ret.d = m + 2;
    ret.e = n + 3;
    ret.f = m + 3;
    return ret;
}

/* Added.  Callback function. */

typedef int(*INT_INT_CALLBACK) (int a, int b);

int MakeCallback(int i, INT_INT_CALLBACK p)
{
    return (*p)(i, 4) + (*p)(i + 1, 5);
}


/* Another callback function.  This tests the various argument types. */
typedef double(*DBL_CALLBACK) (int a, char b, double c, float d, short e, int *f);

double MakeCallback2(DBL_CALLBACK p)
{
    int x = 1;
    double y = p(12345, 'X', (double)1.414, (float)2.8, 44, &x);
    return y;
}

// Check that void results work for callbacks.
void MakeCallback3(void(*mlcall)(int), int i)
{
    mlcall(i + 1);
}

/* Test for finalisation. */
void *AllocateIt()
{
    void *p = malloc(1);
    printf("Allocated object at %p\n", p);
    fflush(stdout);
    return p;
}

void FreeIt(void *p)
{
    printf("Freed object at %p\n", p);
    fflush(stdout);
    free(p);
}

/* Test for call-by-reference.  Added in updated FFI. */
void UpdateArg(int i, int *p)
{
    *p += i;
}

/* Test for returning a function.  Added in updated FFI. */
static int acallBack(int q)
{
    return q * 2;
}

typedef int(*CB)(int);

void ReturnFn(CB *v)
{
    *v = acallBack;
}

/* Test for large number of arguments which requires the stack on ARM64. */
int ManyArguments(int a, int b, int c, int d, int e, int f, int g, int h, int i, int j)
{
    return a + h *10 + i*100 + j*1000;
}
