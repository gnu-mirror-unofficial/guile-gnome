/* guile-gnome
 * Copyright (C) 2003,2004,2011 Andy Wingo <wingo at pobox dot com>
 *
 * guile-support.c: Support routines for old Guile versions
 *
 * This program is free software; you can redistribute it and/or    
 * modify it under the terms of the GNU General Public License as   
 * published by the Free Software Foundation; either version 2 of   
 * the License, or (at your option) any later version.              
 *                                                                  
 * This program is distributed in the hope that it will be useful,  
 * but WITHOUT ANY WARRANTY; without even the implied warranty of   
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the    
 * GNU General Public License for more details.                     
 *                                                                  
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, contact:
 *
 * Free Software Foundation           Voice:  +1-617-542-5942
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652
 * Boston, MA  02111-1307,  USA       gnu@gnu.org
 */

#include <guile-support.h>
#include <string.h>


char*
scm_to_locale_string_dynwind (SCM s)
{
    char *ret = scm_to_locale_string (s);
    scm_dynwind_free (ret);
    return ret;
}

char*
scm_symbol_chars (SCM s)
{
    return scm_to_locale_string (scm_symbol_to_string (s));
}

char*
scm_symbol_chars_dynwind (SCM s)
{
    char *ret = scm_symbol_chars (s);
    scm_dynwind_free (ret);
    return ret;
}

char*
scm_keyword_chars (SCM s)
{
    return scm_symbol_chars (scm_keyword_to_symbol (s));
}

char*
scm_keyword_chars_dynwind (SCM s)
{
    return scm_symbol_chars_dynwind (scm_keyword_to_symbol (s));
}

typedef struct {
    void *func;
    void *p[5];
    unsigned int u[3];
    int d[3];
    const void *c[4];
} arg_data;

static void*
_invoke_v__p_p (void *p)
{
    arg_data *a = p;
    void (*func)(void*, void*) = a->func;
    func(a->p[0], a->p[1]);
    return NULL;
}

void
scm_dynwind_guile_v__p_p (void* (*dynwind)(void*(*)(void*), void*),
                          void *func, void *arg1, void *arg2)
{
    arg_data args = {func, {arg1, arg2,},};
    dynwind (_invoke_v__p_p, &args);
}

static void*
_invoke_v__p_p_p_p_p (void *p)
{
    arg_data *a = p;
    void (*func)(void*, void*, void*, void*, void*) = a->func;
    func(a->p[0], a->p[1], a->p[2], a->p[3], a->p[4]);
    return NULL;
}

void
scm_dynwind_guile_v__p_p_p_p_p (void* (*dynwind)(void*(*)(void*), void*),
                                void *func, void *arg1, void *arg2,
                                void *arg3, void *arg4, void *arg5)
{
    arg_data args = {func, {arg1, arg2, arg3, arg4, arg5},};
    dynwind (_invoke_v__p_p_p_p_p, &args);
}

static void*
_invoke_v__p_u_p_p (void *p)
{
    arg_data *a = p;
    void (*func)(void*, unsigned int, void*, void*) = a->func;
    func(a->p[0], a->u[0], a->p[1], a->p[2]);
    return NULL;
}

void
scm_dynwind_guile_v__p_u_p_p (void* (*dynwind)(void*(*)(void*), void*),
                              void *func, void *arg1, unsigned int arg2,
                              void *arg3, void *arg4)
{
    arg_data args = {func, {arg1, arg3, arg4,}, {arg2,},};
    dynwind (_invoke_v__p_u_p_p, &args);
}

static void*
_invoke_v__p_u_c_p (void *p)
{
    arg_data *a = p;
    void (*func)(void*, unsigned int, const void*, void*) = a->func;
    func(a->p[0], a->u[0], a->c[0], a->p[1]);
    return NULL;
}

void
scm_dynwind_guile_v__p_u_c_p (void* (*dynwind)(void*(*)(void*), void*),
                              void *func, void *arg1, unsigned int arg2,
                              const void *arg3, void *arg4)
{
    arg_data args = {func, {arg1, arg4,}, {arg2,}, {0,}, {arg3,}};
    dynwind (_invoke_v__p_u_c_p, &args);
}
