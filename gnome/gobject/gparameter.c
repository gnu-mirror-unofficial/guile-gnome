/* -*- Mode: C; c-basic-offset: 4 -*- */
/* guile-gnome
 * Copyright (C) 2003,2004 Andy Wingo <wingo at pobox dot com>
 *
 * gparameter.c: Support for GParamSpec
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


#include <stdio.h>
#include <string.h>
#include "gparameter.h"
#include "guile-support.h"



SCM scm_class_gparam;

static SCM scm_gparam_struct_vtable;



#define scm_gparam_struct_i_name		(scm_vtable_offset_user)
#define scm_gparam_struct_i_nick		(scm_vtable_offset_user + 1)
#define scm_gparam_struct_i_blurb		(scm_vtable_offset_user + 2)
#define scm_gparam_struct_i_flags		(scm_vtable_offset_user + 3)
#define scm_gparam_struct_i_param_type		(scm_vtable_offset_user + 4)
#define scm_gparam_struct_i_value_type		(scm_vtable_offset_user + 5)
#define scm_gparam_struct_i_owner_type		(scm_vtable_offset_user + 6)
#define scm_gparam_struct_i_n_args		(scm_vtable_offset_user + 7)
#define scm_gparam_struct_i_args		(scm_vtable_offset_user + 8)

#define SCM_SET_GPARAM_STRUCT_NAME(x,f)		{SCM_STRUCT_DATA (x) [scm_gparam_struct_i_name] = SCM_UNPACK (scm_str2symbol (f));}
#define SCM_SET_GPARAM_STRUCT_NICK(x,f)		{SCM_STRUCT_DATA (x) [scm_gparam_struct_i_nick] = SCM_UNPACK (scm_str2string (f));}
#define SCM_SET_GPARAM_STRUCT_BLURB(x,f)	{SCM_STRUCT_DATA (x) [scm_gparam_struct_i_blurb] = SCM_UNPACK (scm_str2string (f));}
#define SCM_SET_GPARAM_STRUCT_FLAGS(x,f)	{SCM_STRUCT_DATA (x) [scm_gparam_struct_i_flags] = SCM_UNPACK (f);}
#define SCM_SET_GPARAM_STRUCT_PARAM_TYPE(x,f)	{SCM_STRUCT_DATA (x) [scm_gparam_struct_i_param_type] = SCM_UNPACK (scm_c_register_gtype (f));}
#define SCM_SET_GPARAM_STRUCT_VALUE_TYPE(x,f)	{SCM_STRUCT_DATA (x) [scm_gparam_struct_i_value_type] = SCM_UNPACK (scm_c_register_gtype (f));}
#define SCM_SET_GPARAM_STRUCT_OWNER_TYPE(x,f)	{SCM_STRUCT_DATA (x) [scm_gparam_struct_i_owner_type] = SCM_UNPACK (scm_c_register_gtype (f));}
#define SCM_SET_GPARAM_STRUCT_ARG(x,i,f)	{SCM_STRUCT_DATA (x) [scm_gparam_struct_i_args + i] = SCM_UNPACK (f);}

#define SCM_GPARAM_STRUCT_NAME(x)		SCM_SYMBOL_CHARS (SCM_PACK ((SCM_STRUCT_DATA (x) [scm_gparam_struct_i_name])))
#define SCM_GPARAM_STRUCT_NICK(x)		(SCM_FALSEP (SCM_PACK (SCM_STRUCT_DATA (x) [scm_gparam_struct_i_nick])) ? \
						 NULL : \
						 SCM_STRING_CHARS (SCM_PACK (SCM_STRUCT_DATA (x) [scm_gparam_struct_i_nick])))
#define SCM_GPARAM_STRUCT_BLURB(x)		(SCM_FALSEP (SCM_PACK (SCM_STRUCT_DATA (x) [scm_gparam_struct_i_blurb])) ? \
						 NULL : \
						 SCM_STRING_CHARS (SCM_PACK (SCM_STRUCT_DATA (x) [scm_gparam_struct_i_blurb])))
#define SCM_GPARAM_STRUCT_FLAGS(x)		SCM_PACK (SCM_STRUCT_DATA (x) [scm_gparam_struct_i_flags])
#define SCM_GPARAM_STRUCT_PARAM_TYPE(x)		((GType) SCM_SMOB_DATA (SCM_PACK ((SCM_STRUCT_DATA (x) [scm_gparam_struct_i_param_type]))))
#define SCM_GPARAM_STRUCT_VALUE_TYPE(x)		((GType) SCM_SMOB_DATA (SCM_PACK ((SCM_STRUCT_DATA (x) [scm_gparam_struct_i_value_type]))))
#define SCM_GPARAM_STRUCT_OWNER_TYPE(x)		((GType) SCM_SMOB_DATA (SCM_PACK ((SCM_STRUCT_DATA (x) [scm_gparam_struct_i_owner_type]))))
#define SCM_GPARAM_STRUCT_N_ARGS(x)		(SCM_STRUCT_DATA (x) [scm_gparam_struct_i_n_args])
#define SCM_GPARAM_STRUCT_ARG(x,i)		SCM_PACK ((SCM_STRUCT_DATA (x) [scm_gparam_struct_i_args + i]))



#define SCM_GPARAM_STRUCTP(x)			(SCM_STRUCTP (x) && \
						 SCM_EQ_P (scm_struct_vtable (x), scm_gparam_struct_vtable))
#define SCM_VALIDATE_GPARAM_STRUCT(pos, value) \
  do { \
    SCM_ASSERT (SCM_GPARAM_STRUCTP (value), value, pos, FUNC_NAME); \
    SCM_ASSERT ((SCM_SYMBOLP (SCM_PACK (SCM_STRUCT_DATA (value) [scm_gparam_struct_i_name])) && \
                 (SCM_FALSEP (SCM_PACK (SCM_STRUCT_DATA (value) [scm_gparam_struct_i_nick])) || \
                  SCM_STRINGP (SCM_PACK (SCM_STRUCT_DATA (value) [scm_gparam_struct_i_nick]))) && \
                 (SCM_FALSEP (SCM_PACK (SCM_STRUCT_DATA (value) [scm_gparam_struct_i_blurb])) || \
                  SCM_STRINGP (SCM_PACK (SCM_STRUCT_DATA (value) [scm_gparam_struct_i_blurb]))) && \
                 SCM_GTYPEP (SCM_PACK (SCM_STRUCT_DATA (value) [scm_gparam_struct_i_param_type])) && \
                 SCM_GTYPEP (SCM_PACK (SCM_STRUCT_DATA (value) [scm_gparam_struct_i_value_type])) && \
                 (SCM_FALSEP (SCM_PACK (SCM_STRUCT_DATA (value) [scm_gparam_struct_i_owner_type])) || \
                  SCM_GTYPEP (SCM_PACK (SCM_STRUCT_DATA (value) [scm_gparam_struct_i_owner_type])))), \
                value, pos, FUNC_NAME); \
  } while (0)



static GQuark quark_param_struct = 0;

/* #define DEBUG_PRINT */

#ifdef DEBUG_PRINT
#define DEBUG_ALLOC(str, args...) g_print ("I: " str "\n", ##args)
#else
#define DEBUG_ALLOC(str, args...)
#endif



SCM_SYMBOL (sym_gruntime_error, "gruntime-error");



static SCM
print_gparam_struct (SCM gparam_struct, SCM port)
{
    SCM args;
    guint length, i;

    length = SCM_GPARAM_STRUCT_N_ARGS (gparam_struct);
    args = scm_c_make_vector (length, SCM_UNDEFINED);

    for (i = 0; i < length; i++)
	scm_vector_set_x (args, SCM_MAKINUM (i),
			  SCM_GPARAM_STRUCT_ARG (gparam_struct, i));

    scm_display (scm_makfrom0str ("#<gparam-struct "), port);
    scm_write (SCM_LIST8 (SCM_PACK (SCM_STRUCT_DATA (gparam_struct) [scm_gparam_struct_i_name]),
			  SCM_PACK (SCM_STRUCT_DATA (gparam_struct) [scm_gparam_struct_i_nick]),
			  SCM_PACK (SCM_STRUCT_DATA (gparam_struct) [scm_gparam_struct_i_blurb]),
			  SCM_PACK (SCM_STRUCT_DATA (gparam_struct) [scm_gparam_struct_i_flags]),
			  SCM_PACK (SCM_STRUCT_DATA (gparam_struct) [scm_gparam_struct_i_param_type]),
			  SCM_PACK (SCM_STRUCT_DATA (gparam_struct) [scm_gparam_struct_i_value_type]),
			  SCM_PACK (SCM_STRUCT_DATA (gparam_struct) [scm_gparam_struct_i_owner_type]),
			  args),
	       port);
    scm_display (scm_makfrom0str (">"), port);

    return SCM_UNSPECIFIED;
}

static SCM scm_c_pspec_to_param_struct (GParamSpec *pspec)
#define FUNC_NAME "%pspec->param-struct"
{
    guint n_args = 0;
    SCM this;
    char *blurb;

    if (G_IS_PARAM_SPEC_BOOLEAN (pspec) || G_IS_PARAM_SPEC_STRING (pspec) ||
	G_IS_PARAM_SPEC_OBJECT(pspec)   || G_IS_PARAM_SPEC_BOXED (pspec)  ||
        G_IS_PARAM_SPEC_UNICHAR (pspec) || G_IS_PARAM_SPEC_VALUE_ARRAY (pspec))
        n_args = 1;
    
    else if (G_IS_PARAM_SPEC_CHAR (pspec)  || G_IS_PARAM_SPEC_UCHAR (pspec)  ||
	     G_IS_PARAM_SPEC_INT (pspec)   || G_IS_PARAM_SPEC_UINT (pspec)   ||
	     G_IS_PARAM_SPEC_LONG (pspec)  || G_IS_PARAM_SPEC_ULONG (pspec)  ||
	     G_IS_PARAM_SPEC_INT64 (pspec) || G_IS_PARAM_SPEC_UINT64 (pspec) ||
	     G_IS_PARAM_SPEC_FLOAT (pspec) || G_IS_PARAM_SPEC_DOUBLE(pspec))
        n_args = 3;

    else if (G_IS_PARAM_SPEC_POINTER (pspec))
        n_args = 0;

    else if (G_IS_PARAM_SPEC_ENUM (pspec) || G_IS_PARAM_SPEC_FLAGS (pspec))
        n_args = 2;

    else
        SCM_ERROR_NOT_YET_IMPLEMENTED (SCM_BOOL_F);
      
    this = scm_make_struct (scm_gparam_struct_vtable, SCM_MAKINUM (n_args),
                            SCM_EOL);

    SCM_SET_GPARAM_STRUCT_NAME (this, (char *) g_param_spec_get_name (pspec));
    SCM_SET_GPARAM_STRUCT_NICK (this, (char *) g_param_spec_get_nick (pspec));
    blurb = (char *) g_param_spec_get_blurb (pspec);
    SCM_SET_GPARAM_STRUCT_BLURB (this, blurb ? blurb : "");
    SCM_SET_GPARAM_STRUCT_FLAGS (this, SCM_MAKINUM (pspec->flags));
    SCM_SET_GPARAM_STRUCT_PARAM_TYPE (this, G_TYPE_FROM_INSTANCE (pspec));
    SCM_SET_GPARAM_STRUCT_VALUE_TYPE (this, pspec->value_type);
    SCM_SET_GPARAM_STRUCT_OWNER_TYPE (this, pspec->owner_type);

    if (G_IS_PARAM_SPEC_BOOLEAN (pspec)) {
        GParamSpecBoolean *b = (GParamSpecBoolean *)pspec;
        SCM_SET_GPARAM_STRUCT_ARG (this, 0, SCM_BOOL(b->default_value));
    }
      
    else if (G_IS_PARAM_SPEC_CHAR (pspec)) {
        GParamSpecChar *c = (GParamSpecChar *) pspec;
        SCM_SET_GPARAM_STRUCT_ARG (this, 0, SCM_MAKINUM (c->minimum));
        SCM_SET_GPARAM_STRUCT_ARG (this, 1, SCM_MAKINUM (c->maximum));
        SCM_SET_GPARAM_STRUCT_ARG (this, 2, SCM_MAKINUM (c->default_value));
    }
    
    else if  (G_IS_PARAM_SPEC_UCHAR (pspec)) {
        GParamSpecUChar *u = (GParamSpecUChar *) pspec;
        SCM_SET_GPARAM_STRUCT_ARG (this, 0, SCM_MAKINUM (u->minimum));
        SCM_SET_GPARAM_STRUCT_ARG (this, 1, SCM_MAKINUM (u->maximum));
        SCM_SET_GPARAM_STRUCT_ARG (this, 2, SCM_MAKINUM (u->default_value));
    }
    
    else if (G_IS_PARAM_SPEC_INT (pspec)) {
        GParamSpecInt *i = (GParamSpecInt *) pspec;
        SCM_SET_GPARAM_STRUCT_ARG (this, 0, scm_long2num (i->minimum));
        SCM_SET_GPARAM_STRUCT_ARG (this, 1, scm_long2num (i->maximum));
        SCM_SET_GPARAM_STRUCT_ARG (this, 2, scm_long2num (i->default_value));
    }

    else if (G_IS_PARAM_SPEC_UINT (pspec)) {
        GParamSpecUInt *i = (GParamSpecUInt *) pspec;
        SCM_SET_GPARAM_STRUCT_ARG (this, 0, scm_ulong2num (i->minimum));
        SCM_SET_GPARAM_STRUCT_ARG (this, 1, scm_ulong2num (i->maximum));
        SCM_SET_GPARAM_STRUCT_ARG (this, 2, scm_ulong2num (i->default_value));
    }
    
    else if (G_IS_PARAM_SPEC_LONG (pspec)) {
        GParamSpecLong *l = (GParamSpecLong *) pspec;
        SCM_SET_GPARAM_STRUCT_ARG (this, 0, scm_long2num (l->minimum));
        SCM_SET_GPARAM_STRUCT_ARG (this, 1, scm_long2num (l->maximum));
        SCM_SET_GPARAM_STRUCT_ARG (this, 2, scm_long2num (l->default_value));
    }
    
    else if (G_IS_PARAM_SPEC_ULONG (pspec)) {
        GParamSpecULong *u = (GParamSpecULong *) pspec;
        SCM_SET_GPARAM_STRUCT_ARG (this, 0, scm_ulong2num (u->minimum));
        SCM_SET_GPARAM_STRUCT_ARG (this, 1, scm_ulong2num (u->maximum));
        SCM_SET_GPARAM_STRUCT_ARG (this, 2, scm_ulong2num (u->default_value));
    }

    else if (G_IS_PARAM_SPEC_INT64 (pspec)) {
        GParamSpecInt64 *l = (GParamSpecInt64 *) pspec;
        SCM_SET_GPARAM_STRUCT_ARG (this, 0, scm_long_long2num (l->minimum));
        SCM_SET_GPARAM_STRUCT_ARG (this, 1, scm_long_long2num (l->maximum));
        SCM_SET_GPARAM_STRUCT_ARG (this, 2, scm_long_long2num (l->default_value));
    }
    
    else if (G_IS_PARAM_SPEC_UINT64 (pspec)) {
        GParamSpecUInt64 *u = (GParamSpecUInt64 *) pspec;
        SCM_SET_GPARAM_STRUCT_ARG (this, 0, scm_ulong_long2num (u->minimum));
        SCM_SET_GPARAM_STRUCT_ARG (this, 1, scm_ulong_long2num (u->maximum));
        SCM_SET_GPARAM_STRUCT_ARG (this, 2, scm_ulong_long2num (u->default_value));
    }

    else if (G_IS_PARAM_SPEC_FLOAT (pspec)) {
        GParamSpecFloat *f = (GParamSpecFloat *) pspec;
        SCM_SET_GPARAM_STRUCT_ARG (this, 0, scm_make_real (f->minimum));
        SCM_SET_GPARAM_STRUCT_ARG (this, 1, scm_make_real (f->maximum));
        SCM_SET_GPARAM_STRUCT_ARG (this, 2, scm_make_real (f->default_value));
    }
    
    else if (G_IS_PARAM_SPEC_DOUBLE (pspec)) {
        GParamSpecDouble *d = (GParamSpecDouble *) pspec;
        SCM_SET_GPARAM_STRUCT_ARG (this, 0, scm_make_real (d->minimum));
        SCM_SET_GPARAM_STRUCT_ARG (this, 1, scm_make_real (d->maximum));
        SCM_SET_GPARAM_STRUCT_ARG (this, 2, scm_make_real (d->default_value));
    }

    /* this is borken, but it will suffice for now... */
    else if (G_IS_PARAM_SPEC_UNICHAR (pspec)) {
        GParamSpecUnichar *u = (GParamSpecUnichar *) pspec;
        SCM_SET_GPARAM_STRUCT_ARG (this, 0, scm_long2num ((long)u->default_value));
    }

    else if (G_IS_PARAM_SPEC_POINTER (pspec)) { }
    
    else if (G_IS_PARAM_SPEC_STRING (pspec)) {
        GParamSpecString *s = (GParamSpecString *) pspec;
        SCM_SET_GPARAM_STRUCT_ARG (this, 0,
                                   (s->default_value ? scm_str2string (s->default_value)
                                                     : SCM_BOOL_F));
    }
    
    else if (G_IS_PARAM_SPEC_OBJECT (pspec) || G_IS_PARAM_SPEC_BOXED (pspec)) {
        SCM_SET_GPARAM_STRUCT_ARG (this, 0, scm_c_register_gtype (pspec->value_type));
    }
    
    else if (G_IS_PARAM_SPEC_ENUM (pspec)) {
        GParamSpecEnum *e = (GParamSpecEnum *) pspec;
        GType enum_type = G_TYPE_FROM_CLASS (e->enum_class);
      
        SCM_SET_GPARAM_STRUCT_ARG (this, 0, scm_c_register_gtype (enum_type));
        SCM_SET_GPARAM_STRUCT_ARG (this, 1, SCM_MAKINUM (e->default_value));
    }
    
    else if (G_IS_PARAM_SPEC_FLAGS (pspec)) {
        GParamSpecFlags *f = (GParamSpecFlags *) pspec;
        GType flags_type = G_TYPE_FROM_CLASS (f->flags_class);
      
        SCM_SET_GPARAM_STRUCT_ARG (this, 0, scm_c_register_gtype (flags_type));
        SCM_SET_GPARAM_STRUCT_ARG (this, 1, SCM_MAKINUM (f->default_value));
    }

    else if (G_IS_PARAM_SPEC_VALUE_ARRAY (pspec)) {
        GParamSpecValueArray *va = (GParamSpecValueArray *) pspec;
        
        SCM_SET_GPARAM_STRUCT_ARG (this, 0, (va->element_spec
                                             ? scm_c_make_gtype_instance 
                                             ((GTypeInstance*)va->element_spec)
                                             : SCM_BOOL_F));
    }
    
    else
        SCM_ERROR_NOT_YET_IMPLEMENTED (SCM_BOOL_F);
        
    return this;
}
#undef FUNC_NAME



SCM_DEFINE (scm_gparam_to_param_struct, "gparam->param-struct", 1, 0, 0,
	    (SCM param),
	    "")
#define FUNC_NAME s_scm_gparam_to_param_struct
{
    GParamSpec *pspec;
    SCM pspec_struct;

    SCM_VALIDATE_GPARAM_COPY (1, param, pspec);

    pspec_struct = g_param_spec_get_qdata (pspec, quark_param_struct);
    if (pspec_struct)
	return pspec_struct;

    pspec_struct = scm_c_pspec_to_param_struct (pspec);

    /* DEBUG_ALLOC ("  protecting new pspec-struct; but when the pspec dies it will"
                    " be unreffed"); */

    g_param_spec_set_qdata_full (pspec, quark_param_struct,
				 scm_gc_protect_object (pspec_struct),
				 (GDestroyNotify) scm_gc_unprotect_object);

    return pspec_struct;
}
#undef FUNC_NAME



SCM_DEFINE (scm_gparam_primitive_create, "gparam-primitive-create", 4, 0, 0,
	    (SCM class, SCM type, SCM object, SCM pspec_struct),
	    "")
#define FUNC_NAME s_scm_gparam_primitive_create
{
    GParamSpec *pspec = NULL;
    GParamFlags flags;
    GType gtype, param_type, value_type;
    guint n_args = 0;
    SCM smob;

    SCM_VALIDATE_GTYPE_CLASS (1, class);
    SCM_VALIDATE_GTYPE_COPY (2, type, gtype);
    SCM_ASSERT (SCM_IS_A_P (SCM_CLASS_OF (object), scm_class_gtype_class), object, 3, FUNC_NAME);
    SCM_ASSERT (G_TYPE_IS_PARAM (gtype), type, 2, FUNC_NAME);
    SCM_VALIDATE_GPARAM_STRUCT (4, pspec_struct);

    param_type = SCM_GPARAM_STRUCT_PARAM_TYPE (pspec_struct);
    value_type = SCM_GPARAM_STRUCT_VALUE_TYPE (pspec_struct);
    flags = SCM_NUM2INT (4, SCM_GPARAM_STRUCT_FLAGS (pspec_struct));
      
    if (param_type == G_TYPE_PARAM_BOOLEAN
        || param_type == G_TYPE_PARAM_STRING
        || param_type == G_TYPE_PARAM_OBJECT
        || param_type == G_TYPE_PARAM_BOXED
        || param_type == G_TYPE_PARAM_UNICHAR
        || param_type == G_TYPE_PARAM_VALUE_ARRAY)
        n_args = 1;
    
    else if (param_type == G_TYPE_PARAM_CHAR
             || param_type == G_TYPE_PARAM_UCHAR
             || param_type == G_TYPE_PARAM_INT
             || param_type == G_TYPE_PARAM_UINT
             || param_type == G_TYPE_PARAM_LONG
             || param_type == G_TYPE_PARAM_ULONG
	     || param_type == G_TYPE_PARAM_INT64
             || param_type == G_TYPE_PARAM_UINT64
	     || param_type == G_TYPE_PARAM_FLOAT
             || param_type == G_TYPE_PARAM_DOUBLE)
        n_args = 3;

    else if (param_type == G_TYPE_PARAM_POINTER)
        n_args = 0;

    else if (param_type == G_TYPE_PARAM_ENUM ||  param_type == G_TYPE_PARAM_FLAGS)
        n_args = 2;

    else
        SCM_ERROR_NOT_YET_IMPLEMENTED (pspec_struct);
    
       
    SCM_ASSERT ((SCM_GPARAM_STRUCT_N_ARGS (pspec_struct) == n_args), pspec_struct, 4, FUNC_NAME); 
        

    if (param_type == G_TYPE_PARAM_BOOLEAN) {
        SCM_VALIDATE_BOOL (0, SCM_GPARAM_STRUCT_ARG (pspec_struct, 0));
        pspec = g_param_spec_boolean (SCM_GPARAM_STRUCT_NAME (pspec_struct),
                                      SCM_GPARAM_STRUCT_NICK (pspec_struct),
                                      SCM_GPARAM_STRUCT_BLURB (pspec_struct),
                                      SCM_NFALSEP (SCM_GPARAM_STRUCT_ARG (pspec_struct, 0)),
                                      flags);
    }

    else if (param_type == G_TYPE_PARAM_CHAR) {
        SCM_VALIDATE_INUM_RANGE (0, SCM_GPARAM_STRUCT_ARG (pspec_struct, 0), -128, 128);
        SCM_VALIDATE_INUM_RANGE (1, SCM_GPARAM_STRUCT_ARG (pspec_struct, 1), -128, 128);
        SCM_VALIDATE_INUM_RANGE (2, SCM_GPARAM_STRUCT_ARG (pspec_struct, 2), -128, 128);
        pspec = g_param_spec_char (SCM_GPARAM_STRUCT_NAME (pspec_struct),
                                   SCM_GPARAM_STRUCT_NICK (pspec_struct),
                                   SCM_GPARAM_STRUCT_BLURB (pspec_struct),
                                   SCM_INUM (SCM_GPARAM_STRUCT_ARG (pspec_struct, 0)),
                                   SCM_INUM (SCM_GPARAM_STRUCT_ARG (pspec_struct, 1)),
                                   SCM_INUM (SCM_GPARAM_STRUCT_ARG (pspec_struct, 2)),
                                   flags);
    }

    else if  (param_type == G_TYPE_PARAM_UCHAR) {
        SCM_VALIDATE_INUM_RANGE (0, SCM_GPARAM_STRUCT_ARG (pspec_struct, 0), 0, 256);
        SCM_VALIDATE_INUM_RANGE (1, SCM_GPARAM_STRUCT_ARG (pspec_struct, 1), 0, 256);
        SCM_VALIDATE_INUM_RANGE (2, SCM_GPARAM_STRUCT_ARG (pspec_struct, 2), 0, 256);
        pspec = g_param_spec_uchar (SCM_GPARAM_STRUCT_NAME (pspec_struct),
                                    SCM_GPARAM_STRUCT_NICK (pspec_struct),
                                    SCM_GPARAM_STRUCT_BLURB (pspec_struct),
                                    SCM_INUM (SCM_GPARAM_STRUCT_ARG (pspec_struct, 0)),
                                    SCM_INUM (SCM_GPARAM_STRUCT_ARG (pspec_struct, 1)),
                                    SCM_INUM (SCM_GPARAM_STRUCT_ARG (pspec_struct, 2)),
                                    flags);
    }
    
    else if (param_type == G_TYPE_PARAM_INT) {
        pspec = g_param_spec_int (SCM_GPARAM_STRUCT_NAME (pspec_struct),
                                  SCM_GPARAM_STRUCT_NICK (pspec_struct),
                                  SCM_GPARAM_STRUCT_BLURB (pspec_struct),
                                  SCM_NUM2LONG (0, SCM_GPARAM_STRUCT_ARG (pspec_struct, 0)),
                                  SCM_NUM2LONG (0, SCM_GPARAM_STRUCT_ARG (pspec_struct, 1)),
                                  SCM_NUM2LONG (0, SCM_GPARAM_STRUCT_ARG (pspec_struct, 2)),
                                  flags);
    }
    
    else if (param_type == G_TYPE_PARAM_UINT) {
        pspec = g_param_spec_uint (SCM_GPARAM_STRUCT_NAME (pspec_struct),
				   SCM_GPARAM_STRUCT_NICK (pspec_struct),
				   SCM_GPARAM_STRUCT_BLURB (pspec_struct),
				   SCM_NUM2ULONG (0, SCM_GPARAM_STRUCT_ARG (pspec_struct, 0)),
				   SCM_NUM2ULONG (0, SCM_GPARAM_STRUCT_ARG (pspec_struct, 1)),
				   SCM_NUM2ULONG (0, SCM_GPARAM_STRUCT_ARG (pspec_struct, 2)),
                                   flags);
    }

    else if (param_type == G_TYPE_PARAM_LONG) {
        pspec = g_param_spec_long (SCM_GPARAM_STRUCT_NAME (pspec_struct),
				   SCM_GPARAM_STRUCT_NICK (pspec_struct),
				   SCM_GPARAM_STRUCT_BLURB (pspec_struct),
				   SCM_NUM2LONG (0, SCM_GPARAM_STRUCT_ARG (pspec_struct, 0)),
				   SCM_NUM2LONG (0, SCM_GPARAM_STRUCT_ARG (pspec_struct, 1)),
				   SCM_NUM2LONG (0, SCM_GPARAM_STRUCT_ARG (pspec_struct, 2)),
				   flags);
    }

    else if (param_type == G_TYPE_PARAM_ULONG) {
        pspec = g_param_spec_ulong (SCM_GPARAM_STRUCT_NAME (pspec_struct),
                                    SCM_GPARAM_STRUCT_NICK (pspec_struct),
                                    SCM_GPARAM_STRUCT_BLURB (pspec_struct),
                                    SCM_NUM2ULONG (0, SCM_GPARAM_STRUCT_ARG (pspec_struct, 0)),
                                    SCM_NUM2ULONG (0, SCM_GPARAM_STRUCT_ARG (pspec_struct, 1)),
                                    SCM_NUM2ULONG (0, SCM_GPARAM_STRUCT_ARG (pspec_struct, 2)),
                                    flags);
    }

    else if (param_type == G_TYPE_PARAM_INT64) {
        pspec = g_param_spec_int64 (SCM_GPARAM_STRUCT_NAME (pspec_struct),
                                    SCM_GPARAM_STRUCT_NICK (pspec_struct),
                                    SCM_GPARAM_STRUCT_BLURB (pspec_struct),
                                    SCM_NUM2LONG_LONG (0, SCM_GPARAM_STRUCT_ARG (pspec_struct, 0)),
                                    SCM_NUM2LONG_LONG (0, SCM_GPARAM_STRUCT_ARG (pspec_struct, 1)),
                                    SCM_NUM2LONG_LONG (0, SCM_GPARAM_STRUCT_ARG (pspec_struct, 2)),
                                    flags);
    }

    else if (param_type == G_TYPE_PARAM_UINT64) {
        pspec = g_param_spec_uint64 (SCM_GPARAM_STRUCT_NAME (pspec_struct),
                                     SCM_GPARAM_STRUCT_NICK (pspec_struct),
                                     SCM_GPARAM_STRUCT_BLURB (pspec_struct),
                                     SCM_NUM2ULONG_LONG (0, SCM_GPARAM_STRUCT_ARG (pspec_struct, 0)),
                                     SCM_NUM2ULONG_LONG (0, SCM_GPARAM_STRUCT_ARG (pspec_struct, 1)),
                                     SCM_NUM2ULONG_LONG (0, SCM_GPARAM_STRUCT_ARG (pspec_struct, 2)),
                                     flags);
    }

    else if (param_type == G_TYPE_PARAM_FLOAT) {
        float min_value, max_value, default_value;

	SCM_VALIDATE_FLOAT_COPY (0, SCM_GPARAM_STRUCT_ARG (pspec_struct, 0), min_value);
	SCM_VALIDATE_FLOAT_COPY (0, SCM_GPARAM_STRUCT_ARG (pspec_struct, 1), max_value);
	SCM_VALIDATE_FLOAT_COPY (0, SCM_GPARAM_STRUCT_ARG (pspec_struct, 2), default_value);

	pspec = g_param_spec_float (SCM_GPARAM_STRUCT_NAME (pspec_struct),
				    SCM_GPARAM_STRUCT_NICK (pspec_struct),
				    SCM_GPARAM_STRUCT_BLURB (pspec_struct),
				    min_value, max_value, default_value,
				    flags);
    }

    else if (param_type == G_TYPE_PARAM_DOUBLE) {
        pspec = g_param_spec_double (SCM_GPARAM_STRUCT_NAME (pspec_struct),
				     SCM_GPARAM_STRUCT_NICK (pspec_struct),
				     SCM_GPARAM_STRUCT_BLURB (pspec_struct),
				     scm_num2dbl (SCM_GPARAM_STRUCT_ARG (pspec_struct, 0), FUNC_NAME),
				     scm_num2dbl (SCM_GPARAM_STRUCT_ARG (pspec_struct, 1), FUNC_NAME),
				     scm_num2dbl (SCM_GPARAM_STRUCT_ARG (pspec_struct, 2), FUNC_NAME),
				     flags);
    }

    else if (param_type == G_TYPE_PARAM_UNICHAR) {
        pspec = g_param_spec_unichar (SCM_GPARAM_STRUCT_NAME (pspec_struct),
                                      SCM_GPARAM_STRUCT_NICK (pspec_struct),
                                      SCM_GPARAM_STRUCT_BLURB (pspec_struct),
                                      (gunichar)SCM_NUM2LONG (0, SCM_GPARAM_STRUCT_ARG (pspec_struct, 0)),
                                      flags);
    }

    else if (param_type == G_TYPE_PARAM_POINTER) {
        pspec = g_param_spec_pointer (SCM_GPARAM_STRUCT_NAME (pspec_struct),
				      SCM_GPARAM_STRUCT_NICK (pspec_struct),
				      SCM_GPARAM_STRUCT_BLURB (pspec_struct),
				      flags);
    }

    else if (param_type == G_TYPE_PARAM_STRING) {
        const gchar *string = NULL;

	if (SCM_NFALSEP (SCM_GPARAM_STRUCT_ARG (pspec_struct, 0)))
	    SCM_VALIDATE_STRING_COPY (0, SCM_GPARAM_STRUCT_ARG (pspec_struct, 0), string);

	pspec = g_param_spec_string (SCM_GPARAM_STRUCT_NAME (pspec_struct),
				     SCM_GPARAM_STRUCT_NICK (pspec_struct),
				     SCM_GPARAM_STRUCT_BLURB (pspec_struct),
				     string,
				     flags);
    }

    else if (param_type == G_TYPE_PARAM_OBJECT) {
        GType object_type;

        SCM_VALIDATE_GTYPE_COPY (0, SCM_GPARAM_STRUCT_ARG (pspec_struct, 0), object_type);
        pspec = g_param_spec_object (SCM_GPARAM_STRUCT_NAME (pspec_struct),
                                     SCM_GPARAM_STRUCT_NICK (pspec_struct),
                                     SCM_GPARAM_STRUCT_BLURB (pspec_struct),
                                     object_type,
                                     flags);
    }

    else if (param_type == G_TYPE_PARAM_BOXED) {
        GType boxed_type;

	SCM_VALIDATE_GTYPE_COPY (0, SCM_GPARAM_STRUCT_ARG (pspec_struct, 0), boxed_type);
	pspec = g_param_spec_boxed (SCM_GPARAM_STRUCT_NAME (pspec_struct),
				    SCM_GPARAM_STRUCT_NICK (pspec_struct),
				    SCM_GPARAM_STRUCT_BLURB (pspec_struct),
				    boxed_type,
				    flags);
    }

    else if (param_type == G_TYPE_PARAM_ENUM) {
        GType enum_type;

	SCM_VALIDATE_GTYPE_COPY (0, SCM_GPARAM_STRUCT_ARG (pspec_struct, 0), enum_type);
	pspec = g_param_spec_enum (SCM_GPARAM_STRUCT_NAME (pspec_struct),
				   SCM_GPARAM_STRUCT_NICK (pspec_struct),
				   SCM_GPARAM_STRUCT_BLURB (pspec_struct),
				   enum_type,
				   SCM_INUM (SCM_GPARAM_STRUCT_ARG (pspec_struct, 1)),
				   flags);
    }

    else if (param_type == G_TYPE_PARAM_FLAGS) {
        GType flags_type;

        SCM_VALIDATE_GTYPE_COPY (0, SCM_GPARAM_STRUCT_ARG (pspec_struct, 0), flags_type);
        pspec = g_param_spec_flags (SCM_GPARAM_STRUCT_NAME (pspec_struct),
                                    SCM_GPARAM_STRUCT_NICK (pspec_struct),
                                    SCM_GPARAM_STRUCT_BLURB (pspec_struct),
                                    flags_type,
                                    SCM_INUM (SCM_GPARAM_STRUCT_ARG (pspec_struct, 1)),
                                    flags);
    }
    else if (param_type == G_TYPE_PARAM_VALUE_ARRAY) {
        pspec = g_param_spec_value_array (SCM_GPARAM_STRUCT_NAME (pspec_struct),
                                          SCM_GPARAM_STRUCT_NICK (pspec_struct),
                                          SCM_GPARAM_STRUCT_BLURB (pspec_struct),
                                          NULL,
                                          flags);
    }
    else {SCM_ERROR_NOT_YET_IMPLEMENTED (pspec_struct);}
			  
    if (!pspec)
        scm_error (sym_gruntime_error, FUNC_NAME,
		   "Can't create gparam instance ~A from this pspec struct: ~A",
		   SCM_LIST2 (type, pspec_struct), SCM_EOL);

    smob = scm_c_make_gtype_instance ((GTypeInstance *) pspec);
    /* remove floating reference (we still hold the one from
     * make_gtype_instance) */
    g_param_spec_sink (pspec);
    DEBUG_ALLOC ("sunk guile-owned param spec %p of type %s, ->%u", 
                 pspec, g_type_name (G_TYPE_FROM_INSTANCE (pspec)),
                 pspec->ref_count);

    scm_slot_set_x (object, scm_sym_gtype_instance, smob);

    return smob;
}
#undef FUNC_NAME

SCM_DEFINE (scm_gparam_to_value_type, "gparam->value-type", 1, 0, 0,
	    (SCM param),
	    "")
#define FUNC_NAME s_scm_gparam_to_value_type
{
    GParamSpec *gparam;

    SCM_VALIDATE_GPARAM_COPY (1, param, gparam);

    return scm_c_register_gtype (G_PARAM_SPEC_VALUE_TYPE (gparam));
}
#undef FUNC_NAME

void
scm_init_gnome_gobject_parameters (void)
{
    SCM gsubr;

#ifndef SCM_MAGIC_SNARFER
#include "gparameter.x"
#endif

    quark_param_struct = g_quark_from_static_string ("%scm-param-struct");

    gsubr = scm_c_make_gsubr ("%print-gparam-struct", 2, 0, 0, print_gparam_struct);

    scm_gparam_struct_vtable = scm_permanent_object
	(scm_make_vtable_vtable (scm_makfrom0str ("pwpwpwpwpwpwpwpW"),
				 SCM_INUM0, SCM_LIST1 (gsubr)));

    scm_c_define ("gparam-struct-name", SCM_MAKINUM (scm_gparam_struct_i_name));
    scm_c_define ("gparam-struct-nick", SCM_MAKINUM (scm_gparam_struct_i_nick));
    scm_c_define ("gparam-struct-blurb", SCM_MAKINUM (scm_gparam_struct_i_blurb));
    scm_c_define ("gparam-struct-flags", SCM_MAKINUM (scm_gparam_struct_i_flags));
    scm_c_define ("gparam-struct-param-type", SCM_MAKINUM (scm_gparam_struct_i_param_type));
    scm_c_define ("gparam-struct-value-type", SCM_MAKINUM (scm_gparam_struct_i_value_type));
    scm_c_define ("gparam-struct-owner-type", SCM_MAKINUM (scm_gparam_struct_i_owner_type));
    scm_c_define ("gparam-struct-n-args", SCM_MAKINUM (scm_gparam_struct_i_n_args));
    scm_c_define ("gparam-struct-args", SCM_MAKINUM (scm_gparam_struct_i_args));
    scm_c_define ("gparam-struct-vtable", scm_gparam_struct_vtable);

    /* fixme: these names suck */
    scm_c_define ("gruntime:uint-max", scm_ulong2num (G_MAXUINT));
    scm_c_define ("gruntime:int-min", scm_long2num (G_MININT));
    scm_c_define ("gruntime:int-max", scm_long2num (G_MAXINT));
    scm_c_define ("gruntime:ulong-max", scm_ulong2num (G_MAXULONG));
    scm_c_define ("gruntime:long-min", scm_long2num (G_MINLONG));
    scm_c_define ("gruntime:long-max", scm_long2num (G_MAXLONG));
    scm_c_define ("gruntime:uint64-max", scm_ulong_long2num (G_MAXUINT64));
    scm_c_define ("gruntime:int64-min", scm_long_long2num (G_MININT64));
    scm_c_define ("gruntime:int64-max", scm_long_long2num (G_MAXINT64));
    scm_c_define ("gruntime:float-max", scm_make_real (G_MAXFLOAT));
    scm_c_define ("gruntime:float-min", scm_make_real (G_MINFLOAT));
    scm_c_define ("gruntime:double-max", scm_make_real (G_MAXDOUBLE));
    scm_c_define ("gruntime:double-min", scm_make_real (G_MINDOUBLE));
    scm_c_define ("gruntime:byte-order", scm_long2num (G_BYTE_ORDER));

    scm_c_define_and_export_gtype_x (G_TYPE_PARAM_CHAR);
    scm_c_define_and_export_gtype_x (G_TYPE_PARAM_UCHAR); 				
    scm_c_define_and_export_gtype_x (G_TYPE_PARAM_BOOLEAN);
    scm_c_define_and_export_gtype_x (G_TYPE_PARAM_INT);
    scm_c_define_and_export_gtype_x (G_TYPE_PARAM_UINT);
    scm_c_define_and_export_gtype_x (G_TYPE_PARAM_ENUM);
    scm_c_define_and_export_gtype_x (G_TYPE_PARAM_FLAGS);
    scm_c_define_and_export_gtype_x (G_TYPE_PARAM_LONG);
    scm_c_define_and_export_gtype_x (G_TYPE_PARAM_ULONG);
    scm_c_define_and_export_gtype_x (G_TYPE_PARAM_INT64);
    scm_c_define_and_export_gtype_x (G_TYPE_PARAM_UINT64);
    scm_c_define_and_export_gtype_x (G_TYPE_PARAM_FLOAT);
    scm_c_define_and_export_gtype_x (G_TYPE_PARAM_DOUBLE);
    scm_c_define_and_export_gtype_x (G_TYPE_PARAM_STRING);
    scm_c_define_and_export_gtype_x (G_TYPE_PARAM_POINTER);
    scm_c_define_and_export_gtype_x (G_TYPE_PARAM_BOXED);
    scm_c_define_and_export_gtype_x (G_TYPE_PARAM_OBJECT);

    scm_class_gparam = scm_permanent_object (SCM_VARIABLE_REF (scm_c_lookup ("<gparam>")));
    /* only export from parameters.scm */
}
