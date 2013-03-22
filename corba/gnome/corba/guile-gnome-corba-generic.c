/* guile-gnome
 * Copyright (C) 2001, 2013 Martin Baulig <martin@gnome.org>
 * Copyright (C) 2003 Andy Wingo <wingo at pobox dot com>
 *
 * guile-gnome-corba-generic.c:
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

#include <guile-gnome-corba-generic.h>
#include <guile-gnome-gobject.h>
#include <glib.h>
#include <string.h>

static GQuark quark_corba_typecode = 0;
static GHashTable *typecode_type_hash = NULL;

gchar *
guile_corba_generic_repo_id_to_name (const gchar *format, const gchar *repo_id)
{
    gchar *new_repo_id, *ptr, *retval;

    if (!strncmp (repo_id, "IDL:", 4))
	repo_id += 4;

    new_repo_id = g_strdup (repo_id);
    ptr = strchr (new_repo_id, ':');
    if (ptr)
	*ptr = '\0';

    for (ptr = new_repo_id; *ptr; ptr++)
	if (*ptr == '/')
	    *ptr = ':';

    if (format) {
	retval = g_strdup_printf (format, new_repo_id);
	g_free (new_repo_id);
    } else
	retval = new_repo_id;

    return retval;
}

gchar *
guile_corba_generic_make_type_name (const gchar *StudlyCaps)
{
    static SCM de_studly_capsify = SCM_BOOL_F;
    SCM ret;
    
    if (SCM_FALSEP (de_studly_capsify)) {
        de_studly_capsify = SCM_VARIABLE_REF
            (scm_c_module_lookup (scm_c_resolve_module ("gnome gobject utils"),
                                  "gtype-name->class-name"));
    }
    
    ret = scm_call_1 (de_studly_capsify, scm_from_locale_string (StudlyCaps));

    return scm_to_locale_string (scm_symbol_to_string (ret));
}

static gchar *
make_enum_name (const gchar *repo_id, const gchar *name)
{
    gchar *new_repo_id, *new_name, *retval;

    new_repo_id = guile_corba_generic_repo_id_to_name (NULL, repo_id);
    new_name = guile_corba_generic_make_type_name (name);

    retval = g_strdup_printf ("%s:%s", new_repo_id, new_name);

    g_free (new_repo_id);
    g_free (new_name);

    return retval;
}

GType
guile_corba_generic_typecode_to_type (CORBA_TypeCode tc)
{
    CORBA_TypeCode real_tc;
    GType retval;

    retval = GPOINTER_TO_INT (g_hash_table_lookup (typecode_type_hash, tc->repo_id));
    if (retval)
	return retval;

    retval = G_TYPE_NONE;

    real_tc = tc;
    while (real_tc->kind == CORBA_tk_alias)
	real_tc = real_tc->subtypes [0];

    switch (real_tc->kind) {
    case CORBA_tk_enum: {
	GEnumValue *values;
	gchar *name, *c;
	guint i;

	values = g_new0 (GEnumValue, real_tc->sub_parts + 1);
	for (i = 0; i < real_tc->sub_parts; i++) {
	    values [i].value = i;
	    values [i].value_name = make_enum_name (tc->repo_id, real_tc->subnames [i]);
	    values [i].value_nick = guile_corba_generic_make_type_name (real_tc->subnames [i]);
	}

	name = guile_corba_generic_repo_id_to_name (NULL, tc->repo_id);
	for (c = name; *c; c++)
	    if (*c == ':') *c = '+';

	retval = g_enum_register_static (name, values);
	g_type_set_qdata (retval, quark_corba_typecode, real_tc);
	g_hash_table_insert (typecode_type_hash, real_tc->repo_id,
			     GINT_TO_POINTER (retval));
	g_free (name);

	break;
    }

    default:
	break;
    }

    return retval;
}

CORBA_TypeCode
guile_corba_generic_type_to_typecode (GType type)
{
    CORBA_TypeCode tc;

    tc = g_type_get_qdata (type, quark_corba_typecode);
    if (!tc)
	tc = TC_null;

    return tc;
}

void
scm_pre_init_gnome_corba_generic (void)
{
    quark_corba_typecode = g_quark_from_static_string ("%corba-typecode");

    typecode_type_hash = g_hash_table_new (g_str_hash, g_str_equal);
}
