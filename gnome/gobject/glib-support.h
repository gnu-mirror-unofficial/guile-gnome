#if !defined(_GUILE_GOBJECT_GLIB_SUPPORT_H)
#define _GUILE_GOBJECT_GLIB_SUPPORT_H

#include <glib.h>
#include <libguile.h>

G_BEGIN_DECLS

void scm_init_glib (void);
void _wrap_g_main_loop_run (GMainLoop *loop);

G_END_DECLS

#endif
