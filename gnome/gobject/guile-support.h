#ifndef __GUILE_SUPPORT_H__
#define __GUILE_SUPPORT_H__

#include <glib/gmacros.h>

#include <libguile.h>

G_BEGIN_DECLS

SCM scm_str2string (const char *src);
SCM scm_str2symbol (const char *src);

G_END_DECLS

#endif /* __GUILE_SUPPORT_H__ */
