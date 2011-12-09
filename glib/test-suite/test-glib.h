#if !defined(_INC_TEST_SUITE_TEST_GLIB_H)
#define _INC_TEST_SUITE_TEST_GLIB_H

#include <glib.h>

G_BEGIN_DECLS

/* Just return the argument */
GList *test_glist_echo (GList *list);

G_END_DECLS


#endif
