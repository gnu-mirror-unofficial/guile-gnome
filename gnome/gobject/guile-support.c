#include <guile-support.h>
#include <string.h>

SCM 
scm_str2string (const char *src)
{
  if (!src) return SCM_BOOL_F;
  return scm_mem2string (src, strlen (src));
}

SCM 
scm_str2symbol (const char *src)
{
  if (!src) return SCM_BOOL_F;
  return scm_mem2symbol (src, strlen (src));
}

