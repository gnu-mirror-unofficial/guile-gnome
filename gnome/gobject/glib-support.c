#include "glib-support.h"

#define GRUNTIME_ERROR(format, func_name, args...) \
  scm_error (scm_str2symbol ("gruntime-error"), func_name, format, \
             ##args, SCM_EOL)

static SCM deliver_signals;

void
scm_init_glib (void)
{
    deliver_signals = scm_permanent_object (SCM_VARIABLE_REF (scm_c_lookup ("%deliver-signals")));
}

static gboolean
handle_signals (gpointer unused) 
{
    scm_call_0 (deliver_signals);
    return TRUE;
}

static void
add_interrupt_handler (guint *timeout)
{
    /* process interrupts every tenth of a second */
    *timeout = g_timeout_add (100, handle_signals, NULL);
}

static SCM
run_loop (GMainLoop *loop)
{
    g_main_loop_run (loop);
    return SCM_UNSPECIFIED;
}

static void
remove_interrupt_handler (guint *timeout)
{
    g_source_remove (*timeout);
    *timeout = 0;
}

void
_wrap_g_main_loop_run (GMainLoop *loop)
{
    guint timeout;

    scm_internal_dynamic_wind ((scm_t_guard)add_interrupt_handler,
                               (scm_t_inner)run_loop,
                               (scm_t_guard)remove_interrupt_handler,
                               loop,
                               &timeout);
}
