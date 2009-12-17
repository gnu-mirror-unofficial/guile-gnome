/* guile-gnome
 * Copyright (C) 2004, 2009 Free Software Foundation, Inc.
 *
 * gnome-vfs-support.c: Support routines for the gnome-vfs wrapper
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

#include "gnome-vfs-port.h"
#include "gnome-vfs-support.h"
#include "guile-gnome-gobject.h"
#include <stdio.h>
#include <string.h>


/* Define this macro if Guile 1.7.x or better is in use. */
#if defined (SCM_MINOR_VERSION) && (SCM_MINOR_VERSION >= 7) && \
    defined (SCM_MAJOR_VERSION) && (SCM_MAJOR_VERSION >= 1)
#define SCM_VERSION_17X 1
#endif

#if SCM_MAJOR_VERSION == 1 && SCM_MINOR_VERSION < 9
typedef off_t scm_t_off;
#endif

/* Support for coding against Guile 1.7 */
#ifndef SCM_VERSION_17X

#define scm_gc_malloc(size, what) scm_must_malloc((size), (what))
#define scm_gc_free(mem, size, what) \
  do{ scm_must_free (mem); scm_done_free (size); } while (0)

#define LOCK SCM_DEFER_INTS
#define UNLOCK SCM_ALLOW_INTS

static SCM
scm_new_port_table_entry (scm_t_bits tag)
#define FUNC_NAME "scm_new_port_table_entry"
{
    SCM port;
    scm_t_port *pt;
    
    SCM_NEWCELL (port);
    pt = scm_add_to_port_table (port);
    SCM_SET_CELL_TYPE(port, tag);
    SCM_SETPTAB_ENTRY (port, pt);
    return port;
}
#undef FUNC_NAME

#else /* SCM_VERSION_17X */

#define LOCK scm_i_pthread_mutex_lock (&scm_i_port_table_mutex)
#define UNLOCK scm_i_pthread_mutex_unlock (&scm_i_port_table_mutex)

#endif /* SCM_VERSION_17X */

static scm_t_bits scm_tc16_vport = 0;

#define CHECK_RESULT(res) \
    do{if (res != GNOME_VFS_OK) RESULT_ERROR (res);}while(0)

/* Most of this taken from guile's vports.c */

/* default buffer size, used if the O/S won't supply a value.  */
static const size_t default_buffer_size = 1024;

/* create VPORT buffer with specified sizes (or -1 to use default size or
   0 for no buffer.  */
static void
scm_vport_buffer_add (SCM port, long read_size, int write_size)
#define FUNC_NAME "scm_vport_buffer_add"
{
    scm_t_port *pt = SCM_PTAB_ENTRY (port);

    if (read_size == -1 || write_size == -1) {
        if (read_size == -1)
            read_size = default_buffer_size;
        if (write_size == -1)
            write_size = default_buffer_size;
    }

    if (SCM_INPUT_PORT_P (port) && read_size > 0) {
        pt->read_buf = scm_gc_malloc (read_size, "port buffer");
        pt->read_pos = pt->read_end = pt->read_buf;
        pt->read_buf_size = read_size;
    } else {
        pt->read_pos = pt->read_buf = pt->read_end = &pt->shortbuf;
        pt->read_buf_size = 1;
    }

    if (SCM_OUTPUT_PORT_P (port) && write_size > 0) {
        pt->write_buf = scm_gc_malloc (write_size, "port buffer");
        pt->write_pos = pt->write_buf;
        pt->write_buf_size = write_size;
    } else {
        pt->write_buf = pt->write_pos = &pt->shortbuf;
        pt->write_buf_size = 1;
    }

    pt->write_end = pt->write_buf + pt->write_buf_size;
    if (read_size > 0 || write_size > 0)
        SCM_SET_CELL_WORD_0 (port, SCM_CELL_WORD_0 (port) & ~SCM_BUF0);
    else
        SCM_SET_CELL_WORD_0 (port, SCM_CELL_WORD_0 (port) | SCM_BUF0);
}
#undef FUNC_NAME

static long
vfs_mode_bits (GnomeVFSOpenMode mode)
{
    long ret = SCM_OPN;
    if (mode & GNOME_VFS_OPEN_READ) ret |= SCM_RDNG;
    if (mode & GNOME_VFS_OPEN_WRITE) ret |= SCM_WRTNG;
    return ret;
}

SCM
scm_gnome_vfs_handle_to_port (GnomeVFSHandle *handle, GnomeVFSOpenMode mode,
                              const gchar* uri)
#define FUNC_NAME "scm_gnome_vfs_handle_to_port"
{
    long mode_bits = vfs_mode_bits (mode);
    SCM port;
    scm_t_port *pt;

    LOCK;

    port = scm_new_port_table_entry (scm_tc16_vport);
    SCM_SET_CELL_TYPE(port, scm_tc16_vport | mode_bits);
    pt = SCM_PTAB_ENTRY(port);
    pt->rw_random = mode & GNOME_VFS_OPEN_RANDOM;
    SCM_SETSTREAM (port, handle);
    if (mode_bits & SCM_BUF0)
        scm_vport_buffer_add (port, 0, 0);
    else
        scm_vport_buffer_add (port, -1, -1);
    SCM_SET_FILENAME (port, scm_makfrom0str (uri));

    UNLOCK;

    return port;
}
#undef FUNC_NAME

GnomeVFSHandle*
scm_gnome_vfs_port_to_handle (SCM port)
#define FUNC_NAME "scm_gnome_vfs_port_to_handle"
{
    if (SCM_VPORTP (port)) {
        if (SCM_OPVPORTP (port))
            return (GnomeVFSHandle*)SCM_STREAM (port);
        else
            return NULL;
    }
    scm_wrong_type_arg (FUNC_NAME, 1, port);
    return NULL; /* shouldn't get here */
}
#undef FUNC_NAME


static int 
vport_print (SCM exp, SCM port, scm_print_state *pstate SCM_UNUSED)
{
    scm_puts ("#<", port);
    scm_print_port_mode (exp, port);    
    if (SCM_OPVPORTP (exp)) {
        SCM name = SCM_FILENAME (exp);
        if (scm_is_string (name) || SCM_SYMBOLP (name))
            scm_display (name, port);
        else
            scm_puts (SCM_PTOBNAME (SCM_PTOBNUM (exp)), port);
        scm_putc (' ', port);
        scm_intprint (SCM_STREAM (exp), 16, port);
    }
    scm_putc ('>', port);
    return 1;
}

static void vport_flush (SCM port);

/* fill a port's read-buffer with a single read.  returns the first
   char or EOF if end of file.  */
static int
vport_fill_input (SCM port)
{
    GnomeVFSFileSize count;
    GnomeVFSResult res;
    scm_t_port *pt = SCM_PTAB_ENTRY (port);
    GnomeVFSHandle *handle = (GnomeVFSHandle*)SCM_STREAM (port);

    res = gnome_vfs_read (handle, pt->read_buf, pt->read_buf_size, &count);
    if (res == GNOME_VFS_ERROR_EOF)
        return EOF;
    else if (res != GNOME_VFS_OK)
        RESULT_ERROR (res);
    
    if (count == 0)
        return EOF;
    else
        {
            pt->read_pos = pt->read_buf;
            pt->read_end = pt->read_buf + count;
            return *pt->read_buf;
        }
}

static scm_t_off
vport_seek (SCM port, scm_t_off offset, int whence)
{
    GnomeVFSFileSize count;
    GnomeVFSResult res;
    scm_t_port *pt = SCM_PTAB_ENTRY (port);
    GnomeVFSHandle *handle = (GnomeVFSHandle*)SCM_STREAM (port);
    GnomeVFSSeekPosition gwhence;
    
    switch (whence) {
    case SEEK_SET:
        gwhence = GNOME_VFS_SEEK_START; break;
    case SEEK_CUR:
        gwhence = GNOME_VFS_SEEK_CURRENT; break;
    case SEEK_END:
        gwhence = GNOME_VFS_SEEK_END; break;
    default:
        RESULT_ERROR (GNOME_VFS_ERROR_NOT_SUPPORTED); return 0;
    }

    if (pt->rw_active == SCM_PORT_WRITE) {
        if (offset != 0 || whence != SEEK_CUR) {
            vport_flush (port);
            res = gnome_vfs_seek (handle, gwhence, (GnomeVFSFileOffset)offset);
            CHECK_RESULT (res);
            res = gnome_vfs_tell (handle, &count);
            CHECK_RESULT (res);
            return count;
        } else {
            /* read current position without disturbing the buffer.  */
            res = gnome_vfs_seek (handle, gwhence, (GnomeVFSFileOffset)offset);
            CHECK_RESULT (res);
            res = gnome_vfs_tell (handle, &count);
            CHECK_RESULT (res);
            return count + (pt->write_pos - pt->write_buf);
        }
    } else if (pt->rw_active == SCM_PORT_READ) {
        if (offset != 0 || whence != SEEK_CUR) {
            /* could expand to avoid a second seek.  */
            scm_end_input (port);
            res = gnome_vfs_seek (handle, gwhence, (GnomeVFSFileOffset)offset);
            CHECK_RESULT (res);
            res = gnome_vfs_tell (handle, &count);
            CHECK_RESULT (res);
            return count;
        } else {
            /* read current position without disturbing the buffer
               (particularly the unread-char buffer).  */
            res = gnome_vfs_seek (handle, gwhence, (GnomeVFSFileOffset)offset);
            CHECK_RESULT (res);
            res = gnome_vfs_tell (handle, &count);
            CHECK_RESULT (res);
            count -= (pt->read_end - pt->read_pos);
            if (pt->read_buf == pt->putback_buf)
                count -= pt->saved_read_end - pt->saved_read_pos;
            return count;
        }
    } else { /* SCM_PORT_NEITHER */
        res = gnome_vfs_seek (handle, gwhence, (GnomeVFSFileOffset)offset);
        CHECK_RESULT (res);
        res = gnome_vfs_tell (handle, &count);
        CHECK_RESULT (res);
        return count;
    }
}

static void
vport_truncate (SCM port, scm_t_off length)
{
    GnomeVFSResult res;
    GnomeVFSHandle *handle = (GnomeVFSHandle*)SCM_STREAM (port);

    res = gnome_vfs_truncate_handle (handle, (GnomeVFSFileSize)length);
    CHECK_RESULT (res);
}

/* helper for vport_write: try to write data, using multiple system
   calls if required.  */
#define FUNC_NAME "write_all"
static void write_all (SCM port, const void *data, size_t remaining)
{
    GnomeVFSResult res;
    GnomeVFSHandle *handle = (GnomeVFSHandle*)SCM_STREAM (port);

    while (remaining > 0) {
        GnomeVFSFileSize done;

        res = gnome_vfs_write (handle, data, remaining, &done);
        
        /* gnome vfs handles the multiple-system-call case for us */
        CHECK_RESULT (res);
        remaining -= done;
        data = ((const char *) data) + done;
    }
}
#undef FUNC_NAME

static void
vport_write (SCM port, const void *data, size_t size)
{
    /* this procedure tries to minimize the number of writes/flushes.  */
    scm_t_port *pt = SCM_PTAB_ENTRY (port);
    scm_t_off space = pt->write_end - pt->write_pos;

    if (pt->write_buf == &pt->shortbuf
        || (pt->write_pos == pt->write_buf && size >= pt->write_buf_size)) {
        /* "unbuffered" port, or
           port with empty buffer and data won't fit in buffer. */
        write_all (port, data, size);
        return;
    }

    if (size <= space) {
        /* data fits in buffer.  */
        memcpy (pt->write_pos, data, size);
        pt->write_pos += size;
        if (pt->write_pos == pt->write_end) {
            vport_flush (port);
            /* we can skip the line-buffering check if nothing's buffered. */
            return;
        }
    } else {
        memcpy (pt->write_pos, data, space);
        pt->write_pos = pt->write_end;
        vport_flush (port);
        {
            const void *ptr = ((const char *) data) + space;
            size_t remaining = size - space;

            if (size >= pt->write_buf_size) {
                write_all (port, ptr, remaining);
                return;
            } else {
                memcpy (pt->write_pos, ptr, remaining);
                pt->write_pos += remaining;
            }
        }
    }

    /* handle line buffering.  */     
    if ((SCM_CELL_WORD_0 (port) & SCM_BUFLINE) && memchr (data, '\n', size))
        vport_flush (port);
}

/* becomes 1 when process is exiting: normal exception handling won't
   work by this time.  */
extern int scm_i_terminating; 

static void
vport_flush (SCM port)
{
    scm_t_port *pt = SCM_PTAB_ENTRY (port);
    GnomeVFSResult res;
    GnomeVFSFileSize count;
    GnomeVFSHandle *handle = (GnomeVFSHandle*)SCM_STREAM (port);
    unsigned char *ptr = pt->write_buf;
    long init_size = pt->write_pos - pt->write_buf;
    long remaining = init_size;

    while (remaining > 0) {
        res = gnome_vfs_write (handle, ptr, remaining, &count);

        if (res != GNOME_VFS_OK) {
            /* error.  assume nothing was written this call, but
               fix up the buffer for any previous successful writes.  */
            long done = init_size - remaining;
	      
            if (done > 0) {
                int i;

                for (i = 0; i < remaining; i++) 
                    *(pt->write_buf + i) = *(pt->write_buf + done + i);
                pt->write_pos = pt->write_buf + remaining;
            }
            if (scm_i_terminating) {
                fprintf (stderr, "Error: could not flush gnome-vfs handle %p",
                         handle);
                count = remaining;
            } else if (scm_gc_running_p) {
                /* silently ignore the error.  scm_error would abort if we
                   called it now.  */
                count = remaining;
            } else {
                CHECK_RESULT (res);
            }
        }
        ptr += count;
        remaining -= count;
    }
    pt->write_pos = pt->write_buf;
    pt->rw_active = SCM_PORT_NEITHER;
}

/* clear the read buffer and adjust the file position for unread bytes. */
static void
vport_end_input (SCM port, int offset)
{
    GnomeVFSResult res;
    GnomeVFSHandle *handle = (GnomeVFSHandle*)SCM_STREAM (port);
    scm_t_port *pt = SCM_PTAB_ENTRY (port);
  
    offset += pt->read_end - pt->read_pos;

    if (offset > 0) {
        pt->read_pos = pt->read_end;
        /* will throw error if unread-char used at beginning of file
           then attempting to write.  seems correct.  */
        res = gnome_vfs_seek (handle, GNOME_VFS_SEEK_CURRENT, -offset);
        CHECK_RESULT (res);
    }
    pt->rw_active = SCM_PORT_NEITHER;
}

static int
vport_close (SCM port)
{
    GnomeVFSResult res;
    GnomeVFSHandle *handle = (GnomeVFSHandle*)SCM_STREAM (port);
    scm_t_port *pt = SCM_PTAB_ENTRY (port);

    vport_flush (port);
    res = gnome_vfs_close (handle);
    SCM_SETSTREAM (port, NULL);
    if (res != GNOME_VFS_OK) {
        if (scm_gc_running_p)
            /* silently ignore the error.  scm_error would abort if we
               called it now.  */
            ;
        else
            CHECK_RESULT (res);
    }
    if (pt->read_buf == pt->putback_buf)
        pt->read_buf = pt->saved_read_buf;
    if (pt->read_buf != &pt->shortbuf)
        scm_gc_free (pt->read_buf, pt->read_buf_size, "port buffer");
    if (pt->write_buf != &pt->shortbuf)
        scm_gc_free (pt->write_buf, pt->write_buf_size, "port buffer");
    return 0;
}

static size_t
vport_free (SCM port)
{
    vport_close (port);
    return 0;
}

void
scm_init_gnome_vfs_ports ()
{
    scm_t_bits tc;
    
    tc = scm_make_port_type ("gnome-vfs-port", vport_fill_input, vport_write);

    scm_set_port_free            (tc, vport_free);
    scm_set_port_print           (tc, vport_print);
    scm_set_port_flush           (tc, vport_flush);
    scm_set_port_end_input       (tc, vport_end_input);
    scm_set_port_close           (tc, vport_close);
    scm_set_port_seek            (tc, vport_seek);
    scm_set_port_truncate        (tc, vport_truncate);
    /* no select(2) for gnomevfs
       scm_set_port_input_waiting   (tc, vport_input_waiting); */

    scm_tc16_vport = tc;
}
