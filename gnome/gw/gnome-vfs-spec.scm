;; guile-gnome
;; Copyright (C) 2004 Free Software Foundation, Inc.

;; This program is free software; you can redistribute it and/or    
;; modify it under the terms of the GNU General Public License as   
;; published by the Free Software Foundation; either version 2 of   
;; the License, or (at your option) any later version.              
;;                                                                  
;; This program is distributed in the hope that it will be useful,  
;; but WITHOUT ANY WARRANTY; without even the implied warranty of   
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the    
;; GNU General Public License for more details.                     
;;                                                                  
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, contact:
;;
;; Free Software Foundation           Voice:  +1-617-542-5942
;; 59 Temple Place - Suite 330        Fax:    +1-617-542-2652
;; Boston, MA  02111-1307,  USA       gnu@gnu.org

;;; Commentary:
;;
;;g-wrap specification for gnome-vfs.
;;
;;; Code:

(define-module (gnome gw gnome-vfs-spec)
  #:use-module (oop goops)
  #:use-module (g-wrap)
  #:use-module (g-wrap guile)
  #:use-module (gnome gw gobject-spec)
  #:use-module (gnome gw support defs)
  #:use-module (gnome gw support gobject))

(define-class <gnome-vfs-wrapset> (<gobject-wrapset-base>)
  #:id 'gnome-gnome-vfs
  #:dependencies '(standard gnome-glib gnome-gobject))

(define-method (initialize (ws <gnome-vfs-wrapset>) initargs)
  (next-method ws (cons #:module (cons '(gnome gw gnome-vfs) initargs)))
  
  (add-type! ws (make <gnome-vfs-result-type>
                  #:gtype-id "GNOME_VFS_TYPE_VFS_RESULT"
                  #:ctype "GnomeVFSResult"
                  #:c-type-name "GnomeVFSResult"
                  #:c-const-type-name "GnomeVFSResult"
                  #:ffspec 'uint
                  #:wrapped "Custom"))
  (add-type-alias! ws "GnomeVFSResult" '<gnome-vfs-result>)

  ;; these are platform-dependent -- FIXME.
  (add-type-alias! ws "GnomeVFSFileSize" 'unsigned-long-long)
  (add-type-alias! ws "GnomeVFSFileOffset" 'long-long)
  (load-defs-with-overrides ws "gnome/defs/gnome-vfs.defs"))

(define-method (global-declarations-cg (self <gnome-vfs-wrapset>))
  (list (next-method)
        "#include <libgnomevfs/gnome-vfs.h>\n"
        "#include <libgnomevfs/gnome-vfs-enum-types.h>\n"
        "#include <libgnomevfs/gnome-vfs-mime-handlers.h>\n"
        "#include <libgnomevfs/gnome-vfs-application-registry.h>\n"
        "#include \"gnome-vfs-support.h\"\n"))

(define-method (initializations-cg (self <gnome-vfs-wrapset>) err)
  (list
   (next-method)
   "gnome_vfs_init ();\n"
   "g_type_class_ref (GNOME_VFS_TYPE_VFS_RESULT);\n"))

;; if a GnomeVFSResult return value is not GNOME_VFS_OK, throw an error.
(define-class <gnome-vfs-result-type> (<gobject-classed-type>))

(define-method (make-typespec (type <gnome-vfs-result-type>) (options <list>))
  (next-method type (cons 'caller-owned options)))

(define-method (unwrap-value-cg (type <gnome-vfs-result-type>)
                                (value <gw-value>)
                                status-var)
  (let ((c-var (var value))
        (scm-var (scm-var value))
        (gtype-id (gtype-id type)))
    (list
     "if (SCM_TYP16_PREDICATE (scm_tc16_gvalue, " scm-var ")\n"
     "    && G_VALUE_HOLDS ((GValue*)SCM_SMOB_DATA (" scm-var "), " gtype-id "))\n"
     "  " c-var " = g_value_get_enum ((GValue*)SCM_SMOB_DATA (" scm-var "));\n"
     "else {\n"
     ;; will throw an exception if the conversion fails
     ;; don't use scm_c_scm_to_gvalue because that will unecessarily
     ;; create a new value
     "  SCM newval = scm_scm_to_gvalue (scm_c_register_gtype (" gtype-id "), "
     scm-var ");\n"
     "  " c-var " = g_value_get_enum ((GValue*)SCM_SMOB_DATA (newval));\n"
     "}\n")))

(define-method (wrap-value-cg (type <gnome-vfs-result-type>)
                                    (result <gw-value>)
                                    status-var)
  (let ((c-var (var result))
        (scm-var (scm-var result)))
    (list
     "if (" c-var " == GNOME_VFS_OK)\n"
     "  " scm-var " = SCM_UNSPECIFIED;\n"
     "else\n"
     "  scm_throw (scm_str2symbol (g_enum_get_value ("
     "(GEnumClass*)g_type_class_peek (GNOME_VFS_TYPE_VFS_RESULT), " c-var
     ")->value_nick), SCM_EOL);\n")))
