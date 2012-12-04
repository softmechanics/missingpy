/* arch-tag: Python Utility Functions, header file
Copyright (C) 2005 John Goerzen <jgoerzen@complete.org>

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

#include <Python.h>
extern void hspy_decref(PyObject *o);
extern void hspy_incref(PyObject *o);
extern PyObject ** hspy_getexc();
extern int hspy_list_check(PyObject *o);
extern int hspy_tuple_check(PyObject *o);
extern PyObject *hspy_none(void);
extern PyObject *hspy_true(void);
extern PyObject *hspy_false(void);

/* These are now macros */
extern PyObject *glue_PyMapping_Keys(PyObject *o);
extern PyObject *glue_PyMapping_Items(PyObject *o);
