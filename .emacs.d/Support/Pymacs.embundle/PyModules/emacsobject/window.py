#encoding:utf8
"""
:file:    emacswindow.py
:author:  yjzeng
:created: 2011-5-24
"""

from Pymacs import lisp
from ._e2p import EmacsObject, map2py


class Window(EmacsObject):
    """封装Emacs的窗口对象；这是Pymacs的不足，所以通过Emacs Lisp的变量
    名直接维护"""

    def get_window_buffer(self):
        return map2py('(window-buffer %s)' % self._v)


    def set_window_buffer(self, buffer_object):
        lisp('(set-window-buffer %s %s)' %
             (self.varname, buffer_object.varname))

    window_buffer = property(get_window_buffer, set_window_buffer)


    def delete_window(self):
        lisp('(delete-window %s)' % self._v)


    def select_window(self):
        lisp('(select-window %s)' % self._v)


    def split_window_vertically(self):
        lisp('(select-window %s)' % self._v)
        return map2py('(split-window-vertically)')

    @property
    def buffer(self):
        return map2py('(window-buffer %s)' % self._v)


    def set_window_point(self, pt):
        lisp('(set-window-point %s %d)' % (self._v, pt))


    def get_window_point(self):
        return lisp('(window-point %s)' % self._v)

    window_point = property(get_window_point, set_window_point)


def selected_window():
    return map2py('(selected-window)')
