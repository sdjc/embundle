#encoding:utf-8
"""
:file:    emacsframe.py
:author:  yjzeng
:created: 2011-5-24

感觉Pymacs封装的Emacs Lisp不厚道，想来想去，我决定采用这样的策略:

  1. 同时使用Emacs Lisp和Python，中间通过Pymacs通讯；
  2. 对于简单变量(字符串、整数)，Pymacs已经封装得很好了，直接用；
  3. 对于复杂变量(window, buffer)，我决定不使用Pymacs那种半封装半没封装
     的，而使用Python对象封装Lisp变量名，通过Lisp原生代码直接进行操作，
     仅仅使用Python进行封装。

其实算来Emacs Lisp中也只有为数不多的几种"复杂对象"。
"""

from Pymacs import lisp
from ._e2p import EmacsObject, map2py, getlist


class Frame(EmacsObject):

    @staticmethod
    def get_handle(hash_in_emacs):
        return hash_in_emacs.split(' ')[-1][:-2]

    @property
    def left(self):
        return lisp("(frame-parameter %s 'left)" % self._v)

    @property
    def top(self):
        return lisp("(frame-parameter %s 'top)" % self._v)

    def set_frame_position(self, left, top):
        lisp('(set-frame-position %s %d %d)' % (self._v, left, top))

    def select_frame_set_input_focus(self):
        lisp("(select-frame-set-input-focus %s)" % self._v)

    def window_list(self):
        lst = []
        for i, _w in enumerate(lisp('(window-list)')):
            wobj = map2py('(nth %d (window-list))' % i)
            lst.append(wobj)
        return lst


def selected_frame():
    return map2py('(selected-frame)')


def frame_list():
    """
    - 生成frame_list要造成非常大时间的开销
    - 当窗口数量发生改变时，需要重新缓存，否则就会出错
    """
    return getlist('(frame-list)')
