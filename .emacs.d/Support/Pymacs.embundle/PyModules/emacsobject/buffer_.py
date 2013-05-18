#encoding:utf-8
"""
:file:    emacsbuffer.py
:author:  yjzeng
:created: 2011-5-24

感觉Pymacs封装的Emacs Lisp不厚道，想来想去，我决定采用这样的策略:

  1. 同时使用Emacs Lisp和Python，中间通过Pymacs通讯；
  2. 对于简单变量(字符串、整数)，Pymacs已经封装得很好了，直接用；
  3. 对于复杂变量(window, buffer)，我决定不使用Pymacs那种半封装半没封装
  的，而使用Python对象封装Lisp变量名，通过Lisp原生代码直接进行操作，仅
  仅使用Python进行封装。

其实算来Emacs Lisp中也只有为数不多的几种"复杂对象"。
"""

from Pymacs import lisp
# from . import _e2p
from ._e2p import EmacsObject, map2py, getlist
from .utils import raw_trace


class Buffer(EmacsObject):


    def __getattr__(self, key):
        '''
        读取
        '''
        raw_trace('-- __getattr__ %s' %key) ####
        if key.startswith('_'):
            raise AttributeError()
        try:
            return getattr(self, '_' + key)
        except AttributeError:
            key = key.replace('_', '-')
            try:
                return self._read_local(key)
            except AttributeError:
                return EmacsObject.__getattr__(self, key)


    def get_buffer_window(self):
        return map2py('(get-buffer-window %s)' % self._v)


    def get_buffer_process(self):
        '''这个buffer中运行着的进程对象'''
        return map2py('(get-buffer-process %s)' % self._v)


    def insert_text(self, t):
        lisp('(set-buffer %s)' % self._v)
        lisp.insert(t) ####


    def kill_buffer(self):
        lisp('(kill-buffer %s)' % self._v)
        del self


    def set_buffer(self):
        '''使buffer实例成为"当前"的'''
        lisp('(set-buffer %s)' % self._v)


    def get_region(self):
        '''
        获得这个buffer对象中被选中的部分文字
        '''
        lisp('(set-buffer %s)' % self.varname)
        b, e = lisp.region_beginning(), lisp.region_end()
        s = lisp.buffer_substring(b, e)
        return s


    def _read_local(self, key):
        '''读取本buffer的变量值'''
        raw_trace('_read_local %s' % key) ####
        r = lisp("(local-variable-if-set-p '%s %s)" % (key, self.varname))
        raw_trace('r=%s' % str(r)) ####
        if not r:
            raise AttributeError()
        return lisp("(buffer-local-value '%(key)s %(var)s)" %
                    {'key':key, 'var': self.varname})


    def _write_local(self, key, value):
        '''设置本buffer的变量值'''
        # 这个value有点特殊
        if isinstance(value, str):
            tranvalue = '"' + value + '"'
        elif isinstance(value, unicode):
            tranvalue = '"' + value.encode('utf-8') + '"'
        else:
            tranvalue = str(value)

        lisp('''
(with-current-buffer %(varname)s
  (setq %(key)s %(value)s))''' %
             {'varname': self.varname,
              'key': key,
              'value': tranvalue})


def buffer_list():
    '''
    从emacs中获得buffer列表，映射成emacsobjct.Buffer

    emacsobject本身还分两个层次:

      1. 一个是基本的对象层面: 把emacs对象映射成python对象；

      2. 一个是在这里的"高级"的对象层面: 压根就不用关心下面那层干了什么
         样的操作，它只需要知道我要获得一个对象

    如执行下面的语句:

      _e2p.getlist('(buffer-list)')

    应该发生几件事情:

      1. 返回一个Python的列表对象；
      2. 列表中的所有对象也被转换成Python对象；
    '''
    return getlist('(buffer-list)')


def current_buffer():
    return map2py('(current-buffer)')


def get_buffer_create(buffer_name):
    return map2py('(get-buffer-create "%s")' % buffer_name)
