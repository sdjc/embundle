#encoding:utf8
"""
:file:    profiling/__init__.py
:author:  yjzeng
:created: 2011-5-26

其实我也很无奈，Python中居然没有我要的简单的计时功能?!我们仅关注于较大
的时间花销，暂时先忽略掉低于1ms的。
"""

import os
import time


class Tick:
    """
    用法是: 必须在一个函数中用一个变量t = Tick(函数名)；因为有变量t的存
    在，实例的引用祸首为1就不会被直接释放，而是在函数结束时释放，释放时
    正好打印出耗时。

    >>> t = Tick('prefix')
    >>> t.print_elapsed() # ms
    prefix 22.234234234
    """
    
    # _indent = 1
    
    def __init__(self, prefix, outputfilename="~/.profiling.log"):
        self._outputfilename = os.path.expanduser(outputfilename)
        self._prefix = prefix
        self._start = self._tick = time.time() * 1000
        self._indent = Tick._indent
        Tick._indent += 1
        self._output('{', '')
        
    def __del__(self):
        self._output('} %dms' % (time.time()*1000-self._start), '')
        Tick._indent -= 1
        
    def print_elapsed(self, mark=''):
        t = time.time() * 1000
        self._output('', '%s %d' % (mark, t - self._tick))
        self._tick = t
        
    def _output(self, lead, message):
        # 虽然这样频繁的打开关闭文件会有一定的开销，但是可以避免写入同一个文件时候需要锁的开销
        with file(self._outputfilename, 'ab') as out:
            print >>out, ' '*4*self._indent, lead, self._prefix, message
