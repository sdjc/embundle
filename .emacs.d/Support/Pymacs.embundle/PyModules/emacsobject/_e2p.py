#coding: utf-8
'''
_e2p.py
2011-6-8

map emacs lisp type/object to python type/object
'''

from Pymacs import lisp
from .utils import raw_trace


_objpool = {}
_varcount = 0


class EmacsObject(object):
    '''
    所有Emacs对象对应的Python对象的基类，注意，这个东西能通
    过_e2p.getobject来获得，不要自己创建这个实例，因为我还没实现用这个
    东西来控制Emacs。
    '''


    def __init__(self, var, handle):
        object.__init__(self)
        self._v = var
        self._h = handle


    def __getattr__(self, key):
        '''
        特殊的读取对象的方式
        '''
        # 从emacs lisp中读取这个东西
        val = lisp(key)
        # 把val转换成python类型
        # todo: 假定这里读出来的东西不是对象类型
        raw_trace('%s %s' % (type(val), str(val))) ####
        return val


    def get_varname(self):
        return self._v


    varname = property(get_varname)


    @property
    def handle(self):
        return self._h


    @staticmethod
    def extract_handle(hash_):
        return hash_


    @staticmethod
    def get_handle(_hash):
        """是这样的，每种不同类型的东西在Emacs中的表示方法是不同的，因
        此这个函数将在不同的派生类中被重载"""
        return _hash


def getlist(lispstat):
    """
    从Emacs中获得一个列表对象，这里假设这个列表中放着的都是因为对象，列
    表不存在嵌套，比如(frame-list), (buffer-list)
    """
    result = []
    one = lisp(lispstat)
    for i in range(len(one)):
        # 如果对象不存在就创建
        obj = map2py('(nth %d %s)' % (i, lispstat))
        result.append(obj)
    return result


def map2py(lispstat):
    '''
    假设不作任何缓存；即使做，这里的接口也不变，这样才能使上面那一层足
    够灵活
    '''
    varname = _getvarname()
    lisp('(setq %s %s)' % (varname, lispstat))
    ehash = lisp('(prin1-to-string %s)' % varname)

    # 从ehash中提取出类型信息
    tp = _gettypefromhash(ehash)
    handle = tp.get_handle(ehash)

    # 好吧，不让同一个hash存在一个以上的实例
    try:
        return _objpool[handle]
    except KeyError:
        obj = tp(varname, handle)
        _objpool[varname] = obj
        _objpool[handle]  = obj
        return obj


def _getvarname():
    v = 'var-%d' % _varcount
    globals()['_varcount'] += 1
    return v


def _gettypefromhash(hashv):
    '''
    把hashv中带的类型信息映射到Python类型上

    这里先不使用任何的优化手段。
    '''
    from .frame import Frame
    from .buffer_ import Buffer
    from .process import Process
    from .window import Window
    if 'frame' in hashv:
        return Frame
    elif 'buffer' in hashv:
        return Buffer
    elif 'window' in hashv:
        return Window
    elif 'process' in hashv:
        return Process
    else:
        raise KeyError()
