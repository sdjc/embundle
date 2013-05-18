# encoding: utf-8
'''
'''

from Pymacs import lisp
# import emacsobject
from .pwgenerator import generate, generate_g1, generate_g3


# 我终于明白了，新版本的Pymacs中把从emacs中来的字符转换成unicode，而下
# 面的代码逻辑依据的是utf-8字符串，所以出了问题；果然，现在ropemacs也有
# 问题，我想升级一下rope
def pymacs_generate(pw, domain):
    _generatewith(pw, domain, generate)


def pymacs_generate_g1(pw, domain):
    _generatewith(pw, domain, generate_g1)


def pymacs_generate_g3(pw, domain):
    _generatewith(pw, domain, generate_g3)
    

def _generatewith(pw, domain, func):
    hashcode = func(pw.encode('utf-8'),
                    domain.encode('utf-8')).decode('utf-8')
    lisp.insert(hashcode)
    lisp.message(hashcode)


def import_from_pymacs():
    '''判断是否是从Pymacs中启动的'''
    import inspect
    import sys
    frame = sys._getframe()
    while frame:
        if inspect.getfile(frame).rstrip('c').endswith('pymacs.py'):
            return True
        frame = frame.f_back


if import_from_pymacs():
    globals()['interactions'] = {
        pymacs_generate: '',
        pymacs_generate_g1: '',
        pymacs_generate_g3: ''}
