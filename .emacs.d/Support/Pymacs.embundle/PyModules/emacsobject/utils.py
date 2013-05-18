#encoding:utf8
"""
:file:    emacsobject/emacsaccess.py
:author:  yjzeng
:created: 2011-5-25

把这个模块的东西以对象来区分，这样会更方便使用。
"""


from Pymacs import lisp


def message(msg):
    lisp.message(msg)


def raw_trace(msg, filename='emacsobject.log'):
    '''
    因为message也好，print也好，都存在着通过pymacs传递消息的调用，消息
    传递存在着编码、解码之类的开销，因此直接输出到文件中可能更快
    '''
    with file(filename, 'a') as out:
        print >>out, msg
