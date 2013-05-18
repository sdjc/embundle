# encoding: utf-8
'''
emexperiment.py
2011-7-15

我想加一些功能，又只想通过Python快速的添加功能，于是现在这个.py文件中把
东西搞出来，等必要的时候再用elisp重写。

如果可能的话，我觉得em还是需要用Python来重写。需要用elisp重写的是em中已
经稳定下来的功能，而不是需要调整的功能，一旦废弃，使整个功能要废弃。
'''


import os
from emacsobject.utils import message
from emacsobject.buffer_ import buffer_list, current_buffer


# def groups_function():
#     """
#     用python实现的tabbar分组回调
#     """
#     return 'test' ####


def addkeyword(keyword):
    snippetdir = _findsnippetdir()
    try:
        os.mkdir(snippetdir)
    except OSError: # exists
        pass
    with file(os.path.join(snippetdir, keyword), 'w') as out:
        out.write(keyword)


def killbuffers(directory):
    '''
    根据一些条件kill掉buffer

    有几种特定情况有kill掉:

      1. 对应的文件已经被删除的；
      2. 某个目录下的所有文件；
    '''
    count = 0
    # 遍历所有buf
    for buf in buffer_list():
        # 如果满足下列的条件就kill掉
        filename = buf.buffer_file_name
        if not filename:
            # 这个buffer没有对应的文件
            continue
        if not os.path.exists(filename) or \
               _isparentdir(filename, directory):
            count += 1
            buf.kill_buffer()
    message('Kill %s buffer' % count)


def _findsnippetdir():
    major_mode = str(current_buffer().major_mode)
    if major_mode.startswith("'"):
        major_mode = major_mode[1:]
    snippetroot = os.path.expanduser('~/.emacs.d/snippets')
    for root, dirs, _files in os.walk(snippetroot):
        for d in dirs:
            if d == major_mode:
                return os.path.join(root, d)
    return os.path.join(snippetroot, major_mode)


def _isparentdir(filename, directory):
    realfilename = os.path.realpath(filename)
    realdirectory = os.path.realpath(directory)
    parts = realfilename.split('/')
    for i in range(1, len(parts)):
        xx = '/'.join(parts[:i+1])
        if xx == realdirectory:
            return True


interactions = {
    killbuffers: 'sKill buffers in directory: ',
    # groups_function: '',
    addkeyword: 'sKeyword: '}



# Performance test


# from Pymacs import lisp
# import time


# def trace(msg, filename='/Users/yjzeng/emlog.log'):
#     with file(filename, 'a') as out:
#         print >> out, msg



# def closesomebuffers():
#     tick1 = time.time()
#     for frm in lisp('(frame-list)'):
#         trace(str(frm)) ####
#     tick2 = time.time()
#     trace('cost=%f' % (tick2 - tick1)) ####


# interactions = {
#     closesomebuffers: ''}
