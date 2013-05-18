# encoding:utf-8
"""
2011-5-26

对Emacs建立一个到Python的对象映射层。这个层依赖于Pymacs，但存在着很严重
的性能问题。

还有一个问题是不能很好的对这个层进行测试，依靠每次都要重启－打印的机制
来调试非常费劲。

发现一点: 在Python和Emacs之间的传输是瓶颈。
"""


def initialize():
    '''
    初始化整个emacsobject模块。

    这个方法暴露出来的原因是因为: 脚本在运行时并不知道是Emacs中调用，还
    是直接在Python Interpreter中，所以只能让.el的脚本来两次用这个东西。
    '''
    from Pymacs import lisp
    # 注册上这些回调，而emacsobject.el只需要关心怎么调试这个脚本
    lisp('''
(defun emacsobject-helper-notify-add-frame ()
  (setq gtran (selected-frame))
  (emacsobject-notify-add-frame))


(defun emacsobject-helper-notify-delete-frame (&rest args)
  (interactive)
  (setq gtran (selected-frame))
  (emacsobject-notify-delete-frame))


(defadvice make-frame (after after-make-frame () activate)
  (emacsobject-helper-notify-add-frame))


(add-hook 'delete-frame-hook 'emacsobject-helper-notify-delete-frame)
''')


def notify_add_frame():
    '''为了减少数据的传输，我们约定传过来的名称统一叫做gtran'''
    pass


def notify_delete_frame():
    '''gtran'''
    pass


def import_from_pymacs():
    '''判断是否是从Pymacs中启动的'''
    import inspect
    import sys
    frame = sys._getframe()
    while frame:
        if inspect.getfile(frame).rstrip('c').endswith('pymacs.py'):
            return True
        frame = frame.f_back


interactions = {
    notify_add_frame: '',
    notify_delete_frame: ''}


# 在不进行优化的情况下，我们只关心删除一个对象，而不关心是否创建一个对
# 象
if import_from_pymacs():
    initialize()
