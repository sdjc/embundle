# encoding: utf-8
# profiling/__init__.py
# 2011-8-31


def ticked(func):
    """
    A decorator to tick any function call.
    Usage:
    @ticked
    def foo(): pass

    Outputs:
    --- { foo
    --- } 0ms foo

    注意：

    Pymacs的interactions采用了取函数名称属性的做法，在还没搞明白其原理
    之前，先不要在通过Pymacs暴露给Emacs的函数上使用这个修饰。
    """
    from tick import Tick
    def _ticked(*args, **kwargs):
        t = Tick(func.__name__)
        return func(*args, **kwargs)
    return _ticked
