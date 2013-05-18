# encoding: utf-8
'''
editingtypes.py
2011-7-26

对Emacs中其他不常用的类型进行建模
'''


from .base import EmacsObject


class Terminal(EmacsObject):
    pass


class Marker(EmacsObject):
    pass


class Stream(EmacsObject):
    pass


class Keymap(EmacsObject):
    pass


class Overlay(EmacsObject):
    pass


class Font(EmacsObject):
    pass


# Window Configuration
# Frame Configuration
