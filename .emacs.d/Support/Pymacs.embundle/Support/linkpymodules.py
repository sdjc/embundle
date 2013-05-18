# encoding: utf-8
'''
扫描所有的.embundle/PyModules，创建链接
'''


import glob
import os
from subprocess import call


for r, ds, fs in os.walk('../../..'):
    for d in ds:
        if not d.endswith('.embundle'):
            continue
        pympath = os.path.join(r, d, 'PyModules')
        if not os.path.isdir(pympath):
            continue
        for modulefile in os.listdir(pympath):
            fp = os.path.relpath(os.path.join(pympath, modulefile),
                                 '../../.pymodules')
            call('ln -sfv %s ../../.pymodules' % fp, shell=True)
