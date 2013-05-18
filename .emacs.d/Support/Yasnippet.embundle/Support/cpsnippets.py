# encoding: utf-8
'''
从各个.embundle中的Snippets内容复制到./snippets中
'''


import os
from subprocess import call


for r, ds, fs in os.walk('../../..'):
    for d in ds:
        if not d.endswith('.embundle'):
            continue

        snippets = os.path.join(r, d, 'Snippets')
        if not os.path.isdir(snippets):
            continue

        call('rsync -av %s/* ../../snippets' % snippets,
             shell=True)
