#!/usr/bin/env python
'''

'''

from sys import argv
from getopt import getopt
from os import path
import hashlib, hmac


def generate_g1(_pw, _domain):
    return 'xxx' ####
    

def generate_g2(pw, domain):
    SALT = '$$'
    op = '%s%s%s' % (pw, domain, SALT)
    mobj = hmac.new(domain, op, hashlib.md5)
    m = mobj.hexdigest()
    L = 8 + len(op) % 5
    return m[0:L]
    

def generate_g3(pw, domain):
    SALT = '$$'
    op = '%s%s%s' % (pw, domain, SALT)
    mobj = hmac.new(domain, op, hashlib.md5)
    m = mobj.hexdigest()
    s = 2 + len(op) % 5
    L = 8 + s #len(op) % 5
    # toupper
    r = []
    for i, c in enumerate(m[0:L]):
        if i % s == 0:
            r.append(c.upper())
        else:
            r.append(c)
    return ''.join(r)
    

# for call from other module
generate = generate_g2


# run as main
if __name__ == '__main__':
    sel_fn = generate
    opts, args = getopt(argv[1:], 'G:')
    for opt, arg in opts:
        if opt in ('-G'):
            sel_fn = eval('generate_g%d' % arg)
    try:
        print sel_fn(args[0], args[1])
    except IndexError:
        print 'Usage:', path.basename(__file__), '[-G1|-G2|-G3]', \
            'passwd', 'host'
        print ' '*6, 'default to -G2'
