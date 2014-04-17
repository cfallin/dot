#!/usr/bin/python2

import sys
import os
import re

def open_url(url):
    os.system("xdg-open %s" % url)

if len(sys.argv) > 1:
    f = open(sys.argv[1], 'r')
else:
    f = sys.stdin

r = re.compile('(https?://\S+)')
for line in f.readlines():
   matches = re.findall(r, line)
   for m in matches:
       open_url(m)
