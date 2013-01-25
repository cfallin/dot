#!/usr/bin/python
import sys

pre = []
args = []

for l in sys.stdin.readlines():
    l = l.strip()
    if l.find('=') == -1:
        continue
    k = l.split('=')[0].strip()
    if k == 'Arguments' or k == 'arguments': args.append(l)
    else: pre.append(l)

for p in pre:
    print p
args.reverse()
for a in args:
    print a
    print "queue"
    print
