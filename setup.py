#!/usr/bin/env python

import os
import os.path
import sys

archival_path = ''
if len(sys.argv) > 1:
    archival_path = sys.argv[1]
    print('Using archival path: %s' % archival_path)

skip = ['setup.py', '..', '.', '.git', '.gitmodules', '.gitignore']
for entry in os.listdir('.'):
    if entry in skip: continue
    dest = '../%s' % entry
    linktarg = 'dot/%s' % entry
    archival = '%s/%s' % (archival_path, entry)

    if os.path.exists(dest):
        if os.path.islink(dest) and os.readlink(dest) == linktarg:
            print("Destination '%s' already exists as link." % entry)
            continue
        elif archival_path != '' and not os.path.exists(archival):
            print("Archiving '%s' to archival path." % entry)
            os.system('mv %s %s' % (dest, archival))
        else:
            print("Destination '%s' exists and no archival path specified, or archival location exists." % entry)
            continue

    if os.path.exists(dest): # move may have failed
        continue

    if os.system('ln -s %s %s' % (linktarg, dest)) != 0:
        print "Link failed: destination '%s'." % entry
