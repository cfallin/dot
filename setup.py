#!/usr/bin/env python

import os
import os.path
import sys

os.system('git submodule foreach git pull')

archival_path = ''
if len(sys.argv) > 1:
    archival_path = sys.argv[1]
    print('Using archival path: %s' % archival_path)

def setup_link(dest, linktarg, archival):
    if os.path.exists(dest):
        if os.path.islink(dest) and os.readlink(dest) == linktarg:
            print("Destination '%s' already exists as link." % entry)
            return
        elif archival_path != '' and not os.path.exists(archival):
            print("Archiving '%s' to archival path." % entry)
            os.system('mv %s %s' % (dest, archival))
        else:
            print("Destination '%s' exists and no archival path specified, or archival location exists." % entry)
            return 

    if os.path.exists(dest): # move may have failed
        return

    if os.system('ln -s %s %s' % (linktarg, dest)) != 0:
        print "Link failed: destination '%s'." % entry

skip = ['setup.py', '..', '.', '.git', '.gitmodules', '.gitignore', 'bin']
for entry in os.listdir('.'):
    if entry in skip: continue
    dest = '../.%s' % entry
    linktarg = 'dot/%s' % entry
    archival = '%s/%s' % (archival_path, entry)
    setup_link(dest, linktarg, archival)

if not os.path.exists('../bin'):
    os.mkdir('../bin')
if archival_path != '' and not os.path.exists(archival_path + '/bin'):
    os.mkdir(archival_path + '/bin')

for entry in os.listdir('bin'):
    if entry in ['.', '..']: continue
    dest = '../bin/%s' % entry
    linktarg = '../dot/bin/%s' % entry
    archival = '%s/bin/%s' % (archival_path, entry)
    setup_link(dest, linktarg, archival)

os.system("chmod -R go-rwx ssh/")
