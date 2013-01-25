#!/usr/bin/python

import os
import sys

if len(sys.argv) < 3:
    print "Usage: ./uncondor.py [-a] new-submit-file.sub condor-ids.out"
    sys.exit(1)

args = sys.argv[1:]
all_jobs = False
if args[0] == '-a':
    all_jobs = True
    args = args[1:]

f = open(args[0], 'w')
ids = open(args[1], 'w')

f.write("Universe = Vanilla\n")
f.write("Error = log/$(cluster).$(process).err\n")
f.write("Log = log/$(cluster).$(process).log\n")
f.write("""
Requirements = (((Machine == "ece000.ece.cmu.edu") || (Machine == "ece001.ece.cmu.edu") || (Machine == "ece002.ece.cmu.edu") || (Machine == "ece003.ece.cmu.edu") || (Machine == "ece004.ece.cmu.edu") || (Machine == "ece005.ece.cmu.edu") || (Machine == "ece006.ece.cmu.edu") || (Machine == "ece007.ece.cmu.edu") || (Machine == "ece008.ece.cmu.edu") || (Machine == "ece009.ece.cmu.edu") || (Machine == "ece010.campus.ece.cmu.local") || (Machine == "ece011.campus.ece.cmu.local") || (Machine == "ece012.campus.ece.cmu.local") || (Machine == "ece013.campus.ece.cmu.local") || (Machine == "ece014.campus.ece.cmu.local") || (Machine == "ece015.campus.ece.cmu.local") || (Machine == "ece016.campus.ece.cmu.local") || (Machine == "ece017.campus.ece.cmu.local") || (Machine == "ece018.campus.ece.cmu.local") || (Machine == "ece019.campus.ece.cmu.local") || (Machine == "ece020.campus.ece.cmu.local") || (Machine == "ece021.campus.ece.cmu.local") || (Machine == "ece022.campus.ece.cmu.local") || (Machine == "ece023.campus.ece.cmu.local") || (Machine == "ece024.campus.ece.cmu.local") || (Machine == "ece025.campus.ece.cmu.local") || (Machine == "ece026.campus.ece.cmu.local") || (Machine == "ece027.campus.ece.cmu.local") || (Machine == "ece028.campus.ece.cmu.local") || (Machine == "ece029.campus.ece.cmu.local") || (Machine == "ece030.campus.ece.cmu.local") || (Machine == "ece031.campus.ece.cmu.local")) || (SAFARI_COMPUTE == True) || (SCOTCH_CLUSTER == True))
""")

lastexe = ''
lastiwd = ''

# get idle jobs
if all_jobs:
    constraint = 'TRUE'
else:
    constraint = '(JobStatus == 1)'

whoami = os.popen("whoami").readlines()[0].strip()
lines = os.popen("condor_q -long -constraint '%s' %s" % (constraint, whoami)).readlines()

# fields for each job
_cluster = ''
_proc = ''
_args = ''
_exe = ''
_iwd = ''

for line in lines:
    if line.strip() == '' and _proc != '':
        ids.write("%s.%s\n" % (_cluster, _proc))
        if _iwd != lastiwd:
            f.write("Initialdir = %s\n" % _iwd)
            lastiwd = _iwd
        if _exe != lastexe:
            f.write("Executable = %s\n" %_exe)
            lastexe = _exe
        f.write("Arguments = %s\nQueue\n\n" % _args)
        _proc = ''

    if line.find('=') == -1: continue
    k, v = line.split('=', 1)
    k = k.strip()
    v = v.strip()
    if v[0] == '"' and v[-1] == '"': v = v[1:-1]

    if k == 'Iwd': _iwd = v
    if k == 'Args': _args = v
    if k == 'ProcId': _proc = v
    if k == 'Cmd': _exe = v
    if k == 'ClusterId': _cluster = v
