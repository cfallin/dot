#!/usr/bin/python

import threading
import os
import sys
import signal

joblock = threading.Lock()
njobs = 0
jobs = []
done = []
errors = []
nthreads = 1

class Job:
    def __init__(self, cmd):
        self.cmd = cmd
    def run(self):
        global errors, done, joblock, njobs
        print "start: ", self.cmd
        ret = os.system(self.cmd)
        ret = 0

        joblock.acquire()

        if ret != 0:
            errors.append(self)
        else:
            done.append(self)

        ndone = len(done)

        joblock.release()

        print "**** complete: %d / %d (%.3f%%)" % (ndone, njobs, 100.0*float(ndone)/njobs if njobs > 0 else 100.0)

def parse_condor(fname):
    global jobs
    f = open(fname)
    exe = ''
    for line in f.readlines():
        parts = line.split('=')
        if len(parts) < 2: continue
        cmd = parts[0].strip().lower()
        if len(parts) > 1: val = parts[1].strip()
        if cmd == 'executable':
            exe = val
        if cmd == 'arguments':
            c = exe + ' ' + val
            j = Job(c)
            jobs.append(j)
        
if len(sys.argv) > 1:
    parse_condor(sys.argv[1])
if len(sys.argv) > 2:
    nthreads = int(sys.argv[2])

njobs = len(jobs)

class runnerthread(threading.Thread):
    def __init__(self):
        threading.Thread.__init__(self)

    def run(self):
        global jobs
        global joblock
        while True:
            job = None
            joblock.acquire()
            if len(jobs) > 0:
                job = jobs[0]
                jobs = jobs[1:]
            joblock.release()
            if job is None: break

            job.run()

th = []

def handle_int(sig, frame):
    global th
    global jobs
    global joblock
    joblock.acquire()
    jobs = []
    joblock.release()
    for t in th:
        t.join()
    sys.exit(1)

signal.signal(signal.SIGINT, handle_int)

for i in range(nthreads):
    t = runnerthread()
    t.start()
    th.append(t)


for t in th:
    t.join()
