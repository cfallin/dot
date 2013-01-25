#!/usr/bin/perl -w

my $clusterid = 0;
my $jobid = 0;

my $term = shift;

open F, "condor_q -long cfallin |";

while(<F>) {
    if (/ClusterId = (\d+)/) {
        $clusterid = $1 + 0;
    }
    if (/ProcId = (\d+)/) {
        $jobid = $1 + 0;
    }
    if (/Args = /) {
        if (/$term/) {
            print "$clusterid.$jobid\n";
        }
    }
}

close F;
