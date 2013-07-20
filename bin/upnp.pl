#!/usr/bin/perl -w

use strict;
use Net::UPnP::ControlPoint;

my $port = 22; # ssh

sub get_ip() {
    open PIPE, "-|", "ip addr";
    my $ip = '';
    while (<PIPE>) {
        next if not /.*inet (192.168.\d+.\d+)\/.*/;
        $ip = $1;
        last;
    }
    close PIPE;
    $ip =~ s/^\s+//;
    $ip =~ s/\s+$//;
    return $ip;
}

my $ip = get_ip();
my $upnp = Net::UPnP::ControlPoint->new();

my @devs = $upnp->search(st => 'upnp:rootdevice', mx => 3);
foreach my $dev (@devs) {
    my $type = $dev->getdevicetype();
    my $name = $dev->getfriendlyname();

    next if $type !~ /urn:schemas-upnp-org:device:InternetGatewayDevice:1/;

    my @services = $dev->getservicelist();
    foreach my $service (@services) {
        my $type = $service->getservicetype();
        my $id = $service->getserviceid();
        if ($type =~ /urn:schemas-upnp-org:service:WANIPConnection:1/) {
            my %args = (
                'NewRemoteHost' => '',
                'NewExternalPort' => $port,
                'NewProtocol' => 'TCP',
            );
            $service->postcontrol('DeletePortMapping', \%args);

            %args = (
                'NewRemoteHost' => '',
                'NewExternalPort' => $port,
                'NewInternalPort' => $port,
                'NewProtocol' => 'TCP',
                'NewInternalClient' => $ip,
                'NewEnabled' => 1,
                'NewPortMappingDescription' => 'ssh',
                'NewLeaseDuration' => 0,
            );
            my $res = $service->postcontrol('AddPortMapping', \%args);
            unless ($res->getstatuscode() == 200) {
                my $code = $res->getstatuscode();
                warn "Bad status code from AddPortMapping rpc: $code\n";
            }

            system "wget -O /dev/null -q http://c1f.net/__ip";
            exit 0;
        }
    }
}
exit 1;
