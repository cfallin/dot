#!/bin/bash

#
# First: set up ssh keys, sudo, keychain on login, vim/tmux/less/the usual.
#
# Second: remove exim4-base and rpcbind. Install nmap and make sure no other
# services are listening on open ports.
#
# Third: install docker.
#
# Fourth: do the below.
#

mkdir -p /home/certs
mkdir -p /home/www/c1f.net
mkdir -p /home/logs/c1f.net
ln -s /home/www/c1f.net/fallin /home/www/fallin.org
mkdir -p /home/logs/fallin.org
chown -R cfallin:cfallin /home/www

docker run --restart=always -d --name nginx-proxy -p 80:80 -p 443:443 -v /home/certs:/etc/nginx/certs:ro -v /etc/nginx/vhost.d -v /usr/share/nginx/html -v /var/run/docker.sock:/tmp/docker.sock:ro jwilder/nginx-proxy
docker run --restart=always -d --name letsencrypt -v /home/certs:/etc/nginx/certs:rw --volumes-from nginx-proxy -v /var/run/docker.sock:/var/run/docker.sock:ro jrcs/letsencrypt-nginx-proxy-companion

# C1F.net
C1FDOMAINS="c1f.net,www.c1f.net,cfallin.org,www.cfallin.org,cfallin.net,www.cfallin.net,cfallin.com,www.cfallin.com,cfall.in,www.cfall.in"
docker run --restart=always -d --name c1f_net -v /home/www/c1f.net:/usr/share/nginx/html -v /home/logs/c1f.net:/var/log/nginx -e VIRTUAL_HOST=$C1FDOMAINS -e LETSENCRYPT_HOST=$C1FDOMAINS -e LETSENCRYPT_EMAIL=root@c1f.net -e VIRTUAL_PORT=80 nginx

# fallin.rog
FALLINDOMAINS=fallin.org,www.fallin.org
docker run --restart=always -d --name fallin_org -v /home/www/fallin.org:/usr/share/nginx/html -v /home/logs/fallin.org:/var/log/nginx -e VIRTUAL_HOST=$FALLINDOMAINS -e LETSENCRYPT_HOST=$FALLINDOMAINS -e LETSENCRYPT_EMAIL=root@c1f.net -e VIRTUAL_PORT=80 nginx
docker run --restart=always -d --name blog_cfall_in -v /home/www/blog.cfall.in:/usr/share/nginx/html -v /home/logs/blog.cfall.in:/var/log/nginx -e VIRTUAL_HOST=blog.cfall.in -e LETSENCRYPT_HOST=blog.cfall.in -e LETSENCRYPT_EMAIL=root@c1f.net -e VIRTUAL_PORT=80 nginx
