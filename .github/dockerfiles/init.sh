#!/bin/sh

sudo mkdir -p -m0755 /var/run/sshd

sudo /usr/sbin/sshd

sudo service postgresql start

sudo -E bash -c "apt-get update && apt-get install -y linux-tools-common linux-tools-generic linux-tools-`uname -r`"

sudo -E bash -c "Xdummy -debug :6 > /tmp/xdummy.log &"

exec /bin/bash -c "$1"
