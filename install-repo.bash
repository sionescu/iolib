#!/bin/bash

cat >> /etc/apt/sources.list <<EOF
deb http://download.opensuse.org/repositories/home:/sionescu/Ubuntu_Precise/ ./
EOF

cat >> /etc/apt/preferences <<EOF

Package: libfixposix*
Pin: origin download.opensuse.org
Pin-Priority: 1001
EOF

apt-get update -qq

apt-get -y --force-yes install libfixposix2 libfixposix-dev
