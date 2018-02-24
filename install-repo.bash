#!/bin/bash

set -o pipefail  # trace ERR through pipes
set -o errtrace  # trace ERR through 'time command' and other functions
set -o nounset   ## set -u : exit the script if you try to use an uninitialised variable
set -o errexit   ## set -e : exit the script if any statement returns a non-true return value
set -o xtrace    # print commands as they are executed

cat > /etc/apt/sources.list.d/libfixposix.list <<EOF
deb http://download.opensuse.org/repositories/home:/sionescu/Debian/ ./
EOF

cat > /etc/apt/preferences.d/libfixposix <<EOF
Package: libfixposix*
Pin: origin download.opensuse.org
Pin-Priority: 1001
EOF

curl https://download.opensuse.org/repositories/home:/sionescu/Debian/Release.key | apt-key add -

apt-get update -qq

apt-get -y install libfixposix3 libfixposix-dev

dpkg -L libfixposix3 libfixposix-dev
