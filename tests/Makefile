# -*- Mode: Makefile; tab-width: 4; indent-tabs-mode: t -*-
#
# Makefile --- Make targets for test echo server.
#
# Copyright (C) 2007, Luis Oliveira  <loliveira@common-lisp.net>
#
# Permission is hereby granted, free of charge, to any person
# obtaining a copy of this software and associated documentation
# files (the "Software"), to deal in the Software without
# restriction, including without limitation the rights to use, copy,
# modify, merge, publish, distribute, sublicense, and/or sell copies
# of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be
# included in all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
# EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
# MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
# NONINFRINGEMENT.  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
# HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
# WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
# DEALINGS IN THE SOFTWARE.
#

# On my mingw/msys system this Makefile doesn't work so well for
# some reason.  The following, however, does work:
#   gcc -c echo-server.c
#   g++ -l echo-server.o -o echo-server.exe

CFLAGS ?= -Wall

all: echo-server

echo-server: echo-server.c
	$(CC) -o $@ $(CFLAGS) $<

clean:
	rm -f echo-server echo-server.exe

# vim: ft=make ts=4 noet
