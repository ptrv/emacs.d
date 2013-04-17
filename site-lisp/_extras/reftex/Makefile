# Makefile for the RefTeX distribution.

# Maintainer: auctex-devel@gnu.org

# Copyright (C) 2007 Free Software Foundation, Inc.

# This file is part of RefTeX.

# RefTeX is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3, or (at your option)
# any later version.

# RefTeX is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with RefTeX; see the file COPYING.  If not, write to the Free
# Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
# MA 02110-1301, USA.

# Commentary:

# To install RefTeX, edit the Makefile, type `make', then `make install'.
# To create the PostScript documentation file reftex.ps, type `make ps'.
# Analogously you can use the targets `pdf', `html', or `dvi' to
# generate documentation as PDF, HTML, or DVI file.

##----------------------------------------------------------------------
##  YOU MIGHT HAVE TO EDIT THE FOLLOWING LINES
##  (or pass your own values for them to make)
##----------------------------------------------------------------------

# Where local software is located.
prefix=/usr/local

# Where info files go.
infodir=$(prefix)/share/info

# Where local lisp files go.
lispdir=$(prefix)/share/emacs/site-lisp/reftex

# Name of emacs binary.
EMACS=emacs

# How to create directories.
MKDIR=mkdir -p

# How to move files.
MV=mv

# How to copy the lisp files to their distination.
INSTALL=install -c
INSTALL_DATA=${INSTALL} -m 644

##----------------------------------------------------------------------
##  BELOW THIS LINE ON YOUR OWN RISK!
##----------------------------------------------------------------------

CP=cp -p

SHELL=/bin/sh

.SUFFIXES: .el .elc .texi

.PHONY: all install lisp info dvi ps pdf html dist clean maintainer-clean \
	release-commit release-sign release-upload www-doc

all: lisp

lisp:
	cd lisp && make EMACS="$(EMACS)" lisp

install:
	cd lisp && make INSTALL_DATA="$(INSTALL_DATA)" MKDIR="$(MKDIR)" \
		lispdir="$(lispdir)" install
	cd doc && make INSTALL_DATA="$(INSTALL_DATA)" MKDIR="$(MKDIR)" \
		MV="$(MV)" infodir="$(infodir)" install

info:
	cd doc && make info

dvi:
	cd doc && make dvi

ps:
	cd doc && make ps

pdf:
	cd doc && make pdf

html:
	cd doc && make html

clean:
	cd lisp && make clean
	cd doc && make clean

maintainer-clean:
	@echo 'This command is intended for maintainers to use; it'
	@echo 'deletes files that may need special tools to rebuild.'
	cd lisp && make clean
	cd doc && make maintainer-clean

# Call `make dist' either with `TAG=X.Y' or `TAG=YYYYMMDD'.  (You can
# use `TAG=`date +%Y%m%d`' for automatic creation of the current
# date.)  It is assumed that there is a `release_X_Y' tag in CVS if
# `TAG=X.Y' is used.
#
# Calling the `dist' target will create a subdirectory `reftex-$(TAG)'
# with a clean checkout from CVS, generate info docs, and package the
# directory in a gzipped tar file.
dist:
	@if [ "X$(TAG)" = "X" ]; then echo "*** No tag ***"; exit 1; fi
	rm -rf reftex-$(TAG)
	mkdir reftex-$(TAG)
	cvs export -d reftex-$(TAG) \
          "`echo $(TAG) | sed -e '/^\([-0-9]*[0-9]\)[-a-z]*$$/s//-D \1T2359/' \
                             -e '/[.]/{s/^/-rrelease_/;s/[.]/_/g}'`" reftex
	cd reftex-$(TAG)/lisp && make EMACS="$(EMACS)" dist
	cd reftex-$(TAG)/doc && make dist
	chmod -R go-w+rX reftex-$(TAG)
	tar -cf - --owner=root --group=root reftex-$(TAG) | \
	  gzip --best > reftex-$(TAG).tar.gz

##----------------------------------------------------------------------
##  RELEASE
##----------------------------------------------------------------------

# Steps for making a release:
# 1) release-commit: Tag the release.
# 2) dist: Create the tar ball.
# 3) release-sign: Sign the tar ball and create directive files for upload.
# 4) release-upload: Upload tar ball, signature, and directive file.

COMMITTER="Ralf Angeli  <angeli@caeruleus.net>"

release-commit:
	@if [ "X$(TAG)" = "X" ]; then echo "*** No tag ***"; exit 1; fi
	$(MV) ChangeLog ChangeLog.old
	echo `date "+%Y-%m-%d "`" "${COMMITTER} > ChangeLog
	echo >> ChangeLog
	echo "	* Version" $(TAG) released. >> ChangeLog
	echo >> ChangeLog
	cat ChangeLog.old >> ChangeLog
	cvs commit -m 'release_$(TAG)' ChangeLog
	cvs tag release_`echo $(TAG) | sed -e 's/[.]/_/g'`

release-sign:
	@if [ "X$(TAG)" = "X" ]; then echo "*** No tag ***"; exit 1; fi
	read -sp "Enter pass phrase: " phrase ; \
	echo "$$phrase" | \
	  gpg --passphrase-fd 0 --detach-sign reftex-$(TAG).tar.gz ; \
	echo -e "version: 1.1\ndirectory: reftex\n\
filename: reftex-$(TAG).tar.gz" >reftex-$(TAG).tar.gz.directive; \
	echo "$$phrase" | \
	  gpg --passphrase-fd 0 --clearsign reftex-$(TAG).tar.gz.directive ; \
	rm reftex-$(TAG).tar.gz.directive

release-upload:
	@if [ "X$(TAG)" = "X" ]; then echo "*** No tag ***"; exit 1; fi
	ncftpput ftp-upload.gnu.org /incoming/ftp reftex-$(TAG).tar.gz*

www-doc: dist
	rm -rf www-doc
	mkdir -p www-doc
	$(CP) reftex-$(TAG)/doc/*.texi www-doc
	cd www-doc \
	&& wget -O gendocs.sh 'http://savannah.gnu.org/cgi-bin/viewcvs/%2acheckout%2a/texinfo/texinfo/util/gendocs.sh?rev=HEAD' \
	&& patch <$(GENDOCSPATCH) \
	&& chmod u+x gendocs.sh \
	&& wget -O gendocs_template 'http://savannah.gnu.org/cgi-bin/viewcvs/%2acheckout%2a/texinfo/texinfo/util/gendocs_template?rev=HEAD' \
	&& sed -i -e 's/html_node\/index\.html/%%PACKAGE%%\/index.html/' gendocs_template \
	&& mkdir .texinfo \
	&& echo -e "emacs split http://www.gnu.org/software/emacs/manual/html_node/emacs/\nemacs mono http://www.gnu.org/software/emacs/manual/html_mono/emacs.html\nauctex mono http://www.gnu.org/software/auctex/manual/auctex.html\nauctex split http://www.gnu.org/software/auctex/manual/auctex/\n" > .texinfo/htmlxref.cnf \
	&& ./gendocs.sh --email bug-auctex@gnu.org reftex "RefTeX Manual"
