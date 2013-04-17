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

TEXI2DVI=texi2dvi
TEXI2PDF=texi2dvi --pdf
MAKEINFO=makeinfo
MAKEINFO_PLAIN=-D rawfile --no-headers
TEXI2HTML=makeinfo --no-split --html --number-sections
DVIPS=dvips
INSTALL_INFO=install-info
TEXIFILES=reftex.texi changes.texi install.texi version.texi
DISTTEXTS=../CHANGES ../INSTALL
MAKEDISTTEXTHEADER="echo -e \"This is the `echo $@ | tr -d ./` \
file for the RefTeX distribution.\n\n\
`sed -n 's/@c \(Copyright (C).*\)/ \1/p' $<`\n\n  Copying\
and distribution of this file, with or without modification,\n  are\
permitted in any medium without royalty provided the copyright\n  notice\
and this notice are preserved.\n\" >$@"

.PHONY: all info dvi ps pdf html install dist clean maintainer-clean

all: info

info: reftex.info

dvi: reftex.dvi

ps: reftex.ps

pdf: reftex.pdf

html: reftex.html

reftex.info: $(TEXIFILES)
	$(MAKEINFO) --no-split reftex.texi

reftex.dvi: $(TEXIFILES)
	$(TEXI2DVI) reftex.texi

reftex.ps: reftex.dvi
	$(DVIPS) -o reftex.ps reftex.dvi

reftex.pdf: $(TEXIFILES)
	$(TEXI2PDF) reftex.texi

reftex.html: $(TEXIFILES)
	$(TEXI2HTML) reftex.texi

install: reftex.info
	if [ ! -d $(infodir) ]; then $(MKDIR) $(infodir); else true; fi ;
	$(INSTALL_DATA) reftex.info $(infodir)
	test ! -f $(infodir)/reftex || \
	  $(MV) $(infodir)/reftex $(infodir)/reftex.old
	-$(INSTALL_INFO) --info-dir=$(infodir) --remove reftex
	-$(INSTALL_INFO) --info-dir=$(infodir) reftex.info

clean:
	rm -f *.aux *.cp *.cps *.dvi *.fn *.fns *.ky *.kys *.pg *.pgs
	rm -f *.toc *.tp *.tps *.vr *.vrs *.log *.html *.ps

maintainer-clean: clean
	rm -f $(DISTTEXTS) reftex.info

dist: $(DISTTEXTS) reftex.info
	rm .cvsignore

../CHANGES: changes.texi
	eval $(MAKEDISTTEXTHEADER)
	$(MAKEINFO) $(MAKEINFO_PLAIN) changes.texi >>$@

../INSTALL: install.texi
	eval $(MAKEDISTTEXTHEADER)
	$(MAKEINFO) $(MAKEINFO_PLAIN) install.texi >>$@

version.texi: ../ChangeLog
	REFTEXDATE=`sed -n '1s/^\([-0-9][-0-9]*\).*/\1/p' ../ChangeLog` ; \
	test "X$${REFTEXDATE}" != X || \
	  { echo "Can't find date in ChangeLog" >&2 ; exit 1; }; \
	REFTEXVERSION=`sed -n '2,/^[0-9]/s/.*Version \(.*\) released\..*/\1/p' ../ChangeLog`; \
	test "X$${REFTEXVERSION}" != X || REFTEXVERSION=$${REFTEXDATE}; \
	echo @set VERSION $${REFTEXVERSION} >version.texi; \
	echo @set DATE $${REFTEXDATE} >>version.texi
