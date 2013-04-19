#!/bin/sh

BASEDIR=$(dirname $0)

cd $BASEDIR

wget -N http://repo.or.cz/w/emacs.git/blob_plain/HEAD:/lisp/progmodes/python.el
