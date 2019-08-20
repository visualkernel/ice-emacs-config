#!/bin/bash

CURRDIR=$(dirname $(readlink -f $0))

ln -s -f $CURRDIR/elpa ~/.emacs.d/elpa
ln -s -f $CURRDIR/init.el ~/.emacs.d/init.el
