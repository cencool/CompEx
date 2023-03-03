#!/bin/bash

LAZBUILD="/usr/bin/lazbuild"
PROJECT="/home/milhar/Documents/prog/compilers/keith/compiler.lpi"

# Modify .lpr file in order to avoid nothing-to-do-bug (http://lists.lazarus.freepascal.org/pipermail/lazarus/2016-February/097554.html)
echo. >> "/home/milhar/Documents/prog/compilers/keith/compiler.lpr"

if $LAZBUILD $PROJECT; then

  if [ $1 = "test" ]; then
    "/home/milhar/Documents/prog/compilers/keith/compiler" 
  fi
fi
