#!/bin/sh
cd *.jsexe
for ff in ../../../webgl-template/* ; do
    f=`basename $ff`
    rm -f "$f"
    ln -s $ff
done
rm -f resources
ln -s ../resources
