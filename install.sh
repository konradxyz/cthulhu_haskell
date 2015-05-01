#/bin/bash
set -eax
cd generator
make
cd ..
rm -rf generator/runtimes
mkdir generator/runtimes
mkdir generator/runtimes/seq
cp -rf seq/static generator/runtimes/seq
cp seq/Makefile generator/runtimes/seq

