#/bin/bash
set -eax
cd generator
make
cd ..
rm -rf generator/runtimes
mkdir generator/runtimes


cd seq
make clean
cd ..

mkdir generator/runtimes/seq
cp -rf seq/static generator/runtimes/seq
cp seq/Makefile generator/runtimes/seq


mkdir generator/runtimes/seq_icpc
cp -rf seq/static generator/runtimes/seq_icpc
cp seq/_Makefile generator/runtimes/seq_icpc/Makefile

mkdir generator/runtimes/par
cp -rf par/static generator/runtimes/par
cp par/Makefile generator/runtimes/par


