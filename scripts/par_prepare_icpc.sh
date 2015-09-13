#!/bin/bash

set -eax
cp runtimes/par/gen.h runtimes/par_icpc
cd runtimes/par_icpc
tar -cvvf ../../run_par_icpc.tar.gz *



