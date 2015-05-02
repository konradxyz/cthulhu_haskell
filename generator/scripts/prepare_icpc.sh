#!/bin/bash

set -eax
cp runtimes/seq/gen.h runtimes/seq_icpc
cd runtimes/seq_icpc
tar -cvvf ../../run_icpc.tar.gz *



