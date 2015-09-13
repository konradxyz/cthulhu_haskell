#!/bin/bash

set -eax
cp runtimes/seq/gen.h runtimes/seq_icpc
cd runtimes/seq_icpc
tar -cvvf ../../run_seq_icpc.tar.gz *



