#ifndef CTHULHU_GEN_H_
#define CTHULHU_GEN_H_
#define START_ENV_SIZE 2
#define START_LABEL 1
#define FINAL_LABEL 3
#include "static/seq/seq.h"
void executeContext(seq::Context* context) {
while ( context != nullptr ) {
switch (context->nextInstruction) {
case 0: /*call*/
SKIP
LOAD_COPY(0)
RET
case 1: /*entry_point*/
SKIP
LOAD_COPY(0)
STORE(1)
ALLOC_PARAMS(1)
PREPARE_PARAM_MOVE(1, 0)
CALL(0, 2)
case 2:
SKIP
RET
case 3:
FINALIZE
default:
break;
}
}
}
#endif
