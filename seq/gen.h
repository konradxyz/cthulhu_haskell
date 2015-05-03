#ifndef CTHULHU_GEN_H_
#define CTHULHU_GEN_H_
#include "static/seq/seq.h"
#define START_ENV_SIZE 2
#define START_LABEL 1
#define FINAL_LABEL 6
void executeContext(seq::Context* context) {
while ( context != nullptr ) {
switch (context->nextInstruction) {
case 0: /*call*/
SKIP
ALLOC_PARAMS(0)
GLOBAL(4, 1, 1)
RET
case 1: /*entry_point*/
SKIP
LOAD_COPY(0)
STORE(1)
ALLOC_PARAMS(0)
CALL(0, 3)
case 3:
SKIP
ADD_PARAM_MOVE(1, 2)
case 2:
SKIP
RET
case 4: /*id*/
SKIP
WAIT(0, 5)
case 5:
SKIP
LOAD_COPY(0)
RET
case 6:
FINALIZE
default:
break;
}
}
}
#endif
