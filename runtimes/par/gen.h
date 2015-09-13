#ifndef CTHULHU_GEN_H_
#define CTHULHU_GEN_H_
#define START_ENV_SIZE 2
#define START_LABEL 2
#define FINAL_LABEL 4
#include "static/seq/seq.h"
void executeContext(std::unique_ptr<seq::Context>&& context) {
while ( context != nullptr ) {
switch (context->nextInstruction) {
case 0: /*call*/
SKIP
WAIT(0, 1)
case 1:
SKIP
LOAD_COPY(0)
RET
case 2: /*entry_point*/
SKIP
LOAD_COPY(0)
STORE(1)
ALLOC_PARAMS(1)
PREPARE_PARAM_MOVE(1, 0)
CALL(0, 3)
case 3:
SKIP
RET
case 4:
FINALIZE
default:
break;
}
}
}
#endif
