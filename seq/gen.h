#ifndef CTHULHU_GEN_H_
#define CTHULHU_GEN_H_
#include "static/seq/seq.h"
#define START_ENV_SIZE 2
#define START_LABEL 1
#define FINAL_LABEL 9
void executeContext(seq::Context* context) {
while ( context != nullptr ) {
switch (context->nextInstruction) {
case 0: /*call*/
SKIP
ALLOC_PARAMS(0)
GLOBAL(4, 1, 5)
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
case 4: /*fibo*/
SKIP
ARITH(LT(ENV_INT(0), 2))
JMP_IF_ZERO(5)
LOAD_COPY(0)
RET
JMP(6)
case 5:
SKIP
ARITH(SUB(ENV_INT(0), 1))
STORE_ARITH(3)
ALLOC_PARAMS(5)
PREPARE_PARAM_MOVE(3, 0)
CALL(4, 8)
case 8:
SKIP
STORE(1)
ARITH(SUB(ENV_INT(0), 2))
STORE_ARITH(4)
ALLOC_PARAMS(5)
PREPARE_PARAM_MOVE(4, 0)
CALL(4, 7)
case 7:
SKIP
STORE(2)
ARITH(ADD(ENV_INT(1), ENV_INT(2)))
LOAD_ARITH
RET
case 6:
SKIP
case 9:
FINALIZE
default:
break;
}
}
}
#endif
