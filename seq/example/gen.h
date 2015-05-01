/*
 * fibo.h
 *
 *  Created on: Mar 27, 2015
 *      Author: kp
 */

#ifndef EXAMPLE_FIBO_H_
#define EXAMPLE_FIBO_H_

#include "static/seq/seq.h"

#define FIBO 0
#define FIBO_ELSE 1
#define FIBO_1 2
#define FIBO_2 3
#define FINAL 4

#define START_ENV_SIZE 6
#define START_LABEL FIBO
#define FINAL_LABEL FINAL

void executeContext(seq::Context* context) {
	while ( context != nullptr ) {
		switch (context->nextInstruction) {
			case FIBO:
				{
					context->arithmeticAccumulator = INT(context->currentFrame->environment[0]) < 2  ? 1 : 0;
				}
				JMP_IF_ZERO(FIBO_ELSE);
				LOAD_MOVE(0);
				RET;
			case FIBO_ELSE:
				{
					context->arithmeticAccumulator = INT(context->currentFrame->environment[0]) - 1;
				}
				STORE_ARITH(1)
				ALLOC_PARAMS(3)
				PREPARE_PARAM_MOVE(0, 1)
				CALL(FIBO, FIBO_1)
			case FIBO_1:
				STORE(1)
				{
					context->arithmeticAccumulator = INT(context->currentFrame->environment[0])  - 2;
				}
				STORE_ARITH(2)
				ALLOC_PARAMS(3)
				PREPARE_PARAM_MOVE(0, 2)
				CALL(FIBO, FIBO_2)
			case FIBO_2:
				{
					context->arithmeticAccumulator = INT(context->currentFrame->environment[1])
										+ INT(context->accumulator);
				}
				LOAD_ARITH
				RET
			case FINAL:
				FINALIZE
			default:
				// should not happen.
				break;
		}

	}
}



#endif /* EXAMPLE_FIBO_H_ */
