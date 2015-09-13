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
#define FINAL 2
#define FIBO_1 3
#define FIBO_2 4
#define FIBO_ADD_PARAM_1 5
#define FIBO_ADD_PARAM_2 6

#define ENV_SIZE 6

std::unique_ptr<seq::Context> generateStartingContext(int param) {
	return seq::generateStartingContext(param, FIBO, FINAL, ENV_SIZE);
}

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
				STORE_ARITH_ACC
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
