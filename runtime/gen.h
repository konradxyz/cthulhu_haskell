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
				CONST(2);
				STORE(1);
				LOAD(0);
				LT(1);
				JMP_IF_ZERO(FIBO_ELSE);
				LOAD(0);
				RET;
			case FIBO_ELSE:
				CONST(1)
				STORE(1)
				LOAD(0)
				SUB(1)
				STORE(1)
				ALLOC(FIBO, 1, 3, FIBO_ADD_PARAM_1)
			case FIBO_ADD_PARAM_1:
				ADD_PARAM(1, FIBO_1)
			case FIBO_1:
				STORE(1)
				CONST(2)
				STORE(2)
				LOAD(0)
				SUB(2)
				STORE(2)
				ALLOC(FIBO, 1, 3, FIBO_ADD_PARAM_2)
			case FIBO_ADD_PARAM_2:
				ADD_PARAM(2, FIBO_2)
			case FIBO_2:
				ADD(1)
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
