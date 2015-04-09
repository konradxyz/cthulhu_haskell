/*
 * fibo.h
 *
 *  Created on: Mar 27, 2015
 *      Author: kp
 */

#ifndef EXAMPLE_FIBO_H_
#define EXAMPLE_FIBO_H_

#include "data/data.h"

#define INT(ptr) static_cast<const casm::IntValue*>((ptr)->getValue())->getValue()

#define LOAD(id) { context->accumulator = context->currentFrame->environment[id]; }

#define CONST(val) { context->accumulator = casm::generateValueWrapper(utils::make_unique<casm::IntValue>(val)); }

#define STORE(id) { context->currentFrame->environment[id] = context->accumulator; }

#define LT(id) {\
		int other = INT(context->currentFrame->environment[id]);\
		int current = INT(context->accumulator);\
		int result = current < other ? 1 : 0;\
		context->accumulator = casm::generateValueWrapper(utils::make_unique<casm::IntValue>(result));\
}

#define JMP_IF_ZERO(label)	{\
		if( INT(context->accumulator) == 0 ) {\
			context->nextInstruction = label;\
			break;\
		}\
}

#define RET  { context->removeLastFrame(); break;}

#define SUB(id) {\
		int other = INT(context->currentFrame->environment[id]);\
		int current = INT(context->accumulator);\
		int result = current - other;\
		context->accumulator = casm::generateValueWrapper(utils::make_unique<casm::IntValue>(result));\
}

#define ADD(id) {\
		int other = INT(context->currentFrame->environment[id]);\
		int current = INT(context->accumulator);\
		int result = current + other;\
		context->accumulator = casm::generateValueWrapper(utils::make_unique<casm::IntValue>(result));\
}

#define ALLOC(label, params, size, retLabel) {\
		if ( params == 0 ) {\
			context->currentFrame->nextInstruction = retLabel;\
			context->allocateFrame(size);\
			context->nextInstruction = label;\
			break;\
		} else\
			context->accumulator =\
					casm::generateValueWrapper(utils::make_unique<casm::FunctionApplyValue>(label, params, size));\
}

#define ADD_PARAM(id, retLabel) {\
		auto apply = static_cast<const casm::ApplyValue*>(context->accumulator->getValue());\
		if ( apply->getParamsAvailable() + 1 >= apply->getParamsNeeded() ) {\
			casm::CallSpecification spec;\
			apply->prepareCall(&spec);\
			auto previousFrame = context->currentFrame;\
			context->currentFrame->nextInstruction = retLabel;\
			context->allocateFrame(spec.getEnvSize());\
			for ( unsigned i = 0; i < spec.getParams()->size(); ++i ) {\
				context->currentFrame->environment[i] = std::move((*spec.getParams())[i]);\
			}\
			context->currentFrame->environment[spec.getParams()->size()] = previousFrame->environment[id];\
			context->nextInstruction = spec.getFunctionInstruction();\
			break;\
		} else\
			context->accumulator =\
				casm::generateValueWrapper(utils::make_unique<casm::ParamApplyValue>(\
						std::move(context->accumulator),\
						std::shared_ptr<casm::ValueWrapper>(context->currentFrame->environment[id])));\
}
#define FINALIZE {\
		context->currentFrame->environment[0]->getFutureContext()->setValueAndWakeAll(std::move(context->accumulator));\
		context = nullptr;\
		break;\
}

#define FIBO 0
#define FIBO_ELSE 1
#define FINAL 2
#define FIBO_1 3
#define FIBO_2 4
#define FIBO_ADD_PARAM_1 5
#define FIBO_ADD_PARAM_2 6

std::unique_ptr<casm::Context> generateStartingContext(int param, unsigned envSize, std::shared_ptr<casm::ValueWrapper>* resultKeeper) {
	*resultKeeper = casm::generateFutureWrapper();
	auto result = utils::make_unique<casm::Context>();
	result->allocateFrame(1);
	result->currentFrame->environment[0] = *resultKeeper;
	result->currentFrame->nextInstruction = FINAL;
	result->allocateFrame(envSize);
	result->currentFrame->environment[0] = casm::generateValueWrapper(utils::make_unique<casm::IntValue>(param));

	return std::move(result);

}

void executeContext(std::unique_ptr<casm::Context>&& contextU) {
	auto context = contextU.get();

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


void perform(casm::ContextBase* base) {

	auto ctx = base->popTask();
	while (ctx != nullptr ) {

		executeContext(std::move(ctx), base);

		ctx = base->popTask();
	}
}



#endif /* EXAMPLE_FIBO_H_ */
