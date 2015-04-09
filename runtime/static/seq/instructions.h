/*
 * instructions.h
 *
 *  Created on: 9 kwi 2015
 *      Author: kp306410
 */

#ifndef INSTRUCTIONS_H_
#define INSTRUCTIONS_H_


#define INT(ptr) static_cast<const seq::IntValue*>((ptr)->getValue())->getValue()

#define LOAD(id) { context->accumulator = context->currentFrame->environment[id]; }

#define CONST(val) { context->accumulator = seq::generateValueWrapper(utils::make_unique<seq::IntValue>(val)); }

#define STORE(id) { context->currentFrame->environment[id] = context->accumulator; }

#define LT(id) {\
		int other = INT(context->currentFrame->environment[id]);\
		int current = INT(context->accumulator);\
		int result = current < other ? 1 : 0;\
		context->accumulator = seq::generateValueWrapper(utils::make_unique<seq::IntValue>(result));\
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
		context->accumulator = seq::generateValueWrapper(utils::make_unique<seq::IntValue>(result));\
}

#define ADD(id) {\
		int other = INT(context->currentFrame->environment[id]);\
		int current = INT(context->accumulator);\
		int result = current + other;\
		context->accumulator = seq::generateValueWrapper(utils::make_unique<seq::IntValue>(result));\
}

#define ALLOC(label, params, size, retLabel) {\
		if ( params == 0 ) {\
			context->currentFrame->nextInstruction = retLabel;\
			context->allocateFrame(size);\
			context->nextInstruction = label;\
			break;\
		} else\
			context->accumulator =\
					seq::generateValueWrapper(utils::make_unique<seq::FunctionApplyValue>(label, params, size));\
}

#define ADD_PARAM(id, retLabel) {\
		auto apply = static_cast<const seq::ApplyValue*>(context->accumulator->getValue());\
		if ( apply->getParamsAvailable() + 1 >= apply->getParamsNeeded() ) {\
			seq::CallSpecification spec;\
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
				seq::generateValueWrapper(utils::make_unique<seq::ParamApplyValue>(\
						std::move(context->accumulator),\
						std::shared_ptr<seq::ValueWrapper>(context->currentFrame->environment[id])));\
}
#define FINALIZE {\
		context = nullptr;\
		break;\
}




#endif /* INSTRUCTIONS_H_ */
