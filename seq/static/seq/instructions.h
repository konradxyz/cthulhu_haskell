/*
 * instructions.h
 *
 *  Created on: 9 kwi 2015
 *      Author: kp306410
 */

#ifndef INSTRUCTIONS_H_
#define INSTRUCTIONS_H_


#define INT(ptr) static_cast<const seq::IntValue*>((ptr.get()))->getValue()

#define LOAD(id) { context->accumulator = context->currentFrame->environment[id]; }

#define CONST(val) { context->accumulator = std::make_shared<seq::IntValue>(val); }

#define STORE(id) { context->currentFrame->environment[id] = std::move(context->accumulator); }
#define STORE_ARITH(id) { context->currentFrame->environment[id] = std::make_shared<seq::IntValue>(context->arithmeticAccumulator); }

#define JMP_IF_ZERO(label)	{\
		if( context->arithmeticAccumulator == 0 ) {\
			context->nextInstruction = label;\
			break;\
		}\
}

#define RET  { context->removeLastFrame(); break;}

#define ALLOC(label, params, size, retLabel) {\
		if ( params == 0 ) {\
			context->currentFrame->nextInstruction = retLabel;\
			context->allocateFrame(size);\
			context->nextInstruction = label;\
			break;\
		} else\
			context->accumulator =\
					std::make_shared<seq::FunctionApplyValue>(label, params, size);\
}

#define ADD_PARAM(id, retLabel) {\
		auto apply = static_cast<seq::ApplyValue*>(context->accumulator.get());\
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
				std::make_shared<seq::ParamApplyValue>(\
						std::static_pointer_cast<seq::ApplyValue>(context->accumulator),\
						std::shared_ptr<seq::Value>(context->currentFrame->environment[id]));\
}
#define FINALIZE {\
		context = nullptr;\
		break;\
}




#endif /* INSTRUCTIONS_H_ */
