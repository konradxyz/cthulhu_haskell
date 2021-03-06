/*
 * instructions.h
 *
 *  Created on: 9 kwi 2015
 *      Author: kp306410
 */

// Before this header is included it is necessary to define FINAL_LABEL macro.
#ifndef INSTRUCTIONS_H_
#define INSTRUCTIONS_H_

#define COMMA ,

#define IDENTITY(x) x
#define MOVE(x) std::move(x)

#define INT(ptr) static_cast<const seq::IntValue*>((ptr.get()))->getValue()

// Arithmetics:
#define ENV_INT(id) INT(context->currentFrame->environment[id])
#define ADD(x, y) ((x) + (y))
#define SUB(x, y) ((x) - (y))
#define LT(x, y) ((x) < (y) ? 1 : 0)
#define LE(x, y) ((x) <= (y) ? 1 : 0)
#define GE(x, y) ((x) > (y) ? 1 : 0)
#define GT(x, y) ((x) >= (y) ? 1 : 0)
#define EQ(x, y) ((x) == (y) ? 1 : 0)
#define NEQ(x, y) ((x) != (y) ? 1 : 0)
#define AND(x, y) ((x) && (y))
#define OR(x, y) ((x) || (y))
#define MUL(x, y) ((x) * (y))

#define ARITH(x) { context->arithmeticAccumulator = (x); }


#define LOAD(id, opt) { context->accumulator = opt(context->currentFrame->environment[id]); }
#define LOAD_MOVE(id) LOAD(id, MOVE)
#define LOAD_COPY(id) LOAD(id, IDENTITY)

#define ALLOC_PARAMS(size) { context->params.resize(size); }

#define PREPARE_PARAM(eid, pid, opt) { context->params[pid] = opt(context->currentFrame->environment[eid]); }
#define PREPARE_PARAM_MOVE(eid, pid) PREPARE_PARAM(eid, pid, MOVE)
#define PREPARE_PARAM_COPY(eid, pid) PREPARE_PARAM(eid, pid, IDENTITY)

#define STORE(id) { context->currentFrame->environment[id] = std::move(context->accumulator); }
#define STORE_ARITH(id) { context->currentFrame->environment[id] = std::make_shared<seq::IntValue>(context->arithmeticAccumulator); }
#define LOAD_ARITH { context->accumulator = std::make_shared<seq::IntValue>(context->arithmeticAccumulator); }
#define ARITH_LOAD_ACC { context->arithmeticAccumulator = INT(context->accumulator); }

#define JMP_IF_ZERO(label)	{\
		if( context->arithmeticAccumulator == 0 ) {\
			context->nextInstruction = label;\
			break;\
		}\
}

#define JMP(label)	{\
			context->nextInstruction = label;\
			break;\
}

#define RET  { context->removeLastFrame(); break;}
#define SKIP  {}

#define CALL(label, retLabel) {\
	context->currentFrame->nextInstruction = retLabel;\
	context->allocateFrame(0);\
	context->nextInstruction = label;\
	context->currentFrame->environment = std::move(context->params);\
	break;\
}

#define CALL_TAIL(label) {\
	context->nextInstruction = label;\
	context->currentFrame->environment = std::move(context->params);\
	break;\
}

#define FORK(label, params, is_complex, retLabel) {\
	if (context->getTaskQueue()->isFull() || !(is_complex)) {\
		context->currentFrame->nextInstruction = retLabel;\
		context->allocateFrame(0);\
		context->nextInstruction = label;\
		context->currentFrame->environment = std::move(params);\
		break;\
	} else {\
		context->accumulator = std::make_shared<seq::FutureValue>();\
		auto tq = context->getTaskQueue();\
		auto fcall = utils::make_unique<seq::Context>(tq);\
		fcall->allocateFrame(1);\
		fcall->currentFrame->environment[0] = context->accumulator;\
		fcall->currentFrame->nextInstruction = FINAL_LABEL;\
		fcall->allocateFrame(0);\
		fcall->nextInstruction = label;\
		fcall->currentFrame->environment = std::move(params);\
		tq->pushTask(std::move(fcall));\
	}\
}

#define CALL_FORK(label, retLabel) FORK(label, context->params, true, retLabel)

#define GLOBAL(label, p, size) {\
			context->accumulator =\
					std::make_shared<seq::FunctionApplyValue>(label, p, size, std::move(context->params), false);\
}

#define GLOBAL_FORK(label, p, size) {\
			context->accumulator =\
					std::make_shared<seq::FunctionApplyValue>(label, p, size, std::move(context->params), true);\
}

#define CONSTRUCT(cid) {\
			context->accumulator =\
					std::make_shared<seq::StructureValue>(cid, std::move(context->params));\
}

#define STORE_FIELD(fid, target) {\
	context->currentFrame->environment[target]\
		= static_cast<seq::StructureValue*>(context->accumulator.get())->getField(fid);\
}

#define JMP_CASE(targets) {\
	int t[] = targets;\
	context->nextInstruction =\
		t[static_cast<seq::StructureValue*>(context->accumulator.get())->getConstructorId()];\
	break;\
}


#define ADD_PARAM(id, retLabel, opt) {\
		auto apply = static_cast<seq::ApplyValue*>(context->accumulator.get());\
		if ( apply->getParamsAvailable() + 1 >= apply->getParamsNeeded() ) {\
			seq::CallSpecification spec;\
			apply->prepareCall(&spec);\
			spec.addParam(opt(context->currentFrame->environment[id]));\
			context->currentFrame->nextInstruction = retLabel;\
			context->allocateFrame(0);\
			context->currentFrame->environment = std::move(*spec.getParams());\
			context->nextInstruction = spec.getFunctionInstruction();\
			break;\
		} else\
			context->accumulator =\
				std::make_shared<seq::ParamApplyValue>(\
						std::static_pointer_cast<seq::ApplyValue>(std::move(context->accumulator)),\
						std::shared_ptr<seq::Value>(context->currentFrame->environment[id]));\
}

#define ADD_PARAM_MOVE(id, retLabel) ADD_PARAM(id, retLabel, MOVE)
#define ADD_PARAM_COPY(id, retLabel) ADD_PARAM(id, retLabel, IDENTITY)

#define ADD_PARAM_MOVE_FORK(id, retLabel) {\
		auto apply = static_cast<seq::ApplyValue*>(context->accumulator.get());\
		if ( apply->getParamsAvailable() + 1 >= apply->getParamsNeeded() ) {\
			seq::CallSpecification spec;\
			apply->prepareCall(&spec);\
			spec.addParam(MOVE(context->currentFrame->environment[id]));\
			FORK(spec.getFunctionInstruction(), *spec.getParams(), spec.isIsComplex(), retLabel)\
		} else\
			context->accumulator =\
				std::make_shared<seq::ParamApplyValue>(\
						std::static_pointer_cast<seq::ApplyValue>(std::move(context->accumulator)),\
						std::shared_ptr<seq::Value>(context->currentFrame->environment[id]));\
}

#define WAIT(id, retLabel) {\
	if (context->currentFrame->environment[id]->isIsFuture() ) {\
		context->nextInstruction = retLabel;\
		context = static_cast<seq::FutureValue*>(context->currentFrame->environment[id].get())->updateContext(std::move(context), id);\
		if ( context == nullptr ) { break;}\
	}\
}

#define FINALIZE {\
		(static_cast<seq::FutureValue*>(context->currentFrame->environment[0].get()))->setValueAndWakeAll(std::move(context->accumulator));\
		context = nullptr;\
		break;\
}




#endif /* INSTRUCTIONS_H_ */
