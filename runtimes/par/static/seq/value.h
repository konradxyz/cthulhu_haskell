/*
 * value.h
 *
 *  Created on: Feb 17, 2015
 *      Author: kp
 */

#ifndef ASM_VALUE_H_
#define ASM_VALUE_H_

#include "static/utils/logging.h"
#include <vector>
#include <memory>
#include <sstream>
#include "static/utils/spinlock.h"

namespace seq {

class Function;
class Value;

class CallSpecification {
public:
	unsigned getEnvSize() const {
		return envSize;
	}

	void setEnvSize(unsigned envSize) {
		this->envSize = envSize;
		params.resize(envSize);
	}

	unsigned getFunctionInstruction() const {
		return functionInstruction;
	}

	void setFunctionInstruction(unsigned functionInstruction) {
		this->functionInstruction = functionInstruction;
	}

	std::vector<std::shared_ptr<Value>>* getParams() {
		return &params;
	}
	void addParam(std::shared_ptr<Value> val) {
		params[next_param++] = std::move(val);
	}
	void setParams(unsigned p) {
		next_param = p;
	}
	~CallSpecification();

	bool isIsComplex() const {
		return isComplex;
	}

	void setIsComplex(bool isComplex) {
		this->isComplex = isComplex;
	}

private:
	unsigned functionInstruction;
	unsigned envSize;
	unsigned next_param = 0;
	bool isComplex;
	std::vector<std::shared_ptr<Value>> params;

};


class Value {
protected:
	const bool is_future = false;
public:
	Value() : is_future(false) {}
	Value(bool is_future) : is_future(is_future) {}
	virtual std::string print() const {
		return "unknown";
	}

	virtual ~Value() {}

	const bool isIsFuture() const {
		return is_future;
	}
};

class ApplyValue : public Value {
private:
	unsigned paramsNeeded;
	unsigned envSize;
protected:
	unsigned paramsAvailable;
public:
	ApplyValue(unsigned paramsNeeded, unsigned paramsAvailable, unsigned envSize) :
			paramsNeeded(paramsNeeded),envSize(envSize),  paramsAvailable(paramsAvailable) {
	}

	unsigned getEnvSize() const {
		return envSize;
	}

	unsigned getParamsAvailable() const {
		return paramsAvailable;
	}

	unsigned getParamsNeeded() const {
		return paramsNeeded;
	}

	virtual void prepareCall(CallSpecification* spec) const = 0;
};

class FunctionApplyValue : public ApplyValue {
private:
	unsigned functionInstruction;
	std::vector<std::shared_ptr<Value>> params;
	bool is_complex;
public:
	FunctionApplyValue(unsigned functionLabel, unsigned paramsNeeded, unsigned envSize,
			std::vector<std::shared_ptr<Value>>&& params, bool is_complex)
		: ApplyValue(paramsNeeded, params.size(), envSize),
		  functionInstruction(functionLabel), params(std::move(params)), is_complex(is_complex) {}
	void prepareCall(CallSpecification* spec) const override {
		spec->setFunctionInstruction(functionInstruction);
		spec->setEnvSize(getEnvSize());
		spec->setParams(params.size());
		spec->setIsComplex(is_complex);
		for (unsigned i = 0; i < params.size(); ++i ) {
			(*spec->getParams())[i] = params[i];
		}
	}
};

class ParamApplyValue : public ApplyValue {
private:
	std::shared_ptr<ApplyValue> function;
	std::shared_ptr<Value> param;
public:
	ParamApplyValue(std::shared_ptr<ApplyValue>&& function, std::shared_ptr<Value>&& param)
		: ApplyValue(*function), function(std::move(function)), param(std::move(param)) {
		++paramsAvailable;
	}
	void prepareCall(CallSpecification* spec) const override {
		function->prepareCall(spec);
		spec->addParam(param);
	}
};

class StructureValue : public Value {
private:
	unsigned constructorId;
	std::vector<std::shared_ptr<Value>> arguments;
public:
	StructureValue(unsigned cid, std::vector<std::shared_ptr<Value>>&& params)
		: constructorId(cid), arguments(std::move(params)) {}
	std::shared_ptr<Value> getField(unsigned id) {
		return arguments[id];
	}
	unsigned getConstructorId() {
		return constructorId;
	}
};

class IntValue : public Value {
public:
	std::string print() const override {
		std::ostringstream ss;
		ss << value;
		std::string str = ss.str();

		return std::string("(int ") + str + ")";
	}
	int getValue() const {
		return value;
	}
	IntValue(int value) : value(value) {}
private:
	int value;
};

struct ContextUpdateRequest {
	std::unique_ptr<Context> context;
	unsigned id;
	ContextUpdateRequest(std::unique_ptr<Context>&& context, unsigned id) : context(std::move(context)), id(id) {}

	~ContextUpdateRequest();
};


class FutureValue : public Value {
public:
	FutureValue() : Value(true) {}

	// This one will be used only once, when all working threads finish.
	std::shared_ptr<Value> getFuture() {
		return future;
	}
	std::string print() const override {
		return "future";
	}
	std::unique_ptr<Context> updateContext(std::unique_ptr<Context>&& context,
			unsigned id) {
		auto frame = context->currentFrame;
		auto tmp = std::move(frame->environment[id]);
		{
			std::lock_guard<utils::SpinLock> guard(lock);
			if (future != nullptr) {
				frame->environment[id] = future;
				return std::move(context);
			} else {
				waiting.push_back(utils::make_unique<ContextUpdateRequest>(std::move(context), id));
				return nullptr;
			}
		}
	}

	void setValueAndWakeAll(std::shared_ptr<Value>&& value) {
		std::lock_guard<utils::SpinLock> guard(lock);
		std::vector<std::unique_ptr<Context>> contexts;
		this->future = std::move(value);
		for (auto& request : waiting) {
			auto frame = request->context->currentFrame;
			frame->environment[request->id] = future;
			contexts.push_back(std::move(request->context));
		}

		if ( contexts.size() > 0 ) {
			auto queue = contexts[0]->getTaskQueue();
			queue->pushTasks(&contexts);
		}
	}

private:
	std::shared_ptr<Value> future = nullptr;
	std::vector<std::unique_ptr<ContextUpdateRequest>> waiting;
	utils::SpinLock lock;

};

}

#endif /* ASM_VALUE_H_ */
