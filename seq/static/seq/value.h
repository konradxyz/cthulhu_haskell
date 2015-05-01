/*
 * value.h
 *
 *  Created on: Feb 17, 2015
 *      Author: kp
 */

#ifndef ASM_VALUE_H_
#define ASM_VALUE_H_

#include <vector>
#include <memory>
#include <sstream>

namespace seq {

class Function;
class Value;
class ContextBase;

class CallSpecification {
public:
	unsigned getEnvSize() const {
		return envSize;
	}

	void setEnvSize(unsigned envSize) {
		this->envSize = envSize;
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
	~CallSpecification();

private:
	unsigned functionInstruction;
	unsigned envSize;
	std::vector<std::shared_ptr<Value>> params;

};


class Value {
public:
	virtual std::string print() const {
		return "unknown";
	}

	virtual ~Value() {}
};

class ApplyValue : public Value {
private:
	unsigned paramsNeeded;
	unsigned paramsAvailable;
	unsigned envSize;
public:
	ApplyValue(unsigned paramsNeeded, unsigned paramsAvailable, unsigned envSize) :
			paramsNeeded(paramsNeeded), paramsAvailable(paramsAvailable), envSize(envSize) {
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
public:
	FunctionApplyValue(unsigned functionLabel, unsigned paramsNeeded, unsigned envSize,
			std::vector<std::shared_ptr<Value>>&& params)
		: ApplyValue(paramsNeeded, params.size(), envSize), functionInstruction(functionLabel), params(std::move(params)) {}
	void prepareCall(CallSpecification* spec) const override {
		spec->setFunctionInstruction(functionInstruction);
		spec->setEnvSize(getEnvSize());
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
		: ApplyValue(*function), function(std::move(function)), param(std::move(param)) {}
	void prepareCall(CallSpecification* spec) const override {
		function->prepareCall(spec);
		spec->getParams()->push_back(param);
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

}

#endif /* ASM_VALUE_H_ */
