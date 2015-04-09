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
class ValueWrapper;

enum ValueType {
	FUTURE_VALUE = 0,
	REAL_VALUE = 1
};

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

	std::vector<std::shared_ptr<ValueWrapper>>* getParams() {
		return &params;
	}
	~CallSpecification();

private:
	unsigned functionInstruction;
	unsigned envSize;
	std::vector<std::shared_ptr<ValueWrapper>> params;

};

// TODO: we might improve performance by moving int to this class.
class ValueWrapper {
private:
	const ValueType type;
	std::unique_ptr<Value> value;
public:
	ValueWrapper(std::unique_ptr<Value>&& value);

	const Value* getValue() {
		return value.get();
	}

	const ValueType getType() const {
		return type;
	}

	~ValueWrapper();
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
public:
	FunctionApplyValue(unsigned functionLabel, unsigned paramsNeeded, unsigned envSize)
		: ApplyValue(paramsNeeded, 0, envSize), functionInstruction(functionLabel) {}
	void prepareCall(CallSpecification* spec) const override {
		spec->setFunctionInstruction(functionInstruction);
		spec->setEnvSize(getEnvSize());
	}
};

class ParamApplyValue : public ApplyValue {
private:
	std::shared_ptr<ValueWrapper> function;
	std::shared_ptr<ValueWrapper> param;
public:
	ParamApplyValue(std::shared_ptr<ValueWrapper>&& function, std::shared_ptr<ValueWrapper>&& param)
		: ApplyValue(*static_cast<const ApplyValue*>(function->getValue())), function(std::move(function)), param(std::move(param)) {}
	void prepareCall(CallSpecification* spec) const override {
		static_cast<const ApplyValue*>(function->getValue())->prepareCall(spec);
		spec->getParams()->push_back(param);
	}
};

class StructureValue : public Value {
private:
	unsigned constructorId;
	std::vector<std::shared_ptr<ValueWrapper>> arguments;
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

std::shared_ptr<ValueWrapper> generateValueWrapper(std::unique_ptr<Value>&& value);

}

#endif /* ASM_VALUE_H_ */
