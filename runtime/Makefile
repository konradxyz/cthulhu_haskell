CXXFLAGS =	-O2 -g -Wall -fmessage-length=0 -std=c++0x -I.

OBJS =		runtime.o

LIBS =

TARGET =	runtime


# $1 - directory, $2, -name $3 - headers
define obj
$(info $1/$2)
OBJS += $(1)/$(2).o
$(1)/$(2).o: $(1)/$(2).cpp $(1)/$(2).h $3
	$(CXX) $(CXXFLAGS) -c -o $1/$2.o $1/$2.cpp
endef

$(eval $(call obj,data,contextbase,data/context.h utils/spinlock.h))

$(eval $(call obj,data,futurecontext,data/context.h data/value.h utils/spinlock.h))

$(eval $(call obj,data,context,data/value.h))

$(eval $(call obj,data,value,data/futurecontext.h utils/spinlock.h))


$(TARGET):	$(OBJS)
	$(CXX) -o $(TARGET) $(OBJS) $(LIBS)

all:	$(TARGET)

clean:
	rm -f $(OBJS) $(TARGET)
