CXXFLAGS =	 -O2 -Wall -fmessage-length=0 -std=c++0x -I.

OBJS =

LIBS =

TARGET =	seq


all:	$(TARGET)

# $1 - directory, $2, -name $3 - headers
define obj
$(info $1/$2)
OBJS += static/$(1)/$(2).o
static/$(1)/$(2).o: static/$(1)/$(2).cpp static/$(1)/$(2).h $3
	$(CXX) $(CXXFLAGS) -c -o static/$1/$2.o static/$1/$2.cpp
endef


$(eval $(call obj,seq,context,static/seq/value.h))

$(eval $(call obj,seq,value))

$(eval $(call obj,seq,seq, gen.h static/seq/seq.h))


$(TARGET):	$(OBJS) gen.h
	$(CXX) -o $(TARGET) $(OBJS) $(LIBS)


clean:
	rm -f $(OBJS) $(TARGET)
