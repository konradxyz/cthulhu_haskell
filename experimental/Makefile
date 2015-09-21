


all: fibo scale

scale: scale.cpp
	g++  -std=c++11 scale.cpp -lpthread -o scale 


fibo: fibo.hs 
	ghc -O2 fibo.hs

clean:
	rm -f fibo.hi fibo.o fibo
