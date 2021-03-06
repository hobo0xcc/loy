CXX:=g++
CXXFLAGS:=$(shell llvm-config --cxxflags --ldflags --system-libs --libs all)
RM:=rm -rf
SRC:=$(wildcard src/*.cpp)

loy: $(SRC)
	$(CXX) -g $(CXXFLAGS) -std=c++17 -o $@ $^
	
clean:
	$(RM) loy loy.dSYM a.out main.o
