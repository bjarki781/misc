CXX := clang++-4.0
CFLAGS := -Wall -std=c++11 $(shell pkg-config --cflags glfw3)
LIBS := -lGLEW $(shell pkg-config --static --libs glfw3)

all: test

test: ogl
	./ogl multisampling

ogl: ogl.cpp loadshaders.cpp diasqr.hpp
	$(CXX) ogl.cpp $(CFLAGS) $(LIBS) loadshaders.cpp -lGL -o ogl

