all: watch_nacl
clean:
	rm -f watch_nacl
watch_nacl: watch_nacl.cc
	g++ -g -pthread -std=c++11 -O3 -o $@ $@.cc
