CXX?=c++
#CFLAGS=-std=c++2a -O3 -Wall -pthread
CFLAGS=-std=c++17 -O3 -Wall -pthread
hull: hull.cpp
	$(CXX) hull.cpp -o hull $(CFLAGS)

.PHONY: run

run: hull
	./hull ../data/in_circle_10M.in
	./hull ../data/kuzmin_10M.in
	./hull ../data/on_circle_10M.in
