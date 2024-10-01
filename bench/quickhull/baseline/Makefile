CXX?=c++
CFLAGS=-O3 -Wall -pthread
hull: hull.cpp
	$(CXX) hull.cpp -o hull $(CFLAGS)

.PHONY: run

run: hull
	./hull ../data/in_circle_10M.in
	./hull ../data/kuzmin_10M.in
	./hull ../data/on_circle_10M.in
