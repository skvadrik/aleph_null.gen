#!/bin/sh

g++ -W -Wall -Wextra -Weffc++ -O2 -o input_istream input_istream.cpp
./input_istream
echo $?
