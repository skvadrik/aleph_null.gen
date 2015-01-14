#!/bin/sh

re2c -o input_istream.cpp input_istream.re
g++ -W -Wall -Wextra -Weffc++ -O2 -o input_istream input_istream.cpp
./input_istream
echo $?
