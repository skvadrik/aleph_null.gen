#!/bin/sh

re2c -o input_default.cpp input_default.re
g++ -W -Wall -Wextra -Weffc++ -O2 -o input_default input_default.cpp
./input_default
echo $?
