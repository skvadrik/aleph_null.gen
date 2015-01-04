#!/bin/sh

re2c -o lex.h lex.re

g++ -W -Wall -Wextra -Weffc++ -O2 -o model_c_buffer model_c_buffer.cpp
./model_c_buffer
echo $?

g++ -W -Wall -Wextra -Weffc++ -O2 -o model_istrstream model_istrstream.cpp
./model_istrstream
echo $?
