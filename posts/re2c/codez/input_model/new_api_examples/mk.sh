#!/bin/sh

~/devel/re2c/code-git/re2c/re2c --input custom -o input_custom_istream.cpp input_custom_istream.re
g++ -W -Wall -Wextra -Weffc++ -O2 -o input_custom_istream input_custom_istream.cpp
./input_custom_istream
echo $?

~/devel/re2c/code-git/re2c/re2c --input custom -o input_custom_default.cpp input_custom_default.re
g++ -W -Wall -Wextra -Weffc++ -O2 -o input_custom_default input_custom_default.cpp
./input_custom_default
echo $?

~/devel/re2c/code-git/re2c/re2c --input custom -o input_custom_fgetc.cpp input_custom_fgetc.re
g++ -W -Wall -Wextra -Weffc++ -O2 -o input_custom_fgetc input_custom_fgetc.cpp
./input_custom_fgetc
echo $?
