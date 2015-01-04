#include "lex.h"

int main ()
{
    char buffer [] = "int buffer [1024]";

    char * cursor    = buffer;
    char * limit     = buffer + sizeof (buffer);
    char * marker    = buffer;
    char * ctxmarker = buffer;

    return !lex<char *> (cursor, limit, marker, ctxmarker);
}
