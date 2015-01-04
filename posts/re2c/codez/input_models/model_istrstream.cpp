#include "lex.h"
#include "ptr_t.h"

int main ()
{
    char buffer [] = "int buffer [1024]";
    std::istringstream is (buffer);

    ptr_t cursor    (&is, 0);
    ptr_t limit     (&is, sizeof (buffer));
    ptr_t marker    (&is, 0);
    ptr_t ctxmarker (&is, 0);

    return !lex<ptr_t> (cursor, limit, marker, ctxmarker);
}
