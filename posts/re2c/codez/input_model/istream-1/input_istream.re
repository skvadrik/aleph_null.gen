#include "ptr_t.h"

bool lex (ptr_t & YYCURSOR, ptr_t & YYLIMIT)
{
    ptr_t YYMARKER    = YYCURSOR;
    ptr_t YYCTXMARKER = YYCURSOR;
#   define YYCTYPE   char
#   define YYFILL(n) { return false; }
    /*!re2c
        "int buffer " / "[" [0-9]+ "]" { return true; }
        *                              { return false; }
    */
}

int main ()
{
    char buffer [] = "int buffer [1024]";
    std::istringstream is (buffer);

    ptr_t cursor (&is, 0);
    ptr_t limit  (&is, sizeof (buffer));

    return !lex (cursor, limit);
}
