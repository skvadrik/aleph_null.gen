#include "ptr_t.h"

bool lex (ptr_t & YYCURSOR, std::streampos YYLIMIT)
{
    std::streampos YYMARKER;
    std::streampos YYCTXMARKER;
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

    ptr_t cursor (is);
    return !lex (cursor, sizeof (buffer));
}
