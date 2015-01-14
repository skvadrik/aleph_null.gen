#include <sstream>

bool lex (std::istringstream & YYCURSOR, const std::streampos YYLIMIT)
{
    std::streampos YYMARKER;
    std::streampos YYCTXMARKER;
#   define YYCTYPE   char
#   define YYFILL(n) {}
    /*!re2c
        "int buffer " / "[" [0-9]+ "]" { return true; }
        *                              { return false; }
    */
}

int main ()
{
    const char buffer [] = "int buffer [1024]";
    std::istringstream is (buffer);
    return !lex (is, sizeof (buffer));
}
