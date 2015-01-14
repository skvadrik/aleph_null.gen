bool lex (const char * YYCURSOR, const char * const YYLIMIT)
{
    const char * YYMARKER;
    const char * YYCTXMARKER;
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
    return !lex (buffer, buffer + sizeof (buffer));
}
