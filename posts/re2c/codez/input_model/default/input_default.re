bool lex (char * YYCURSOR, const char * YYLIMIT)
{
    char * YYMARKER;
    char * YYCTXMARKER;
#   define YYCTYPE   char
#   define YYFILL(n) {}
    /*!re2c
        "int buffer " / "[" [0-9]+ "]" { return true; }
        *                              { return false; }
    */
}

int main ()
{
    char buffer [] = "int buffer [1024]";
    return !lex (buffer, buffer + sizeof (buffer));
}
