template <typename ptr_t>
bool lex
    ( ptr_t cursor
    , ptr_t limit
    , ptr_t marker
    , ptr_t ctxmarker
    )
{
#   define YYCTYPE     char
#   define YYCURSOR    cursor
#   define YYLIMIT     limit
#   define YYMARKER    marker
#   define YYCTXMARKER ctxmarker
#   define YYFILL(n)   { return false; }
    /*!re2c
        "int buffer " / "[" [0-9]+ "]" { return true; }
        *                              { return false; }
    */
}
