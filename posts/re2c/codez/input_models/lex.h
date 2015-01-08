/* Generated by re2c 0.13.7.5 on Thu Dec 11 16:06:50 2014 */
#line 1 "lex.re"
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
    
#line 19 "lex.h"
{
	YYCTYPE yych;

	if ((YYLIMIT - YYCURSOR) < 13) YYFILL(13);
	yych = *YYCURSOR;
	switch (yych) {
	case 'i':	goto yy4;
	default:	goto yy2;
	}
yy2:
	++YYCURSOR;
yy3:
#line 17 "lex.re"
	{ return false; }
#line 34 "lex.h"
yy4:
	yych = *(YYMARKER = ++YYCURSOR);
	switch (yych) {
	case 'n':	goto yy5;
	default:	goto yy3;
	}
yy5:
	yych = *++YYCURSOR;
	switch (yych) {
	case 't':	goto yy7;
	default:	goto yy6;
	}
yy6:
	YYCURSOR = YYMARKER;
	goto yy3;
yy7:
	yych = *++YYCURSOR;
	switch (yych) {
	case ' ':	goto yy8;
	default:	goto yy6;
	}
yy8:
	yych = *++YYCURSOR;
	switch (yych) {
	case 'b':	goto yy9;
	default:	goto yy6;
	}
yy9:
	yych = *++YYCURSOR;
	switch (yych) {
	case 'u':	goto yy10;
	default:	goto yy6;
	}
yy10:
	yych = *++YYCURSOR;
	switch (yych) {
	case 'f':	goto yy11;
	default:	goto yy6;
	}
yy11:
	yych = *++YYCURSOR;
	switch (yych) {
	case 'f':	goto yy12;
	default:	goto yy6;
	}
yy12:
	yych = *++YYCURSOR;
	switch (yych) {
	case 'e':	goto yy13;
	default:	goto yy6;
	}
yy13:
	yych = *++YYCURSOR;
	switch (yych) {
	case 'r':	goto yy14;
	default:	goto yy6;
	}
yy14:
	yych = *++YYCURSOR;
	switch (yych) {
	case ' ':	goto yy15;
	default:	goto yy6;
	}
yy15:
	YYCTXMARKER = YYCURSOR + 1;
	yych = *++YYCURSOR;
	switch (yych) {
	case '[':	goto yy16;
	default:	goto yy6;
	}
yy16:
	yych = *++YYCURSOR;
	switch (yych) {
	case '0':
	case '1':
	case '2':
	case '3':
	case '4':
	case '5':
	case '6':
	case '7':
	case '8':
	case '9':	goto yy17;
	default:	goto yy6;
	}
yy17:
	++YYCURSOR;
	if (YYLIMIT <= YYCURSOR) YYFILL(1);
	yych = *YYCURSOR;
	switch (yych) {
	case '0':
	case '1':
	case '2':
	case '3':
	case '4':
	case '5':
	case '6':
	case '7':
	case '8':
	case '9':	goto yy17;
	case ']':	goto yy19;
	default:	goto yy6;
	}
yy19:
	++YYCURSOR;
	YYCURSOR = YYCTXMARKER;
#line 16 "lex.re"
	{ return true; }
#line 143 "lex.h"
}
#line 18 "lex.re"

}