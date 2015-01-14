/* Generated by re2c 0.13.8.dev on Tue Jan 13 18:41:46 2015 */
#line 1 "input_custom_fgetc.re"
#include <stdio.h>

char peek (FILE * f)
{
    char c = fgetc (f);
    ungetc (c, f);
    return c;
}

bool lex (FILE * f, const long limit)
{
    long marker;
    long ctxmarker;
#   define YYCTYPE        char
#   define YYPEEK()       peek (f)
#   define YYSKIP()       fgetc (f)
#   define YYBACKUP()     marker = ftell (f)
#   define YYBACKUPCTX()  ctxmarker = ftell (f)
#   define YYRESTORE()    fseek (f, marker, SEEK_SET)
#   define YYRESTORECTX() fseek (f, ctxmarker, SEEK_SET)
#   define YYHAS(n)       limit - ftell (f) < n
#   define YYFILL(n)      {}
    
#line 27 "input_custom_fgetc.cpp"
{
	YYCTYPE yych;

	if (YYHAS (13)) YYFILL(13);
	yych = YYPEEK ();
	switch (yych) {
	case 'i':	goto yy4;
	default:	goto yy2;
	}
yy2:
	YYSKIP ();
yy3:
#line 25 "input_custom_fgetc.re"
	{ return false; }
#line 42 "input_custom_fgetc.cpp"
yy4:
	YYSKIP ();
	YYBACKUP ();
	yych = YYPEEK ();
	switch (yych) {
	case 'n':	goto yy5;
	default:	goto yy3;
	}
yy5:
	YYSKIP ();
	yych = YYPEEK ();
	switch (yych) {
	case 't':	goto yy7;
	default:	goto yy6;
	}
yy6:
	YYRESTORE ();
	goto yy3;
yy7:
	YYSKIP ();
	yych = YYPEEK ();
	switch (yych) {
	case ' ':	goto yy8;
	default:	goto yy6;
	}
yy8:
	YYSKIP ();
	yych = YYPEEK ();
	switch (yych) {
	case 'b':	goto yy9;
	default:	goto yy6;
	}
yy9:
	YYSKIP ();
	yych = YYPEEK ();
	switch (yych) {
	case 'u':	goto yy10;
	default:	goto yy6;
	}
yy10:
	YYSKIP ();
	yych = YYPEEK ();
	switch (yych) {
	case 'f':	goto yy11;
	default:	goto yy6;
	}
yy11:
	YYSKIP ();
	yych = YYPEEK ();
	switch (yych) {
	case 'f':	goto yy12;
	default:	goto yy6;
	}
yy12:
	YYSKIP ();
	yych = YYPEEK ();
	switch (yych) {
	case 'e':	goto yy13;
	default:	goto yy6;
	}
yy13:
	YYSKIP ();
	yych = YYPEEK ();
	switch (yych) {
	case 'r':	goto yy14;
	default:	goto yy6;
	}
yy14:
	YYSKIP ();
	yych = YYPEEK ();
	switch (yych) {
	case ' ':	goto yy15;
	default:	goto yy6;
	}
yy15:
	YYBACKUPCTX ();
	YYSKIP ();
	yych = YYPEEK ();
	switch (yych) {
	case '[':	goto yy16;
	default:	goto yy6;
	}
yy16:
	YYSKIP ();
	yych = YYPEEK ();
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
	YYSKIP ();
	if (YYHAS (1)) YYFILL(1);
	yych = YYPEEK ();
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
	YYSKIP ();
	YYRESTORECTX ();
	YYSKIP ();
#line 24 "input_custom_fgetc.re"
	{ return true; }
#line 165 "input_custom_fgetc.cpp"
}
#line 26 "input_custom_fgetc.re"

}

int main ()
{
    const char buffer [] = "int buffer [1024]";
    const char fn [] = "input.txt";

    FILE * f = fopen (fn, "w");
    fwrite (buffer, 1, sizeof (buffer), f);
    fclose (f);

    f = fopen (fn, "rb");
    int result = !lex (f, sizeof (buffer));
    fclose (f);

    return result;
}
