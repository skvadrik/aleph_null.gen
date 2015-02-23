---
title: "--input custom" examples
---

--------------------------
"\--input custom" examples
--------------------------

.. _RE2C: http://re2c.org/manual.html

This RE2C_ switch enables generic input API:

    ===
    YYPEEK ()
    YYSKIP ()
    YYBACKUP ()
    YYBACKUPCTX ()
    YYRESTORE ()
    YYRESTORECTX ()
    YYLESSTHAN (n)
    ===

Let's see how conventional input models fit into it.

pointers to plain buffer
========================

.. code-block:: c

    bool lex (const char * cursor, const char * const limit)
    {
        const char * marker;
        const char * ctxmarker;
    #   define YYCTYPE        char
    #   define YYPEEK()       *cursor
    #   define YYSKIP()       ++cursor
    #   define YYBACKUP()     marker = cursor
    #   define YYBACKUPCTX()  ctxmarker = cursor
    #   define YYRESTORE()    cursor = marker
    #   define YYRESTORECTX() cursor = ctxmarker
    #   define YYLESSTHAN(n)  limit - cursor < n
    #   define YYFILL(n)      {}
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

STL stream
==========

.. code-block:: c

    #include <sstream>

    bool lex (std::istringstream & is, const std::streampos limit)
    {
        std::streampos marker;
        std::streampos ctxmarker;
    #   define YYCTYPE        char
    #   define YYPEEK()       is.peek ()
    #   define YYSKIP()       is.ignore ()
    #   define YYBACKUP()     marker = is.tellg ()
    #   define YYBACKUPCTX()  ctxmarker = is.tellg ()
    #   define YYRESTORE()    is.seekg (marker)
    #   define YYRESTORECTX() is.seekg (ctxmarker)
    #   define YYLESSTHAN(n)  limit - is.tellg () < n
    #   define YYFILL(n)      {}
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

stdio.h
=======

.. code-block:: c

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
    #   define YYLESSTHAN(n)  limit - ftell (f) < n
    #   define YYFILL(n)      {}
        /*!re2c
            "int buffer " / "[" [0-9]+ "]" { return true; }
            *                              { return false; }
        */
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

This one is not particularly efficient: I had to use 'fgets'/'ungetc' pair to emulate 'peek'.

updates
=======

* 2015-02-23: Renamed 'YYHAS(n)' to 'YYLESSTHAN(n)'.
  This primitive actually means "is there less than 'n' input characters left?"
