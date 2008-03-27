/* Test CPP/SPP Replacement
 */

/* TEST: The EMU keyword doesn't screw up the function defn. */
#define EMU
#define EMU2 /*comment*/
char EMU parse_around_emu EMU2 (EMU)
{
}

/* TEST: A simple word can be replaced in a definition. */
#define SUBFLOAT /* Some Float */ float
SUBFLOAT returnanfloat()
{
}

/* TEST: Punctuation an be replaced in a definition. */
#define COLON :
int foo COLON COLON bar ()
{
}

/* TEST: Multiple lexical characters in a definition */
#define SUPER mysuper::
int SUPER baz ()
{
}

/* TEST: Macro replacement  -- NOT IMPLEMENTED YET. */
#if 0
#define INT_FCN(name) int name (int in)

INT_FCN(increment) {
  return in+1;
}
#endif

// TEST: for bad macro.
#define BAD(a) struct a { }

BAD(moose);
