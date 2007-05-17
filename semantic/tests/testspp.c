/* Attempt to test the C preprocessor
 *
 */

int some_fcn ()
{
}


#ifndef MOOSE
int pre_show_moose()
{
}
#endif

#ifdef MOOSE
int pre_dont_show_moose()
{
}
#endif

#if !defined(MOOSE)
int pre_show_moose_if()
{
}
#endif

#if defined(MOOSE)
int pre_dont_show_moose_if()
{
}
#endif

#define MOOSE

#if 0
int dont_show_function()
{
}
#endif

#ifdef MOOSE
int moose_function()
{
}
#endif

#ifndef MOOSE
int dont_show_moose()
{
}
#endif

#if defined(MOOSE)
int moose_function_if()
{
}
#endif

#if !defined(MOOSE)
int dont_show_moose_if() {
}
#endif

#undef MOOSE

#ifdef MOOSE
int no_handy_moose()
{
}
#endif

#define EMU

int EMU parse_around_emu(EMU)
{
}

#define SUBFLOAT float

SUBFLOAT returnanfloat()
{
}
