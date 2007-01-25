/* Special test file for Semantic Analyzer and complex C++ inheritance.
 */

#include "testsubclass.hh"

// The below should test inclusion of lexical macro symbols
// but since it doesn't work, I've reversed it for purposes
// of testing other things.

#define TESTSUBCLASS_HH
#ifndef TESTSUBCLASS_HH

namespace animal {

  class moose {
  public:
    moose() : fFeet(0)
    { }

    void setFeet(int);
    int getFeet();

  private:
    int fFeet; // Usually 2 or 4.
  };

} // moose


namespace deer {

  class moose : public animal::moose {
  public:
    moose() : fAntlers(false)
    { }

    void setAntlers(bool);
    bool getAntlers();

  private:
    bool fAntlers;

  };

} // dear

#endif

void animal::moose::setFeet(int numfeet)
{
  fFeet = numfeet;
}

int animal::moose::getFeet()
{
  return fFeet;
}


void deer::moose::setAntlers(bool have_antlers)
{
  fAntlers = have_antlers;
}

bool deer::moose::getAntlers()
{
  return fAntlers;
}





