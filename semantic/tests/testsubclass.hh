/* Special test file for Semantic Analyzer and complex C++ inheritance.
 *
 * Header file for testsubclass.cpp with class defns but no
 * implementations.
 */

#include <cmath>

#ifndef TESTSUBCLASS_HH
#define TESTSUBCLASS_HH

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

} // deer

#endif
