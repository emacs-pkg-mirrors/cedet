/* Special test file for Semantic Analyzer and complex C++ inheritance.
 *
 * Header file for testsubclass.cpp with class defns but no
 * implementations.
 */

#include <cmath>
#include <stdio.h>

#ifndef TESTSUBCLASS_HH
#define TESTSUBCLASS_HH

namespace animal {

  class moose {
  public:
    moose() : fFeet(0),
	      fIsValid(false)
    { }

    void setFeet(int);
    int getFeet();

    void doNothing();

  protected:

    bool fIsValid;
    int fIsProtectedInt;

  private:
    int fFeet; // Usually 2 or 4.
    bool fIsPrivateBool;
    
  };

  int two_prototypes();
  int two_prototypes();

} // moose


namespace deer {

  class moose : public animal::moose {
  public:
    moose() : fAntlers(false)
    { }

    void setAntlers(bool);
    bool getAntlers();

    void doSomething();

  protected:

    bool fSomeField;

  private:
    bool fAntlers;

  };

} // deer

// A second namespace of the same name will test the
// namespace merging needed to resolve deer::alces
namespace deer {

  class alces : public animal::moose {
  public:
    alces(int lat) : fLatin(lat)
    { }

    void setLatin(bool);
    bool getLatin();

    void doLatinStuff(moose moosein); // for completion testing

    moose createMoose(); // for completion testing.

  protected:
    bool fAlcesBool;
    int fAlcesInt;

  private:
    bool fLatin;
    int fGreek;
  };

};
#endif
