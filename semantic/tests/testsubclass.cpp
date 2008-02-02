/* Special test file for Semantic Analyzer and complex C++ inheritance.
 */

#include <iostream>
#include "testsubclass.hh"

void animal::moose::setFeet(int numfeet)
{
  if (numfeet > 4) {
    std::cerr << "Why would a moose have more than 4 feet?" << std::endl;
    return;
  }

  fFeet = numfeet;
}

int animal::moose::getFeet()
{
  return fFeet;
}

void animal::moose::doNothing()
{
  animal::moose foo();

  fFeet = 3;
}


void deer::moose::setAntlers(bool have_antlers)
{
  fAntlers = have_antlers;
}

bool deer::moose::getAntlers()
{
  return fAntlers;
}

void deer::moose::doSomething()
{
  // All these functions should be identified by semantic analyzer.
  getAntlers();
  setAntlers(true);

  getFeet();
  setFeet(true);

  doNothing();

  fSomeField = true;

  fIsValid = true;
}

void deer::alces::setLatin(bool l) {
  fLatin = l;
}

bool deer::alces::getLatin() {
  return fLatin;
}

void deer::alces::doLatinStuff(moose moosein) {
  // All these functions should be identified by semantic analyzer.
  getFeet();
  setFeet(true);

  getLatin();
  setLatin(true);

  doNothing();

  deer::moose foo();


}

moose deer::alces::createMoose()
{
  moose MooseVariableName;
  bool tmp;
  int itmp;
  bool fool;
  int fast;

  MooseVariableName = createMoose();

  doLatinStuff(MooseVariableName);
  
  tmp = this.f// -1-
    // #1# ( "fAlcesBool" "fIsValid" "fLatin" )
    ;

  itmp = this.f// -2-
    // #2# ( "fAlcesInt" "fGreek" "fIsProtectedInt" )
    ;

  tmp = f// -3-
    // #3# ( "fAlcesBool" "fIsValid" "fLatin" "fool" )
    ;

  itmp = f// -4-
    // #4# ( "fAlcesInt" "fGreek" "fIsProtectedInt" "fast" )
    ;

  MooseVariableName = m// -5-
    // #5# ( "moose" )

  return MooseVariableName;
}

/** Test Scope Changes
 *
 * This function is rigged to make sure the scope changes to account
 * for different locations in local variable parsing.
 */
int someFunction(int mPickle)
{
  moose mMoose = deer::alces::createMoose();

  if (mPickle == 1) {

    int mOption1 = 2;

    m// -5-
      // #5# ( "mMoose" "mOption1" "mPickle" )
      ;

  } else {

    int mOption2 = 2;

    m// -6-
      // #6# ( "mMoose" "mOption2" "mPickle" )
      ;
  }

}
