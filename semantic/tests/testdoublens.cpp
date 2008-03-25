//
// CPP file for semantic-ia-utest
// completion engine unit tests.
//
#include "testdoublens.hpp"

namespace Name1 {
  namespace Name2 {

    Foo::Foo()
    {
      p// -1-
	// #1# ( "pMumble" "publishStuff" )
	;
    }

    int Foo::get()
    {
      p// -2-
	// #2# ( "pMumble" "publishStuff" )
	;
      return 0;
    }

    void Foo::publishStuff(int /* a */, int /* b */)
    {
    }

    void Foo::sendStuff(int /* a */, int /* b */)
    {
    }
    
  } // namespace Name2
} // namespace Name1


// Second test from Ravikiran Rajagopal

namespace A { 
  class foo {
  public:
    void aa();
    void bb();
  };
}
namespace A { 
  class bar { 
  public:
    void xx();
  public:
    foo myFoo;
  };

  void bar::xx()
  {
    myFoo.// -3- <--- cursor is here after the dot
      // #3# ( "aa" "bb" )
      ;
  }
}
