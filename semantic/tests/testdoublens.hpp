//
// Header file used in one of the ia-utest tests.
//
namespace Name1 {
  namespace Name2 {

    class Foo
    {
      typedef unsigned int Mumble;
    public:
      Foo();
      int get();

    private:
      void publishStuff(int a, int b);

      void sendStuff(int a, int b);
    
      Mumble* pMumble;
    };

  } // namespace Name2
} // namespace Name1
