/* Test file for C++ language.
 * Attempt to include as many aspects of the C++ language as possible.
 * Do not include things tested in test.c since that shares the
 * same language.
 *
 * $Id: test.cpp,v 1.10 2002/02/27 13:37:35 zappo Exp $
 *
 */

/* An include test */
#include "c++-test.hh"

double var1 = 1.2;

struct foo1 {
  int test;
};

struct foo2 : public foo1 {
  const int foo21(int a, int b);
  const int foo22(int a, int b) { return 1 }
};

/* Classes */
class class1 {
private:
  int var11;
  struct foo var12;
public:
  int p_var11;
  struct foo p_var12;
};

class i_class1 : public class1 {
private:
  int var11;
  struct foo var12;
public:
  int p_var11;
  struct foo p_var12;
};

class class2 {
private:
  int var21;
  struct foo var22;
public:
  int p_var21;
  struct foo p_var22;
};

class i_class2 : public class1, public class2 {
private:
  int var21;
  struct foo var22;
protected:
  int pt_var21;
public:
  int p_var21;
  struct foo p_var22;
};

class class3 {
  /* A class with strange things in it */
public:
  class3(); /* A constructor */
  enum embedded_enum {
    a, b, c
  };
  struct embedded_struct {
    int a;
    int b;
  };
  class embedded_class {
    embedded_class();
    ~embedded_class();
  };
  ~class3(); /* destructor */
  
  /* Methods */
  int method_for_class3(int a, char b);
  int inline_method(int c) { return c; }

  /* Operators */
  class3& operator= (const class3& something);

  /* Funny declmods */
  const class3 * const method_const_ptr_ptr(const int * const argconst) const = 0;
};

class3::class3()
{
  /* Constructor outside the definition. */
}

int class3::method1_for_class3( int a, int &b)
{
  int a;
  class3 foo;

  // Completion testing line should find external members.
  a = foo.m;

  return 1;
}

char class3::method2_for_class3( int a, int b) throw ( exception1 )
{
  return 'a';
}

void *class3::method3_for_class3( int a, int b) throw ( exception1, exception2 )
{
  int q = a;
  return "Moose";
}

void *class3::method4_for_class3( int a, int b) reentrant
{
}

void *class3::method5_for_class3( int a, int b) const
{
}

// Stuff Klaus found.
// Inheritance w/out a specifying for public.
class class4 : class1 {
  // Pure virtual methods.
  void virtual print () const = 0;

};

class class5 : public virtual class4 {
  // Virtual inheritance
};

/* Namespaces */
namespace namespace1 {
  void ns_method1() { }

  class n_class1 {
  public:
    void method11(int a) { }
  };

  /* This shouldn't parse due to missing semicolon. */
  class _n_class2 : public n_class1 {
    void n_c2_method1(int a, int b) { }
  }
    
}

/* Initializers */
void tinitializers1(): inita1(False),
		       inita2(False)
{
  inita1= 1;
}

/* How about Extern C type things. */

extern "C"
int extern_c_1(int a, int b)
{
  return 1;
}

extern "C" {

  int extern_c_2(int a, int b)
  {
    return 1;
  }

}

/*
 * Ok, how about some template stuff.
 */
template <class CT, class container = vector<CT> >
const CT& max (const CT& a, const CT& b)
{
  return a < b ? b : a;
}

class TemplateUsingClass
{
  typedef map<long, long> TestClassMap;
  typedef TestClassMap::iterator iterator;

  map<int, int> mapclassvarthingy;
};
