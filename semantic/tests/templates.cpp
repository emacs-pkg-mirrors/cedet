// C++ examples and requests from Klaus Berndl

template <class T, FOO> class Vector
{
private:
  static T* v;
  int sz;

public:
  T& elem(int i) {return v[i];}
  virtual ~Vector ();

protected:
  Vector ();
};

template <> class Vector <void*>
{
private:
  void** v;
  int sz;

public:
  Vector ();
  virtual int func1(int i);
  virtual int func2(int i) = 0;
  static virtual int func3(int i) = 0;
  void*& elem(int i) {return v[i];}
  //...
};

template <class T> class Vector <T*> : private Vector <void*>
{
public:
	typedef Vector <void*> Base;

	Vector () : Base() {}

	T*& elem(int i) {return static_cast<T*&>(Base::elem(i));}
	//...
};


class SomeName;
class OtherName;

