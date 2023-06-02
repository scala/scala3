package tests
package supertypeParamsSubstitution

class MyIter[A, CC[_], C]:
  def foo: A
    = ???
  def bar: CC[CC[A]]
    = ???
  def baz: C
    = ???

class MyList[T] extends MyIter[T, MyList, MyList[T]]
//expected: def foo: T
//expected: def bar: MyList[MyList[T]]
//expected: def baz: MyList[T]

class MyListInt extends MyList[Int]
//expected: def foo: Int
//expected: def bar: MyList[MyList[Int]]
//expected: def baz: MyList[Int]
