package tests
package innerClasses

trait A:
  def baz: B
    = baz2
  def baz2: A.this.B //expected: def baz2: B
    = baz
  type B
  class C extends A:
    def foo: A.this.B
      = ???
    def foo2: B
      = ???
    def bar: B
      = ???

class T1:
  trait T
  class T2:
    trait T
    class Impl extends T1.this.T //expected: class Impl extends T
    // we get rid of the this-type above,
    // as ambiguity created by removing this-types is alleviated by links
    // (but this can be changed if needed)
