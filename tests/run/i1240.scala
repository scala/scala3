// A tricky case of overriding behavior
// Note: It might be acceptable if this produced an error instead.
// But testing this is tricky.
abstract class Base[T] {
    def foo(x: T): String
}

class C[T] extends Base[T] {

    def foo(x: D): String = "D foo"
    def foo(x: T): String = "T foo"
}

object Test {
  def main(args: Array[String]) = {
    val b1: Base[D] = new C[D] // which of the two foo's in C overrides the one in B?
    assert(b1.foo(new D) == "T foo")
    val b2: Base[D] = new C[D] {}
      // In Java, this gives an error like this:
      // methods foo(A) from C[D] and foo(String) from C[D] are inherited with the same signature
      // But the analogous example with `b1` compiles OK in Java.
    assert(b2.foo(new D) == "D foo")
      // Here we get "D foo" since a bridge method for foo(x: D) was inserted
      // in the anonymous class of b2.
  }
}

class D

