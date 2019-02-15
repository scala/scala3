// Test that `C|Null` and `C|Nothing` are erased to `C`.

class A
class B
class C
class D

object Foo {
  // This code would not have compiled before, when `C|Null` was erased
  // to `Object`, because post-erasure we would end up with multiple methods
  // with the same signature.

  def foo(a: A|Null): Unit = {
    println("foo(A) called")
  }
  
  def foo(b: B|Null): Unit = {
    println("foo(B) called")
  }

  def foo(c: Null|C): Unit = {
    println("foo(C) called")
  }

  def foo(d: Null|D): Unit = {
    println("foo(D) called")
  }

  def bar[T <: A](a: Null|T): Unit = {
    println("bar(A) called")
  }

  def bar[T <: B](b: Null|T): Unit = {
    println("bar(B) called")
  }

  def fooz(a: A|Nothing): Unit = {
    println("fooz(A) called")
  }

  def fooz(b: B|Nothing): Unit = {
    println("fooz(B) called")
  }
}

object Test {
  def main(args: Array[String]): Unit = {
    import Foo._
    foo(new A)
    foo(new B)
    foo(new C)
    foo(new D)

    bar(new A)
    bar(new B)

    fooz(new A)
    fooz(new B)
  }
}
