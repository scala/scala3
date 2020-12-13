// SI-8197, see also SI-4592 and SI-4728
class A
class B

class Foo(val x: A = null) {
  def this(bla: B*) = {
    this(new A)
  }
}

object Test extends App {
  // both constructors of `Foo` are applicable and neither is more specific
  // than the other. As a fallback, overloading resolution will eliminate the
  // alternative that uses a default argument, therefore the vararg constructor is chosen.
  assert((new Foo).x != null)
}
