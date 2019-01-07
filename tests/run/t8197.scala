// SI-8197, see also SI-4592 and SI-4728
class A(val tag: String)
class B

class Foo(val x: A = new A("default")) {
  def this(bla: B*) = {
    this(new A("varargs"))
  }
}

object Test extends dotty.runtime.LegacyApp {
  // both constructors of `Foo` are applicable. Overloading resolution
  // will eliminate the alternative that uses a default argument, therefore
  // the vararg constructor is chosen.
  assert((new Foo).x.tag == "varargs")
}
