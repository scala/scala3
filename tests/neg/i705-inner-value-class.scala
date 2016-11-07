class Foo {
  class B(val a: Int) extends AnyVal // error: value class may not be a member of another class
}

class VCwithBadMembers(val a: Int) extends AnyVal {
  def this() = this(1) // error: value class may not define secondary constructor
  var x = 0 // error: value class may not define non-parameter field
  val y = 2 // error: value class may not define non-parameter field
  println("hi") // error: value class may not contain initialization statements
}

object Test {
  class B(val a: Int) extends AnyVal // ok
  def f = {
    class C(val a: Int) extends AnyVal // error: value class may not be a local class
    new C(1)
  }
}


