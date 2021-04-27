/* subtyping logs
==> isSubType Test.this.foo.Bar(Test.this.foo.bar) <:< Foo.this.Bar?
  ==> isSubType Test.this.foo.Bar <:< Foo.this.Bar?
    ==> isSubType Foo(Test.this.foo) <:< Foo(Foo.this)?
      ==> isSubType Foo <:< Foo(Foo.this)?
      <== isSubType Foo <:< Foo(Foo.this) = false
    <== isSubType Foo(Test.this.foo) <:< Foo(Foo.this) = false
  <== isSubType Test.this.foo.Bar <:< Foo.this.Bar = false
<== isSubType Test.this.foo.Bar(Test.this.foo.bar) <:< Foo.this.Bar = false
*/


class Foo {
  val bar = new Bar
  class Bar {
    sealed abstract class A
    case class B() extends A
    case class C() extends A
  }
}

class Test {
  val foo = new Foo
  import foo.bar.*

  def h(a: A) = {
    a match {
      case B() => 1
      case _ => 2 // unreachable code
    }
  }

  def f(a: A) =
    a match {
      case B() => 1
    }
}
