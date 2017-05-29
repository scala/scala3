import dotty.DottyPredef.{eqAny => _, _}

object equality1 {
  class A
  class B
  new A == new B // error: cannot compare
}
