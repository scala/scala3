import scala.reflect.ClassTag

class A[Foo](implicit tag: ClassTag[Foo]) {
  object ExtractFoo {
    def unapply(foo: Foo): Boolean = true
  }

  def isFoo(x: Any) = x match {
    case ExtractFoo() => true
    //case foo: Foo => true
    case _ => false
  }

  def testBind(x: Any) = x match {
    case foo0: Foo =>
      (foo0: Foo)
    case (_: Foo) as foo1 =>
      (foo1: Foo)
    case ExtractFoo() as foo2 =>
      (foo2: Foo)
  }
}

object Test {
  def main(args: Array[String]): Unit = {
    assert((new A[String]).isFoo("foo")) // OK
    assert(!(new A[String]).isFoo(42)) // OK in scalac, fails in Dotty
  }
}
