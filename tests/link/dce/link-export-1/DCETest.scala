import scala.annotation.internal.DoNotDCE

object DCETest {
  @DoNotDCE def dceTest: Unit = {
    val x = new Foo
    Test.shouldDCE(x.bar())
    x.foo()
  }
}

class Foo {
  @scala.export def foo(): Unit = System.out.println(42)
  def bar(): Unit = System.out.println(43)
}
