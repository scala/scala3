
object DCETest {
  @scala.annotation.internal.DoNotDCE def dceTest: Unit = {
    val x = new Foo
    Test.shouldDCE(x.bar())
    x.foo()
  }
}

class Foo {
  @scala.export def foo(): Unit = System.out.println(42)
  def bar(): Unit = System.out.println(43)
}
