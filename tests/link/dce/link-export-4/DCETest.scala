import scala.annotation.internal.DoNotDCE

object DCETest {
  @DoNotDCE def dceTest: Unit = {
    System.out.println("dceTest")
    Foo.bar()
    Foo.foo()
  }

  object Foo {

    bar()

    @scala.export def foo(): Unit = System.out.println(42)
    def bar(): Unit = System.out.println(43)
  }
}

