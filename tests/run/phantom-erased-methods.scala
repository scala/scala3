object Test {
  import Boo._

  def main(args: Array[String]): Unit = {
    val foo = new Foo

    // check that parameters have been removed from the functions
    // (would fail with NoSuchMethodException if not erased)
    foo.getClass.getDeclaredMethod("fun1")
    foo.getClass.getDeclaredMethod("fun2", classOf[String])

    try {
      foo.getClass.getDeclaredMethod("fun3")
      assert(false)
    } catch {
      case _: NoSuchMethodException => // OK
    }
  }
}

class Foo {
  import Boo._
  def fun1(unused b: BooAny): Unit = ()
  def fun2(unused b: BooAny)(s: String): Unit = ()
  unused def fun3(): BooAny = boo
}

object Boo extends Phantom {
  type BooAny = Boo.Any
  unused def boo: BooAny = assume
}
