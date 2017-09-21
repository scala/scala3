import dotty.runtime.ErasedPhantom

object Test {
  import Boo._

  def main(args: Array[String]): Unit = {
    val foo = new Foo

    // check that parameters have been removed from the functions
    // (would fail with NoSuchMethodException if not erased)
    foo.getClass.getDeclaredMethod("fun1")
    foo.getClass.getDeclaredMethod("fun2", classOf[String])

    assert(foo.getClass.getDeclaredMethod("fun3").getReturnType == classOf[ErasedPhantom])
  }
}

class Foo {
  import Boo._
  def fun1(b: BooAny): Unit = ()
  def fun2(b: BooAny, s: String): Unit = ()
  def fun3(): BooAny = boo
}

object Boo extends Phantom {
  type BooAny = Boo.Any
  def boo: BooAny = assume
}
