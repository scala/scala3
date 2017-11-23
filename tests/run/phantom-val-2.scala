
object Test {
  def main(args: Array[String]): Unit = {
    val f = new Foo
    println(1)
    bar(f.foo)
    bar(f.foo)
    assert(!f.getClass.getDeclaredFields.exists(_.getName.startsWith("foo")), "field foo not erased")
  }

  def bar(unused x: Boo.BooAny) = ()
}

class Foo {
  import Boo._
  unused val foo = {
    println("foo")
    any
  }
}

object Boo extends Phantom {
  type BooAny = this.Any
  unused def any: BooAny = assume
}
