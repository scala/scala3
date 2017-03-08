
object Test {
  import Boo._

  def main(args: Array[String]): Unit = {
    val f = new Foo
    f.foo
    f.foo
    f.foo = {
      println("foo3")
      any
    }
    f.foo
    assert(!f.getClass.getDeclaredFields.exists(_.getName.startsWith("foo")), "field foo not erased")
  }
}

class Foo {
  import Boo._

  var foo = {
    println("foo")
    any
  }

  foo = {
    println("foo2")
    any
  }
}

object Boo extends Phantom {
  type BooAny = this.Any
  def any: BooAny = assume
}
