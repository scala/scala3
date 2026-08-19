//> using options -Yexplicit-nulls
import language.experimental.magic
object Bar {
  class Foo {
    def m1(i: Int) = i+1
    def m2(i: Int) = i+2
  }
  def unapply(using f1: Foo)(i: Int): Int? =
    if i == 0 then f1.m1(i) else f1.m2(i)

  given Foo = new Foo
  val i1: Int = 0
  val i2: Int = i2 match // warn
    case Bar(i) => i
    case _ => 0
}