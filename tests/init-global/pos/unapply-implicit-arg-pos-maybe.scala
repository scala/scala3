//> using options -Yexplicit-nulls
import language.experimental.magic
object Bar {
  class Foo {
    def m1(i: Int) = i + i1
    def m2(i: Int) = i + 2
  }
  def unapply(using f1: Foo)(using f2: Foo)(i: Int)(using f3: Foo): Int? =
    if i == 0 then f1.m1(i1) + f3.m1(i1) else f2.m2(i) + f3.m2(i)

  given Foo = new Foo
  val i1: Int = 0
  val i2: Int = i1 match
    case Bar(i) => i
    case _ => 0
}