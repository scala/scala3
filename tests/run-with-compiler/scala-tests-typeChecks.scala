import scala.testing._

object Test {

  trait Eq[T]
  implicit val eq: Eq[Int] = new Eq[Int] {}

  implicit class AnyOps[T](x: T) {
    def === (y: T)(implicit c: Eq[T]) = x == y
  }

  def main(args: Array[String]): Unit = {
    assert(typeChecks("5 === 5"))
    assert(!typeChecks("5.6 === 7.7"))

    val x: Int = 5
    assert(typeChecks("x + 3"))
    assert(!typeChecks("y + 3"))
    import scala.util.Left
    assert(typeChecks("Left(3)"))
    assert(!typeChecks("Rigth(3)"))

    def f(x: Int): Int = x * x
    assert(typeChecks("f(3)"))
    assert(!typeChecks("g(3)"))

    type T
    assert(typeChecks("def foo(x: T): T = x"))
    assert(!typeChecks("foo(???)"))
    assert(!typeChecks("def foo(x: S): S = x"))

    assert(!typeChecks("def test(x: Int) ="))

    assert(typeChecks(
      """
      class EqString extends Eq[String]
      new EqString
      """
    ))
  }
}
