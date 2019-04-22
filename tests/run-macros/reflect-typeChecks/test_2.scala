object Test {
  import scalatest._

  trait Eq[T]
  implicit val eq: Eq[Int] = new Eq[Int] {}

  implicit class AnyOps[T](x: T) {
    def === (y: T)(implicit c: Eq[T]) = x == y
  }

  def main(args: Array[String]): Unit = {
    assertCompile("5 === 5")
    assertNotCompile("5.6 === 7.7")

    val x: Int = 5
    assertCompile("x + 3")
    assertNotCompile("y + 3")
    import scala.util.Left
    assertCompile("Left(3)")
    assertNotCompile("Rigth(3)")

    def f(x: Int): Int = x * x
    assertCompile("f(3)")
    assertNotCompile("g(3)")

    type T
    assertCompile("def foo(x: T): T = x")
    assertNotCompile("foo(???)")
    assertNotCompile("def foo(x: S): S = x")

    assertNotCompile("def test(x: Int) =")

    assertCompile(
      """
      class EqString extends Eq[String]
      new EqString
      """
    )
  }
}
