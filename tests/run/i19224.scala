// scalajs: --skip

object Test extends App {
  val field = 1
  def x(): Int => String = (i: Int) => i.toString
  def y(): () => String = () => field.toString

  locally {
    assert(x() == x()) // true on Scala 2, was false on Scala 3...
    assert(y() == y()) // also true if `y` accesses object-local fields

    def z(): Int => String = (i: Int) => i.toString
    assert(z() != z()) // lambdas in constructor are not lifted to static, so no memoization (Scala 2 lifts them, though).
  }

  val t1 = new C
  val t2 = new C

  locally {
    assert(t1.x() == t2.x()) // true on Scala 2, was false on Scala 3...
  }
}
