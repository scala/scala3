
trait t11900 {
  // cf pos/trailing-commas
  //
  import scala.collection.{
    immutable,
    mutable,
  }

  def h[A,
  ]: List[A] = Nil

  def u(
      x: Int,
      y: Int,
  )(using List[Int],
      Set[Int],
  )(using l: List[Int],
    s : Set[Int],
  ): Int = 1

  def g = List(
    1,
    2,
    3,
  )

  def star =
    List(1, 2, 3, 4, 5) match {
      case List(
      1,
      2,
      3,
      ) => false
      case List(
      1,
      2,
      _*,
      ) => true
    }

  def f =
    List(1, 2, 3).map {
      a => a + 1,   // error: weird comma
    }

  class A() {
    println("a"),   // error: weird comma
  }

  def b() = {
    println("b"),   // error: weird comma
  }

  def starcrossed =
    List(1, 2, 3, 4, 5) match {
      case List(
      1,
      2,
      3,
      ) => false
      case List(
      1,
      _*, // error
      2,
      ) => true
    }

  def p(p: (Int,
      String,
      )
  ): Unit

  def q: (Int,
      String,
      )

  val z = 42
}