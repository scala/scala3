//> using options -Xfatal-warnings

import scala.reflect.TypeTest

object Test {
  def test[S, T](using TypeTest[S, T]): Unit = ()
  val a: A = ???

  test[Any, Any]
  test[Int, Int]

  test[Int, Any]
  test[String, Any]
  test[String, AnyRef]

  test[Any, Int]
  test[Any, String]
  test[Any, Some[?]]
  test[Any, Array[Int]]
  test[Seq[Int], List[Int]]

  test[Any, Some[Int]] // error
  test[Any, a.X] // error
  test[a.X, a.Y] // error

}

class A {
  type X
  type Y <: X
}
