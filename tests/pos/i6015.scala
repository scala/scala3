import scala.compiletime._

object Test {
  implicit val i: Int = 23

  inline def foo() = {
    Array[Int](implicitly[Int])
  }

  foo()
}
