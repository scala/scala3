import scala.compiletime._

object Test1 {
  type Foo[F[_]]
  type Bar[T] = T match {
    case Foo[f] => f[Int]
  }

  val li: Bar[Foo[List]] = List(1, 2, 3)
}

object Test2 {
  inline def summon[T] = implicit match {
    case t: T => t
  }

  class Foo[F[_]]

  inline def bar[T] = inline erasedValue[T] match {
    case _: Foo[f] => summon[f[Int]]
  }

  implicit val li: List[Int] = List(1, 2, 3)
  bar[Foo[List]]
}
