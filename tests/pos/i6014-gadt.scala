import scala.compiletime.*

object Test1 {
  type Foo[F[_]]
  type Bar[T] = T match {
    case Foo[f] => f[Int]
  }

  val li: Bar[Foo[List]] = List(1, 2, 3)
}

object Test2 {
  inline def summon[T] = summonFrom {
    case t: T => t
  }

  class Foo[F[_]]

  inline def bar[T] = inline erasedValue[T] match {
    case _: Foo[f] => summon[f[Int]]
  }

  implicit val li: List[Int] = List(1, 2, 3)
  val lii = bar[Foo[List]]
}

object Test3 {
  inline def summon[T] = summonFrom {
    case t: T => t
  }

  type K1Top = [t] =>> Any

  class Foo[F[X] <: K1Top[X]]

  inline def bar[T] = inline erasedValue[T] match {
    case _: Foo[f] => summon[f[Int]]
  }

  implicit val li: List[Int] = List(1, 2, 3)
  val lii = bar[Foo[List]]
}

object Test4 {
  inline def summon[T] = summonFrom {
    case t: T => t
  }

  class Foo[F[t] >: List[t]]

  inline def bar[T] = inline erasedValue[T] match {
    case _: Foo[f] => summon[f[Int]]
  }

  implicit val li: List[Int] = List(1, 2, 3)
  val lii = bar[Foo[List]]
}
