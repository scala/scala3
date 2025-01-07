sealed trait Foo[A]
final class Bar extends Foo[Nothing]

object Test:
  type Extract[T] = T match
    case Foo[_] => Int

  val x: Extract[Bar] = 1
