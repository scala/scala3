object G {
  final class X
  final class Y

  trait Test {
    type Type
    val i: Bar[Y & Type] = 1 // error
  }

  type Bar[A] = A match {
    case X & Y => String
    case Y => Int
  }
}
