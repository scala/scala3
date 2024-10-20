sealed abstract class Gen[+T1]
given [T2]: Conversion[T2, Gen[T2]] = ???

trait Show[T3]
given Show[Boolean] = ???
given [A1: Show, B1: Show, C1: Show]: Show[(A1, B1, C1)] = ???

object ForAll:
  def apply[A2: Show, B2](f: A2 => B2): Unit = ???
  def apply[A3: Show, B3: Show, C3](f: (A3, B3) => C3): Unit = ???
  def apply[A4: Show, B4](gen: Gen[A4])(f: A4 => B4): Unit = ???

@main def Test =
  ForAll: (b1: Boolean, b2: Boolean, b3: Boolean) =>
    ???
