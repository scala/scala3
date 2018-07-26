trait T[A: Numeric]
class TX[A: Numeric] extends T[A]
class TY[A: Numeric] extends T[A]()

trait S[A: Numeric](x: Int)
class SX[A: Numeric] extends S[A](0)
