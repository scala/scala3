extension [T1](x: T1)(using Numeric[T1])
  def combine[T2](y: T2)(using Numeric[T2]) = ???
  def combine(y: String) = ???

val res = 100.combine(200)
