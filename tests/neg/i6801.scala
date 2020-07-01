extension [T](x: T) {
  def + (y: T)(using n: Numeric[T]): T = n.plus(x,y)
}
def foo[T: Numeric](x: T) = 1f + x // error: no implicit argument of type Numeric[Any]
def bar[T: Numeric](x: T) = x + 1f // error: no implicit argument of type Numeric[Any]