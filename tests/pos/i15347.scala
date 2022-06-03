object Test:
  enum E[T]:
    case A(i: Int)

  export E.*

  def f(x: E[Int]) = x match
    case A(i) => i
