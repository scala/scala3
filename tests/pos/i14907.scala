object Module {
  class Fun[N <: Int]()
  type Fill[N <: Int] = N match {
    case 0 => EmptyTuple
    case 1 => Any *: Fill[0]
  }
  extension[N <: Int] (f: Fun[N])
    def apply: Fill[N] => Any = ???

  Fun[1]()(???)
}
