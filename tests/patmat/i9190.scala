class Test {
  case object X; type X = X.type
  case object Y; type Y = Y.type

  type XorY = X | Y

  val testee1: XorY = X
  testee1 match {
    case value: XorY => println(value)
  }

  val testee2: Tuple1[XorY] = Tuple1(X)
  testee2 match {
    case Tuple1(value: XorY) => println(value)
  }

  type IntOrString = Int | String

  val testee3: Tuple1[IntOrString] = Tuple1(42)
  testee3 match {
    case Tuple1(value: IntOrString) => println(value)
  }
}
