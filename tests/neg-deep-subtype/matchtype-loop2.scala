object Test {
  type L[X] = X match {
    case Int => L[X]
  }
  type LL[X] = X match {
    case Int => LL[LL[X]]
  }
  val x: LL[Int] = 2   // error
}
