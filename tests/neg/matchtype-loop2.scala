object Test {
  type L[X] = X match {
    case Int => L[X]
  }
  type LL[X] = X match {   // error: recursion limit exceeded
    case Int => LL[LL[X]]
  }
}
