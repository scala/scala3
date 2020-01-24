object Test {
  type L[X] <: Any = X match {
    case Int => L[X]
  }
  type LL[X] <: Any = X match {   // error: recursion limit exceeded
    case Int => LL[LL[X]]
  }
}
