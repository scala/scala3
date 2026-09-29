case class A(x: Int)

@main def Test(): Unit = {
  val derivedUnwrapped = UnWrapper.derived[A]
  val aUnwrapped: Int = derivedUnwrapped.unwrap(A(42))
  assert(42 == aUnwrapped)
}
