object Foo extends (Int => Int) { // OK
  def apply(x: Int) = x
}

enum E(x: Int) { // used to generate Int => new E(x) as the parent of object E --> crash
  case C(x: Int) extends E(x)
}
