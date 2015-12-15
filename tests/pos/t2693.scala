class A {
  trait Tr[A]
  def usetHk[T[_], A](ta: T[A]) = 0
  usetHk(new Tr[Int]{}: Tr[Int])
  usetHk(new Tr[Int]{}) // fails with: found: java.lang.Object with T[Int], required: ?T[ ?A ]
}
