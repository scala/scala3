object Test {
  final class A
  final class B
  final class C

  trait F1[T1] {
    def apply(x: T1): Unit
  }

  type F[N] = N match {
    case A => F1[String]
    case B => F[A]
    case C => F[B]
  }

  val s1: F[A] = ???
  s1.apply("A")

  val s2: F[B] = ???
  s2.apply("B")

  val s3: F[C] = ???
  s3.apply("C")
}
