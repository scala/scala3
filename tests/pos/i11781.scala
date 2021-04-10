object Test:
  class C1[A](a: A)
  type D1[A] = C1[A]
  new D1(1)

  class C2[B, A](a: A)
  type D2[A] = C2[Int, A]
  new D2(1)
  new D2[Int](1)

  class E(x: Int) extends D2[Int](x)
  class F(x: Int) extends D2(x)

object Ah {
  final case class Values[B, +A](a: A)

  trait Object[B] {
    final type Values[+A] = Ah.Values[B, A]
    object Values {
      def blah: Values[Unit] =
        new Values(())
    }
  }
}