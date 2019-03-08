object App {
  def main(args: Array[String]): Unit = {
    trait FooT {
      type T
      def subst[F[_]](fa: F[T]): F[Int]
    }
    val Foo: FooT = new FooT {
      type T = Int

      def subst[F[_]](fa: F[T]): F[Int] = fa
    }
    type Foo = Foo.T
    type Bar = Foo

    sealed abstract class K[-A]
    case object K1 extends K[Int]
    case object K2 extends K[Foo]
    case object K3 extends K[Bar]

    val foo: K[Int] = Foo.subst[K](K2)
    def get(k: K[Int]): Unit = k match {
      case K1 => ()
      // case K2 => ()
      // case K3 => ()
    }

    get(foo)
  }
}