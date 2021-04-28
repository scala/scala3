case class Foo[F[_]](f: {def f(x: F[Int]): Object})
case class Bar[F[_], G[_]](f: [B] => F[B] => G[B])
