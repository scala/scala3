case class Baz[F[_], G[_]](f: [B] => F[B] => G[B])
