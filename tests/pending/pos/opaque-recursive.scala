object opaquetypes {

  opaque type Fix[F[_]] = F[Fix2[F]]

  opaque type Fix2[F[_]] = Fix[F]

  object Fix {
    def unfold[F[_]](x: Fix[F]): F[Fix]
  }

  object Fix2 {
    def unfold[F[_]](x: Fix2[F]: Fix[F] = x
    def fold[F[_]](x: Fix[F]: Fix2[F] = x
  }

}
