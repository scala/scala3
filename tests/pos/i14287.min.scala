enum Foo[+H[_]]:
  case Bar[F[_]](f: Foo[F]) extends Foo[F]

  def test: Foo[H] = this match
    case Bar(Bar(f)) => Bar(f)
    case _           => this
