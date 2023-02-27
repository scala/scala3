class Fn:
  class R[Y]

case class Foo[F[_]](nest: Foo[F]):
  case class Bar[G[_], R[_]](value: Foo[G])

  def bar[G[_]](using fn: Fn): Bar[G, fn.R] = ???

  def part[G[_]](using fn: Fn): Bar[G, fn.R] =
    (bar[G], ()) match
      case (Bar(value), ()) =>
        Bar(Foo(value))
