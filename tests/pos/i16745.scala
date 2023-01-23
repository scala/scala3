class Test[F[_]]:
  sealed trait Foo[A, B]

  // note that Foo's B param maps directly to U (i.e. no GADT involved on this type param)
  case class Bar[T, U]() extends Foo[F[T], U]:
    def get: Option[U] = None

  def go[T1, U1](that: Foo[F[T1], U1]): Option[U1] =
    that match
      case b: Bar[t, u] =>
        b.get
