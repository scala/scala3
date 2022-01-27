enum Free[+F[_], A]:
  case Return(a: A)
  case Suspend(s: F[A])
  case FlatMap[F[_], A, B](
    s: Free[F, A],
    f: A => Free[F, B]) extends Free[F, B]

  def flatMap[F2[x] >: F[x], B](f: A => Free[F2,B]): Free[F2,B] =
    FlatMap(this, f)

  @annotation.tailrec
  final def step: Free[F, A] = this match
    case FlatMap(FlatMap(fx, f), g) => fx.flatMap(x => f(x).flatMap(y => g(y))).step
    case FlatMap(Return(x), f) => f(x).step
    case _ => this
