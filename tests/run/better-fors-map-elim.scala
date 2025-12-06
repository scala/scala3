enum MyOption[+A]:
  case MySome(x: A)
  case MyNone

  def map[B](f: A => B): MyOption[B] =
    this match
    case MySome(x) => ???  //MySome(f(x))
    case MyNone => ??? //MyNone
  def flatMap[B](f: A => MyOption[B]): MyOption[B] =
    this match
    case MySome(x) => f(x)
    case MyNone => MyNone
object MyOption:
  def apply[A](x: A): MyOption[A] = MySome(x)

@main def Test =

  val _ =
    for
      a <- MyOption(1)
      b <- MyOption(())
    yield ()

  val _ =
    for
      a <- MyOption(1)
      b <- MyOption(2)
    yield b

  val _ =
    for
      a <- MyOption(1)
      (b, c) <- MyOption((2, 3))
    yield (b, c)

  val _ =
    for
      a <- MyOption(1)
      (b, (c, d)) <- MyOption((2, (3, 4)))
    yield (b, (c, d))

  extension (i: Int) def map[A](f: Int => A): A = ???

  val _ = for j <- 42 yield j

  val _ = for x = 42; x <- MyOption(x) yield x
