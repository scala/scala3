//> using options -preview

class myOptionModule(doOnMap: => Unit):
  sealed trait MyOption[+A]:
    inline def map[B](f: A => B): MyOption[B] =
      this match
      case MySome(x) =>
        doOnMap
        MySome(f(x))
      case MyNone => MyNone
    def flatMap[B](f: A => MyOption[B]): MyOption[B] =
      this match
      case MySome(x) => f(x)
      case MyNone => MyNone
  case class MySome[A](x: A) extends MyOption[A]
  case object MyNone extends MyOption[Nothing]
  object MyOption:
    def apply[A](x: A): MyOption[A] = MySome(x)

@main def Test =

  val myOption = myOptionModule(???)

  import myOption.*

  def portablePrintMyOption(opt: MyOption[Any]): Unit = println:
    opt match
    case MySome(()) => "MySome(())"
    case opt => opt

  val z = for {
    a <- MyOption(1)
    b <- MyOption(())
  } yield ()

  portablePrintMyOption(z)

  val z2 = for {
    a <- MyOption(1)
    b <- MyOption(2)
  } yield b

  portablePrintMyOption(z2)

  val z3 = for {
    a <- MyOption(1)
    (b, c) <- MyOption((2, 3))
  } yield (b, c)

  portablePrintMyOption(z3)

  val z4 = for {
    a <- MyOption(1)
    (b, (c, d)) <- MyOption((2, (3, 4)))
  } yield (b, (c, d))

  portablePrintMyOption(z4)
end Test
