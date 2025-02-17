import scala.language.experimental.betterFors

class myOptionModule(doOnMap: => Unit) {
  sealed trait MyOption[+A] {
    def map[B](f: A => B): MyOption[B] = this match {
      case MySome(x) => {
        doOnMap
        MySome(f(x))
      }
      case MyNone => MyNone
    }
    def flatMap[B](f: A => MyOption[B]): MyOption[B] = this match {
      case MySome(x) => f(x)
      case MyNone => MyNone
    }
  }
  case class MySome[A](x: A) extends MyOption[A]
  case object MyNone extends MyOption[Nothing]
  object MyOption {
    def apply[A](x: A): MyOption[A] = MySome(x)
  }
}

object Test extends App {

  val myOption = new myOptionModule(println("map called"))

  import myOption.*

  val z = for {
    a <- MyOption(1)
    b <- MyOption(())
  } yield ()

  println(z)

  val z2 = for {
    a <- MyOption(1)
    b <- MyOption(2)
  } yield b

  println(z2)

  val z3 = for {
    a <- MyOption(1)
    (b, c) <- MyOption((2, 3))
  } yield (b, c)

  println(z3)

  val z4 = for {
    a <- MyOption(1)
    (b, (c, d)) <- MyOption((2, (3, 4)))
  } yield (b, (c, d))

  println(z4)

}
