import scala.language.experimental.betterFors

class myOptionPackage(doOnMap: => Unit) {
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
}

object Test extends App {

  val myOption = new myOptionPackage(println("map called"))

  import myOption.*

  val z = for {
    a <- MySome(1)
    b <- MySome(())
  } yield ()

  println(z)

}
