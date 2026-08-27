//> using options -Xmax-fuel:50
// set explicitlly low to reproduce error in simplified snippet 

package telegramium.bots

trait Decoder[A]
object Decoder:
  given Decoder[Int] = new Decoder[Int] {}

object CirceImplicits:
  def get[A](field: String)(using Decoder[A]): Option[A] = None

  val messageDecoder: Option[Int] =
    for
      f0 <- get[Int]("f0")
      f1 <- get[Int]("f1")
      f2 <- get[Int]("f2")
      f3 <- get[Int]("f3")
      f4 <- get[Int]("f4")
      f5 <- get[Int]("f5")
      f6 <- get[Int]("f6")
      f7 <- get[Int]("f7")
      f8 <- get[Int]("f8")
      f9 <- get[Int]("f9")
      f10 <- get[Int]("f10") // error
      f11 <- get[Int]("f11")
      f12 <- get[Int]("f12")
      f13 <- get[Int]("f13")
      f14 <- get[Int]("f14")
      f15 <- get[Int]("f15")
      f16 <- get[Int]("f16")
      f17 <- get[Int]("f17")
      f18 <- get[Int]("f18")
      f19 <- get[Int]("f19")
    yield f0
