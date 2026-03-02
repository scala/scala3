import language.future

implicit class C(x: String): // error
  def l: Int = x.length

class Ord[T]

object Test:
  implicit def convert(x: String): Int = x.length // error

  implicit val ob: Ord[Boolean] = Ord[Boolean]() // error
  lazy implicit val oi: Ord[Int] = Ord[Int]() // error

  implicit def of: Ord[Float] = Ord[Float]() // error

  implicit def ol[T](implicit x: Ord[T]): Ord[List[T]] = new Ord[List[T]]() // error // error

