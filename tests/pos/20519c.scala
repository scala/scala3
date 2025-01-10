object Main {
  trait TCl[F[_]]

  implicit class Stx[F[_], A](e: F[Option[A]]) {
    def boo(implicit ev: TCl[F]): Unit = ()
  }

  type Result[F[_], A] = F[Option[A]]

  implicit val t: TCl[Option] = new TCl[Option] {}

  def main(args: Array[String]): Unit = {
    val b: Result[Option, Int] = None
    b.boo

    // works without the alias:
    val b2: Option[Option[Int]] = None
    b2.boo
  }
}
