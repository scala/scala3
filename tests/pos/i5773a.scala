trait Semigroup[T] {
  def (x: T) combine (y: T): T
}
object Test {
  implicit val IntSemigroup: Semigroup[Int] = new {
    def (x: Int) combine (y: Int): Int = x + y
  }
  implicit def OptionSemigroup[T: Semigroup]: Semigroup[Option[T]] = new {
    def (x: Option[T]) combine (y: Option[T]): Option[T] = for {
      x0 <- x
      y0 <- y
    } yield x0.combine(y0)
  }
  1.combine(2)
  Some(1).combine(Some(2))
  Option(1) combine Option(2)
}
