object Test {

trait Serializable[T] {
  def write(x: T): Unit
}

implicit def serializeInt: Serializable[Int] = ???

implicit def serializeOption[T](implicit ev: => Serializable[T]): Serializable[Option[T]] =
  new Serializable[Option[T]] {
    def write(xo: Option[T]) = xo match {
      case Some(x) => ev.write(x)
      case None =>
    }
  }

val s = implicitly[Serializable[Option[Int]]]

s.write(Some(33))
s.write(None)

}
