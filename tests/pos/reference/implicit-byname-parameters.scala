object Test {

trait Codec[T] {
  def write(x: T): Unit
}

implicit def intCodec: Codec[Int] = ???

implicit def optionCodec[T]
    (implicit ev: => Codec[T]): Codec[Option[T]] =
  new {
    def write(xo: Option[T]) = xo match {
      case Some(x) => ev.write(x)
      case None =>
    }
  }

val s = implicitly[Codec[Option[Int]]]

s.write(Some(33))
s.write(None)
}
