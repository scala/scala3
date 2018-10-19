object Test {
  trait Option[+T]
  case object None extends Option[Nothing]
  case class Some[+T](x: T) extends Option[T]

  inline def openImpl(): Int =
    Some(42) match { case Some(i) => i }

  def open() = openImpl()

  inline def openImpl1(): Int =
    new Some(42) match { case Some(i) => i }

  def open1() = openImpl1()

  inline def openImpl2(): Int =
    None match { case None => 42 }

  def open2(): Int = openImpl2()
}

// Same as Test, with Scala2 case classes
object Test2 {
  inline def openImpl(): Int =
    Some(42) match { case Some(i) => i }

  def open() = openImpl()

  inline def openImpl1(): Int =
    new Some(42) match { case Some(i) => i }

  def open1() = openImpl1()

  inline def openImpl2(): Int =
     None match { case None => 42 }

  def open2(): Int = openImpl2()

}
