object A {
  def unapply(n: Int): Option[Int] = Some(n)

  def run = (0: Short) match {
    case A(_) => // error: this case is unreachable since class Short is not a subclass of class Int
    case _    =>
  }

  def run2 = (0: Short) match {
    case x: Int => // error: this case is unreachable since class Short is not a subclass of class Int
    case _    =>
  }
}
