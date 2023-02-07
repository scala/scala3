object Test {
  sealed trait Box[T] { def value: T }
  final case class IntBox(value: Int) extends Box[Int]

  implicit def s1[T](implicit box: Box[T]): String = "generic: " + box.value
  implicit def s2(implicit box: Box[Int]): String = "specific: " + box.value

  def test[T](implicit box: Box[T]): String = box match {
    case IntBox(_) => implicitly[String]
  }

  def main(args: Array[String]): Unit =
    println(test(IntBox(1)))
}
