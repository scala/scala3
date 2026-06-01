case class Box[A](value: A) {
  def map[B](f: A => B): Box[B] = Box(f(value))
  def get: A = value
}

class B{
    def first[A](list: List[A]): Option[A] = list.headOption

    val intBox    = Box(42)
    val strBox    = Box("hello")
    val doubleBox = intBox.map(_ * 2.5)
}
