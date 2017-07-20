object Hello {
  def main(args: Array[String]): Unit = {
    sealed trait Wat[T]

    implicit def intWat: Wat[Int] = ???
    implicit def listWat[T](implicit tWat: Wat[T]): Wat[List[T]] = new Wat{}

    def stuff[T](implicit implicitWat: => Wat[List[T]]): Unit = ???
  }
}
