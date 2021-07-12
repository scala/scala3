object Test extends App {
  sealed trait P[T]
  case class C1[T <: String](c1: T) extends P[T]
  case class C2[T](c2: T) extends P[T]

  def test[T](p: P[T], t: T): Unit = p match {
    case C1(_) =>
      // T <: String
      val t : T = ???
      val s : String = t
      def test = new C1[T](t)
      println(1)

    case C2(_) => println(2)
  }
}
