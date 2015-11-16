object Test {
  import scala.collection._

  trait T {
    def f() : Unit
  }

  def view = new T {
    def f() = ()
  }

  trait TLike[+A, RR] { self =>

    def repr: RR = ???

    def view2 = new TraversableView[A, RR] {
      protected lazy val underlying = self.repr
      override def foreach[U](f: A => U): Unit = ???
    }
  }
}
