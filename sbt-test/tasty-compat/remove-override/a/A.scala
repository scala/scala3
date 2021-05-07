package a

object A {

  trait Box0[A] {
    def append(a: A): Unit = ()
  }

  trait BoxInt extends Box0[Int] {
    override def append(a: Int): Unit = ()
  }

  val box: BoxInt = new BoxInt {}

}
