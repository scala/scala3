package a

object A {

  trait Box0[A]

  trait BoxInt extends Box0[Int] {
    def append(a: Int): Unit = ()
  }

  val box: BoxInt = new BoxInt {}

}
