package a

object A {

  trait Box0[A] {
    def append(a: A): this.type = this
  }

  trait BoxInt extends Box0[Int] {
    override def append(a: Int): this.type = this
  }

  val box: BoxInt = new BoxInt {}

}
