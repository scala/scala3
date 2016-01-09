package namedparams

abstract class C[type Elem, type Value](val elem: Elem) {
  def toVal: Elem = ???
}



object Test {
  val c = new C[String, String]("A") {
    override def toVal = elem
  }
  val x: c.Elem = c.elem

  val c2: C { type Elem = String } = c
  val x2: c2.Elem = c2.elem
}
