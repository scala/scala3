package namedparams

class C[type Elem, type Value](val elem: Elem) {
  def toVal: Elem = ???
}

class D[type Elem, V](elem: Elem) extends C[Elem, V](elem)

object Test {
  val c = new C[String, String]("A") {
    override def toVal = elem
  }
  val x: c.Elem = c.elem

  val c2: C { type Elem = String } = c

  val c3 = new C[Elem = String, Value = Int]("B")
  val c4 = new C[Elem = String]("C")
  val x2: c2.Elem = c2.elem
}
