package namedparams

class C[type Elem, type Value](val elem: Elem) {
  def toVal: Elem = ???
}

abstract class D[type Elem, V](elem: Elem) extends C[Elem, V](elem)
abstract class D2[Elem, V](elem: Elem) extends C[Elem, V](elem) // error
abstract class D3[type Elem, V](x: V) extends C[V, V](x) // error
abstract class D4[type Elem](elem: Elem) extends C[Elem, Elem] // error
object Test {
  val c = new C[String, String]("A") {
    override def toVal = elem
  }
  val x: c.Elem = c.elem

  val c2: C { type Elem = String } = c

  val c3 = new C[Elem = String, Value = Int]("B")
  val c4 = new C[Elem = String]("C")
  val x2: c2.Elem = c2.elem

  val c5 = new C[Elem1 = String, Value0 = Int]("B") // error // error

  def d2[E, V](x: E) = new C[Elem = E, Value = V](x)

  val dup = d2[E = Int, V = String, E = Boolean](2) // error
  val z1 = d2[Elem = Int, Value = String](1) // error // error
  val z2 = d2[Value = String, Elem = Int](1) // error // error
  val z3 = d2[Elem = Int](1) // error
  val z4 = d2[Value = Int]("AAA") // error
  val z5 = d2[Elem = Int][Value = String](1) //error // error

}



