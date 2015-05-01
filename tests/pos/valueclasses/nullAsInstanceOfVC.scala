package nullAsInstanceOfVC

// These issues were originally reported in SI-5866 and SI-8097
// FIXME: Make this a run test once we have run tests.

object VCNull {
  case class Foo(d: Double) extends AnyVal {
    override def toString = s"Foo($d)"
  }
  case class Bar(s: String) extends AnyVal {
    override def toString = s"Bar($s)"
  }

  def testDirect(): Unit = {
    val fooDirect: Foo = null.asInstanceOf[Foo]
    val barDirect: Bar = null.asInstanceOf[Bar]
  }

  def testIndirect(): Unit = {
    val fooIndirect: Foo = { val n: Any = null; n.asInstanceOf[Foo] }
    val barIndirect: Bar = { val n: Any = null; n.asInstanceOf[Bar] }
  }

  def nullOf[T]: T = null.asInstanceOf[T]
  def testGeneric(): Unit = {
    val fooGeneric: Foo = nullOf[Foo]
    val barGeneric: Bar = nullOf[Bar]
  }
}
