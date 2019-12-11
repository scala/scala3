package outer.nested


case class B(override val x: Int) extends A(x)

case class C(s: String) extends A(s.length)
