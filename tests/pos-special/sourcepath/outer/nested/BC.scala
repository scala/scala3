package outer.nested


case class B(x: Int) extends A(x)

case class C(s: String) extends A(s.length)
