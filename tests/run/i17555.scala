class Root {
  override def toString() = "Root"
}
trait A extends Root with B {  }
trait B {
   override def toString() = "B"
}
case class C() extends A {
  override def toString() = super.toString()
}
class D() extends A, Serializable {
  override def toString() = super.toString()
}

@main def Test =
  println(C())
  println(D())