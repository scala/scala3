case class A()
case class B()
object Test {
  type T[X] = X match {
    case A => Int
    case B => String
  }
  def f(x: Any): T[x.type] = x match {
    case A() => 1
    case B() => ""
  }
}