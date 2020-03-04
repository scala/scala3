
trait A {
  def p: Int
  def getP = p
}
trait B extends A {
  def p: Int = 22
}
class C extends B {
  private def p: Int = 23  // error
}
@main def Test =
  C().getP  // would crash with a duplicate method error if the private C#p was permitted
