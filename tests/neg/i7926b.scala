trait A {
  def p: Int
  def getP = p
}
class B extends A {
  def p: Int = 22
}
class C extends B {
  private def p: Int = super.p  // error
}
class D extends B {
  private val p: Int = super.p  // OK
}
class E extends B {
  private var p: Int = 0  // error
}
class F extends B {
  private lazy val p: Int = 0  // error
}
object Test {
  def main(args: Array[String]): Unit =
    println(new C().getP) // would give illegal access error at runtime if C compiled
    println(new D().getP) // OK
    println(new E().getP) // would give illegal access error at runtime if E compiled
    println(new F().getP) // would give illegal access error at runtime if F compiled
}