case class Test()
object Test:
  def unapplySeq(t: Test): Some[Seq[Int]] = Some(Seq(1, 2))

trait X:
  def unapply(t: Test): Boolean = true
object X extends X:
  def unapplySeq(t: Test): Some[Seq[Int]] = Some(Seq(1, 2))

case class K(k: Int)
case class J(k: Int, ks: Int*)

case class M(m: Int)
object M:
  def unapply() = true
  def unapplySeq(m: M) = Some(Seq(42, 27))

object ProduktSeq:
  class Foo(val name: String, val children: Int*)
  object Foo:
    def unapply(f: Foo): Option[(String, Seq[Int])] = Some(f.name -> f.children)

  def foo(f: Foo) = f match
    case Foo(name, x, y, ns*) => ">= two children." // error
    case Foo(name, ns*)       => "< two children." // error
end ProduktSeq


@main def test =
  Test() match
    case Test(x*) => println(x) // error // error
  Test() match
    case Test(_*) => () // error
  Test() match
    case X(x*) => // error
  K(42) match
    case K(k*) => // error
  J(42, 27) match
    case J(i, j, js*) => // no error
  M(5) match
    case M(m*) => // error
