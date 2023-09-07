trait Companion:
  final override def toString: String = "Companion"

case class Example(value: Int)
object Example extends Companion

case class C()
object C:
  override def toString = "CC"

case class D()

@main def Test =
  assert(Example.toString == "Companion")
  assert(C.toString == "CC")
  assert(D.toString == "D")
