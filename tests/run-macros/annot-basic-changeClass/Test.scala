@change
class A:
  override def toString = "It is A"

@main def Test =
  val a = new A
  assert(a.toString == "It is A changed by macro annotation")
