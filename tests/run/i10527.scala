case class C(x: Int)
class CC(x: Int) extends C(x) { override def canEqual(o: Any) = o.isInstanceOf[CC] }
final case class D(x: Int)
final case class E(x: Int) { override def canEqual(o: Any) = false }
@main def Test =
  assert(C(1) != new CC(1))
  assert(D(1) == D(1))
  assert(E(1) != E(1))
