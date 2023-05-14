class Inv[X]
class Ref[X]
object Ref {
  def apply(i: Inv[Int], x: Int): Ref[Int] = ???
  def apply[Y](i: Inv[Y], x: Y): Ref[Y] = ???
}

class A {
  val ref: Ref[List[AnyRef]] = Ref(new Inv[List[AnyRef]], List.empty)
}
