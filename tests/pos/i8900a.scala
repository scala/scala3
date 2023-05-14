class Inv[T](val elem: T)
object Test {
  def unwrap[Outer](inv: Inv[Outer]): Outer = inv.elem
  def wrap[Inner](i: Inner): Inv[Inner] = new Inv(i)

  val a = unwrap({
    class Local
    val local = new Local
    wrap(local)
  })
}
