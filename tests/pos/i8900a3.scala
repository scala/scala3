class ContraCo[-T, +S](val t: S)
class CoContra[+T, -S](val t: T)
object Test {
  type Id[T] = T
  def unwrap[Outer](inv: CoContra[Outer, Outer]): Outer = inv.t
  def wrap[Inner](i: Inner): CoContra[Id[Inner], Id[Inner]] = new CoContra(i)

  val a = unwrap({
    class Local
    val local = new Local
    wrap(local)
  })
}
