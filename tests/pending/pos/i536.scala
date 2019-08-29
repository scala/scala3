trait Comp[T]
trait Coll[T]
class C extends Comp[C]
object Max {
  def max[M <: Comp[_ >: M]](x: Coll[_ <: M]): M = ???
  def max[M](x: Coll[_ <: M], cmp: Object): M = ???
  val xs: Coll[C] = ???
  val m1 = max(xs)
  val m2 = max(null)

  java.util.Collections.max(null)
}
