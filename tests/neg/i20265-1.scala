trait Expr[T]

trait Ops[T](using m: scala.deriving.Mirror.ProductOf[T]) {
  def apply(args: Tuple.Map[m.MirroredElemTypes, Expr]): Expr[T] = ??? // error
}

case class P(a: Int)
object P extends Ops[P]

