import scala.quoted._

import dotty.tools.dotc.quoted.Toolbox._

class Index[K, Keys](val index: String) extends AnyVal {
  override def toString: String = index
}
object Index {

  implicit def zero[K, T]: Index[K, (K, T)] = new Index("0")

  implicit transparent def succ[K, H, T](implicit prev: => Index[K, T]): Index[K, (H, T)] = ~succImpl('(prev))('[K], '[H], '[T])

  def succImpl[K, H, T](prev: Expr[Index[K, T]])(implicit k: Type[K], h: Type[H], t: Type[T]): Expr[Index[K, (H, T)]] = {
    val value = s"1 + {${prev.show}}"
    '(new Index(~value.toExpr))
  }
}