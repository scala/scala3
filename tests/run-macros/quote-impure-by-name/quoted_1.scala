import scala.quoted._
import scala.quoted.autolift._

import scala.tasty.Reflection

class Index[K, Keys](val index: String) extends AnyVal {
  override def toString: String = index
}
object Index {

  implicit def zero[K, T]: Index[K, (K, T)] = new Index("0")

  implicit inline def succ[K, H, T](implicit prev: => Index[K, T]): Index[K, (H, T)] = ${ succImpl[K, H, T]('prev) }

  def succImpl[K, H, T](prev: Expr[Index[K, T]])(implicit k: Type[K], h: Type[H], t: Type[T], relection: Reflection): Expr[Index[K, (H, T)]] = {
    import relection._
    val value = s"1 + {${prev.show}}"
    '{new Index(${value})}
  }
}
