import scala.quoted.*


class Index[K, Keys](val index: String) extends AnyVal {
  override def toString: String = index
}
object Index {

  implicit def zero[K, T]: Index[K, (K, T)] = new Index("0")

  implicit inline def succ[K, H, T](implicit inline prev: Index[K, T]): Index[K, (H, T)] = ${ succImpl[K, H, T]('prev) }

  def succImpl[K: Type, H: Type, T: Type](prev: Expr[Index[K, T]])(using Quotes): Expr[Index[K, (H, T)]] = {
    val value = s"1 + {${prev.show}}"
    '{new Index(${Expr(value)})}
  }
}
