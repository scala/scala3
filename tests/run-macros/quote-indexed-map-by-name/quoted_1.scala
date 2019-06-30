import scala.quoted._

class Index[K, Keys](val index: Int) extends AnyVal
object Index {

  implicit def zero[K, T]: Index[K, (K, T)] = new Index(0)

  implicit inline def succ[K, H, T](implicit prev: => Index[K, T]): Index[K, (H, T)] = ${succImpl('[K], '[H], '[T])}

  def succImpl[K, H, T](k: Type[K], h: Type[H], t: Type[T]) given QuoteContext: Expr[Index[K, (H, T)]] = {
    implicit val kk: Type[K] = k
    implicit val hh: Type[H] = h
    implicit val tt: Type[T] = t
    '{new Index(0)}
  }
}
